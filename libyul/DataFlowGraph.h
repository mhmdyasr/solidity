/*
	This file is part of solidity.

	solidity is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	solidity is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with solidity.  If not, see <http://www.gnu.org/licenses/>.
*/
// SPDX-License-Identifier: GPL-3.0

#pragma once

#include <libyul/AST.h>
#include <libyul/AsmAnalysisInfo.h>
#include <libyul/backends/evm/EVMDialect.h>

#include <libyul/optimiser/ASTWalker.h>
#include <libyul/Scope.h>

#include <vector>

namespace solidity::yul
{

struct DFG
{
	explicit DFG() {}
	DFG(DFG const&) = delete;
	DFG(DFG&&) = delete;
	DFG& operator=(DFG const&) = delete;
	DFG& operator=(DFG&&) = delete;
	struct Variable;
	struct Literal;
	struct FunctionCall;
	struct BuiltinCall;
	using Expression = std::variant<Variable, Literal, FunctionCall, BuiltinCall>;
	struct Variable
	{
		std::shared_ptr<DebugData const> debugData;
		Scope::Variable const* variable;
	};
	struct Literal
	{
		std::shared_ptr<DebugData const> debugData;
		u256 value;
	};
	struct BuiltinCall
	{
		std::shared_ptr<DebugData const> debugData;
		BuiltinFunctionForEVM const* builtin = nullptr;
		yul::FunctionCall const* functionCall = nullptr;
		std::vector<Expression> arguments;
		size_t returns = 0;
	};
	struct FunctionCall
	{
		std::shared_ptr<DebugData const> debugData;
		Scope::Function const* function = nullptr;
		std::vector<Expression> arguments;
		size_t returns = 0;
	};
	struct Declaration
	{
		std::shared_ptr<DebugData const> debugData;
		std::vector<Variable> variables;
		std::optional<Expression> value;
	};
	struct Assignment
	{
		std::shared_ptr<DebugData const> debugData;
		std::vector<Variable> variables;
		Expression value;
	};
	struct ExpressionStatement
	{
		std::shared_ptr<DebugData const> debugData;
		Expression expression;
	};
	using Statement = std::variant<Declaration, Assignment, ExpressionStatement>;

	struct BasicBlock
	{
		std::vector<BasicBlock const*> entries;
		std::vector<Statement> statements;
		struct ConditionalJump
		{
			Expression condition;
			BasicBlock const* nonZero = nullptr;
			BasicBlock const* zero = nullptr;
		};
		struct Jump
		{
			BasicBlock const* target = nullptr;
		};
		struct FunctionReturn {};
		struct Stop {};
		struct Revert {};
		std::variant<std::monostate, Jump, ConditionalJump, FunctionReturn, Stop, Revert> exit;
	};
	std::list<BasicBlock> blocks;
	std::vector<Scope::Variable> ghostVariables;
	std::vector<yul::FunctionCall> ghostCalls;

	struct FunctionInfo
	{
		std::shared_ptr<DebugData const> debugData;
		BasicBlock* entry = nullptr;
		std::set<BasicBlock*> exits;
		std::vector<Variable> parameters;
		std::vector<Variable> returnVariables;
	};
	std::map<Scope::Function const*, FunctionInfo> functions;
	BasicBlock* entry = nullptr;
	std::set<BasicBlock*> exits;

	BasicBlock& makeBlock()
	{
		return blocks.emplace_back(BasicBlock{});
	}
};

class DataFlowGraphBuilder
{
public:
	static std::unique_ptr<DFG> build(AsmAnalysisInfo& _analysisInfo, EVMDialect const& _dialect, Block const& _block);

	DFG::Expression operator()(Literal const& _literal);
	DFG::Expression operator()(Identifier const& _identifier);
	DFG::Expression operator()(FunctionCall const&);

	void operator()(VariableDeclaration const& _varDecl);
	void operator()(Assignment const& _assignment);
	void operator()(ExpressionStatement const& _statement);

	void operator()(Block const& _block);

	void operator()(If const& _if);
	void operator()(Switch const& _switch);
	void operator()(ForLoop const&);
	void operator()(Break const&);
	void operator()(Continue const&);
	void operator()(Leave const&);
	void operator()(FunctionDefinition const&);

private:
	DataFlowGraphBuilder(
		DFG& _graph,
		AsmAnalysisInfo& _analysisInfo,
		EVMDialect const& _dialect
	);

	Scope::Variable const& lookupVariable(YulString _name) const;
	DFG::Expression makeExpression(Expression const& _expression);
	std::pair<DFG::BasicBlock*, DFG::BasicBlock*> makeConditionalJump(DFG::Expression _condition);
	void makeConditionalJump(DFG::Expression _condition, DFG::BasicBlock& _nonZero, DFG::BasicBlock& _zero);
	void jump(DFG::BasicBlock& _target);
	DFG& m_graph;
	AsmAnalysisInfo& m_info;
	EVMDialect const& m_dialect;
	DFG::BasicBlock* m_currentBlock = nullptr;
	Scope* m_scope = nullptr;
	std::set<DFG::BasicBlock*> m_exits;
	struct ForLoopInfo { DFG::BasicBlock* afterLoop = nullptr; DFG::BasicBlock* post = nullptr; };
	std::optional<ForLoopInfo> m_forLoopInfo;
	DFG::BasicBlock* m_currentFunctionExit = nullptr;
};

}
