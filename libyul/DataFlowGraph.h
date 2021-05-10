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

#include <deque>
#include <vector>

namespace solidity::yul
{

struct ReturnLabelSlot;
struct VariableSlot;
struct LiteralSlot;
struct TemporarySlot;
using StackSlot = std::variant<ReturnLabelSlot, VariableSlot, LiteralSlot, TemporarySlot>;
using Stack = std::vector<StackSlot>;


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
		std::optional<Stack> targetLayout; // filled by StackLayoutGenerator
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

	struct FunctionInfo;
	struct BasicBlock
	{
		std::vector<BasicBlock const*> entries;
		std::vector<Statement> statements;
		struct ConditionalJump
		{
			Expression condition;
			BasicBlock* nonZero = nullptr;
			BasicBlock* zero = nullptr;
		};
		struct Jump
		{
			BasicBlock* target = nullptr;
		};
		struct FunctionReturn { DFG::FunctionInfo* info = nullptr; };
		struct Stop {};
		struct Revert {};
		std::variant<std::monostate, Jump, ConditionalJump, FunctionReturn, Stop, Revert> exit;
		struct StackLayout
		{
			Stack entry;
			Stack exit;
		};
		std::optional<StackLayout> stackLayout;
	};
	std::list<BasicBlock> blocks;
	std::vector<Scope::Variable> ghostVariables;
	std::vector<yul::FunctionCall> ghostCalls;

	struct FunctionInfo
	{
		std::shared_ptr<DebugData const> debugData;
		Scope::Function const* function = nullptr;
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

struct ReturnLabelSlot
{
	/// The call returning to this label or, when generating a function, nullptr for the label to which the function
	/// is supposed to return.
	DFG::FunctionCall const* call = nullptr;
	bool operator==(ReturnLabelSlot const& _rhs) const { return call == _rhs.call; }
	bool operator<(ReturnLabelSlot const& _rhs) const { return call < _rhs.call; }
};
struct VariableSlot
{
	DFG::Variable const* variable = nullptr;
	bool operator==(VariableSlot const& _rhs) const { return variable->variable == _rhs.variable->variable; }
	bool operator<(VariableSlot const& _rhs) const { return variable->variable < _rhs.variable->variable; }
};
struct LiteralSlot
{
	u256 value;
	std::shared_ptr<DebugData const> debugData{};
	bool operator==(LiteralSlot const& _rhs) const { return value == _rhs.value; }
	bool operator<(LiteralSlot const& _rhs) const { return value < _rhs.value; }
};
struct TemporarySlot
{
	std::variant<DFG::FunctionCall const*, DFG::BuiltinCall const*> call;
	size_t idx = 0;
	bool operator==(TemporarySlot const& _rhs) const { return call == _rhs.call && idx == _rhs.idx; }
	bool operator<(TemporarySlot const& _rhs) const { return std::make_pair(call, idx) < std::make_pair(_rhs.call, _rhs.idx); }
};

class StackLayoutGenerator
{
public:
	static void generate(DFG::BasicBlock& _entry)
	{
		StackLayoutGenerator{}(_entry);
	}
	static void generate(DFG::FunctionInfo& _info);
	void operator()(DFG::BasicBlock& _block);

	void operator()(DFG::Declaration& _declaration);
	void operator()(DFG::Assignment& _declaration);
	void operator()(DFG::ExpressionStatement& _declaration);

	void operator()(DFG::Variable& _variable);
	void operator()(DFG::Literal& _literal);
	void operator()(DFG::FunctionCall& _call);
	void operator()(DFG::BuiltinCall& _call);

	static Stack combineStacks(Stack const& _stack1, Stack const& _stack2);
private:
	StackLayoutGenerator() {}
	Stack* m_currentStack = nullptr;
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
