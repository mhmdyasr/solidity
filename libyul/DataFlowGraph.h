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

struct ReturnLabelSlot
{
	/// The call returning to this label or, when generating a function, nullptr for the label to which the function
	/// is supposed to return.
	yul::FunctionCall const* call = nullptr;
	bool operator==(ReturnLabelSlot const& _rhs) const { return call == _rhs.call; }
	bool operator<(ReturnLabelSlot const& _rhs) const { return call < _rhs.call; }
};
struct VariableSlot
{
	Scope::Variable const* variable = nullptr;
	std::shared_ptr<DebugData const> debugData{};
	bool operator==(VariableSlot const& _rhs) const { return variable == _rhs.variable; }
	bool operator<(VariableSlot const& _rhs) const { return variable < _rhs.variable; }
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
	yul::FunctionCall const* call = nullptr;
	size_t idx = 0;
	bool operator==(TemporarySlot const& _rhs) const { return call == _rhs.call && idx == _rhs.idx; }
	bool operator<(TemporarySlot const& _rhs) const { return std::make_pair(call, idx) < std::make_pair(_rhs.call, _rhs.idx); }
};
struct JunkSlot
{
	bool operator==(JunkSlot const&) const { return true; }
	bool operator<(JunkSlot const&) const { return false; }
};
using StackSlot = std::variant<ReturnLabelSlot, VariableSlot, LiteralSlot, TemporarySlot, JunkSlot>;
using Stack = std::deque<StackSlot>;


struct DFG
{
	explicit DFG() {}
	DFG(DFG const&) = delete;
	DFG(DFG&&) = delete;
	DFG& operator=(DFG const&) = delete;
	DFG& operator=(DFG&&) = delete;
	struct BuiltinCall
	{
		std::shared_ptr<DebugData const> debugData;
		BuiltinFunctionForEVM const* builtin = nullptr;
		yul::FunctionCall const* functionCall = nullptr;
		size_t arguments = 0;
	};
	struct FunctionCall
	{
		std::shared_ptr<DebugData const> debugData;
		Scope::Function const* function = nullptr;
		yul::FunctionCall const* functionCall = nullptr;
	};
	struct Assignment
	{
		std::shared_ptr<DebugData const> debugData;
		std::vector<VariableSlot> variables;
	};

	struct Operation
	{
		Stack input;
		Stack output;
		std::variant<FunctionCall, BuiltinCall, Assignment> operation;
	};

	struct FunctionInfo;
	struct BasicBlock
	{
		std::vector<BasicBlock const*> entries;
		std::vector<Operation> operations;
		struct ConditionalJump
		{
			StackSlot condition;
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
		std::vector<VariableSlot> parameters;
		std::vector<VariableSlot> returnVariables;
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

	StackSlot operator()(Literal const& _literal);
	StackSlot operator()(Identifier const& _identifier);
	StackSlot operator()(FunctionCall const&);

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
	DFG::Operation& visitFunctionCall(FunctionCall const&);

	Scope::Variable const& lookupVariable(YulString _name) const;
	std::pair<DFG::BasicBlock*, DFG::BasicBlock*> makeConditionalJump(StackSlot _condition);
	void makeConditionalJump(StackSlot _condition, DFG::BasicBlock& _nonZero, DFG::BasicBlock& _zero);
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