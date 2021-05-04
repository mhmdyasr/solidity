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

struct StackSlot;

class ExternalIdentifierSlot
{
public:
	ExternalIdentifierSlot(Identifier const& _identifier): m_identifier(_identifier) {}
	Identifier const& identifier() const { return m_identifier; }
private:
	Identifier const& m_identifier;
};
class LiteralSlot
{
public:
	LiteralSlot(u256&& _value): m_value(_value) {}
	u256 const& value() const { return m_value; }
private:
	u256 m_value = u256(0);
};
class VariableSlot
{
public:
	VariableSlot(Scope::Variable const& _variable): m_variable(_variable) {}
	Scope::Variable const& variable() const { return m_variable; }
private:
	Scope::Variable const& m_variable;
};
struct JunkSlot {};
class IntermediateValueSlot
{
public:
	IntermediateValueSlot(FunctionCall const& _funCall, size_t _index): m_call(_funCall), m_index(_index) {}
	FunctionCall const& call() const { return m_call; }
	size_t index() const { return m_index; }
private:
	FunctionCall const& m_call;
	size_t m_index = 0;
};
struct ReturnLabelSlot {};

struct StackSlot
{
public:
	template<typename V>
	StackSlot(V&& _v): content(std::forward<V>(_v))
	{
	}
	std::variant<IntermediateValueSlot, ExternalIdentifierSlot, LiteralSlot, VariableSlot, JunkSlot, ReturnLabelSlot> content;
	langutil::SourceLocation location;
};
using StackLayout = std::vector<StackSlot const*>;


class FunctionCallOperation
{
public:
	FunctionCallOperation(Scope::Function const& _function, FunctionCall const& _call):
	m_function(_function), m_call(_call) {}

	FunctionCall const& call() const { return m_call; }
	Scope::Function const& function() const	{ return m_function; }
private:
	Scope::Function const& m_function;
	FunctionCall const& m_call;
};
class BuiltinCallOperation
{
public:
	BuiltinCallOperation(BuiltinFunctionForEVM const& _builtin, FunctionCall const& _call, size_t _properArgumentCount):
	m_builtin(_builtin), m_call(_call), m_properArgumentCount(_properArgumentCount) {}
	FunctionCall const& call() const { return m_call; }
	BuiltinFunctionForEVM const& builtin() const { return m_builtin; }
	size_t properArgumentCount() const { return m_properArgumentCount; }
private:
	BuiltinFunctionForEVM const& m_builtin;
	FunctionCall const& m_call;
	size_t m_properArgumentCount = 0;
};
class StackRequestOperation
{
public:
	StackRequestOperation() {}
};
struct BasicBlockEntry
{
	BasicBlockEntry(FunctionCallOperation _operation, StackLayout _outputLayout): operation(std::move(_operation)), outputLayout(std::move(_outputLayout)) {}
	BasicBlockEntry(BuiltinCallOperation _operation, StackLayout _outputLayout): operation(std::move(_operation)), outputLayout(std::move(_outputLayout)) {}
	BasicBlockEntry(StackLayout _outputLayout): operation(StackRequestOperation{}), outputLayout(std::move(_outputLayout)) {}
	BasicBlockEntry(BasicBlockEntry const&) = delete;
	BasicBlockEntry(BasicBlockEntry&&) = delete;
	BasicBlockEntry operator=(BasicBlockEntry const&) = delete;
	BasicBlockEntry operator=(BasicBlockEntry&&) = delete;

	std::variant<StackRequestOperation, BuiltinCallOperation, FunctionCallOperation> operation;
	std::vector<StackSlot const*> outputLayout;
};

struct BasicBlock
{
	BasicBlock() {}
	BasicBlock(BasicBlock const&) = delete;
	BasicBlock(BasicBlock&&) = delete;
	BasicBlock operator=(BasicBlock const&) = delete;
	BasicBlock operator=(BasicBlock&&) = delete;
	std::vector<BasicBlock const*> entries;
	StackLayout initialLayout;
	std::list<BasicBlockEntry> content;
	StackLayout finalLayout;
	std::vector<BasicBlock const*> exits;
	bool isFunctionExit = false;
};

struct DataFlowGraph
{
	BasicBlock* root;
	std::map<Scope::Function const*, std::pair<BasicBlock*, BasicBlock*>> functions;
	std::list<StackSlot> stackSlots;
	std::list<BasicBlock> blocks;
};

// Debugging only.
std::string StackLayoutToString(StackLayout const& _layout);

class DataFlowGraphBuilder
{
public:
	DataFlowGraphBuilder(
		DataFlowGraph& _graph,
		AsmAnalysisInfo& _analysisInfo,
		EVMDialect const& _dialect,
		BasicBlock* _root
	);

	void operator()(Literal const& _literal);
	void operator()(Identifier const& _identifier);
	void operator()(FunctionCall const&);
	void operator()(ExpressionStatement const& _statement);
	void operator()(Assignment const& _assignment);
	void operator()(VariableDeclaration const& _varDecl);
	void operator()(If const& _if);
	void operator()(Switch const& _switch);
	void operator()(FunctionDefinition const&);
	void operator()(ForLoop const&);
	void operator()(Break const&);
	void operator()(Continue const&);
	void operator()(Leave const&);
	void operator()(Block const& _block);
	void visit(Expression const& _expression);

	BasicBlock* lastBlock()
	{
		return m_currentBasicBlock;
	}
private:
	void popLast(size_t n);
	template<typename T, typename... Args>
	StackSlot const* makeExpression(Args&&... args)
	{
		yulAssert(m_currentBasicBlock, "");
		return &m_graph.stackSlots.emplace_back(StackSlot(T(std::forward<Args>(args)...)));
	}
	template<typename T, typename... Args>
	void pushExpression(Args&&... args)
	{
		m_currentStackLayout.emplace_back(makeExpression<T>(std::forward<Args>(args)...));
	}
	DataFlowGraph& m_graph;
	AsmAnalysisInfo& m_asmAnalysisInfo;
	EVMDialect const& m_dialect;
	std::vector<StackSlot const*> m_currentStackLayout;
	BasicBlock* m_currentBasicBlock = nullptr;
	Scope* m_scope = nullptr;
	struct ForLoopInfo { BasicBlock* postBlock = nullptr; BasicBlock* afterLoopBlock = nullptr; };
	ForLoopInfo m_currentForLoopInfo;
	BasicBlock* m_currentFunctionExit = nullptr;

	// debugging only
	size_t m_currentIndent = 0;
};

}
