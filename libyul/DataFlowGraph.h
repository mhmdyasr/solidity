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

class FunctionCallSlot
{
public:
	FunctionCallSlot(FunctionCall const& _call, Scope::Function const* _function, std::vector<StackSlot const*>&& _arguments):
	m_call(_call), m_function(_function), m_arguments(std::move(_arguments)) {}
	FunctionCallSlot(FunctionCall const& _call, BuiltinFunctionForEVM const* _function, std::vector<StackSlot const*>&& _arguments):
	m_call(_call), m_function(_function), m_arguments(std::move(_arguments)) {}

	FunctionCall const& call() const { return m_call; }
	Scope::Function const* function() const
	{
		if (auto const* fun = std::get_if<Scope::Function const*>(&m_function))
			return *fun;
		else
			return nullptr;
	}
	BuiltinFunctionForEVM const* builtin() const
	{
		if (auto const* fun = std::get_if<BuiltinFunctionForEVM const*>(&m_function))
			return *fun;
		else
			return nullptr;
	}
	std::vector<StackSlot const*> const& arguments() const { return m_arguments; }
private:
	FunctionCall const& m_call;
	std::variant<Scope::Function const*, BuiltinFunctionForEVM const*> m_function;
	std::vector<StackSlot const*> m_arguments;
};
class ExternalIdentifierSlot
{
public:
	ExternalIdentifierSlot(Identifier const* _identifier): m_identifier(_identifier) {}
	Identifier const& identifier() const { return *m_identifier; }
private:
	Identifier const* m_identifier = nullptr;
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
	VariableSlot(Scope::Variable const* _variable): m_variable(_variable) {}
	Scope::Variable const& variable() const { return *m_variable; }
private:
	Scope::Variable const* m_variable = nullptr;
};
class JunkSlot
{
public:
	JunkSlot() {}
};
struct ReturnLabelSlot {};

struct StackSlot
{
public:
	template<typename V>
	StackSlot(V&& _v): content(std::forward<V>(_v))
	{
	}
	std::variant<FunctionCallSlot, ExternalIdentifierSlot, LiteralSlot, VariableSlot, JunkSlot, ReturnLabelSlot> content;
	langutil::SourceLocation location;
};
using StackLayout = std::vector<StackSlot const*>;

struct BasicBlockEntry
{
	BasicBlockEntry(StackSlot const* _operation, StackLayout _outputLayout): operation(_operation), outputLayout(std::move(_outputLayout)) {}
	BasicBlockEntry(BasicBlockEntry const&) = delete;
	BasicBlockEntry(BasicBlockEntry&&) = delete;
	BasicBlockEntry operator=(BasicBlockEntry const&) = delete;
	BasicBlockEntry operator=(BasicBlockEntry&&) = delete;

	StackSlot const* operation = nullptr;
	std::vector<StackSlot const*> outputLayout;
};
struct FunctionInfo;
struct BasicBlock
{
	BasicBlock() {}
	BasicBlock(BasicBlock const&) = delete;
	BasicBlock(BasicBlock&&) = delete;
	BasicBlock operator=(BasicBlock const&) = delete;
	BasicBlock operator=(BasicBlock&&) = delete;
	std::vector<BasicBlock const*> entries;
	std::list<BasicBlockEntry> content;
	std::vector<BasicBlock const*> exits;

	std::map<Scope::Function const*, FunctionInfo> functions;
	// no logical relation, merely ownership
	std::list<BasicBlock> subBlocks;
	std::list<StackSlot> subExpressions;
};
struct FunctionInfo
{
	BasicBlock const* body = nullptr;
	StackLayout entryLayout;
	StackLayout exitLayout;
};

class DataFlowGraphBuilder
{
public:
	DataFlowGraphBuilder(
		AsmAnalysisInfo& _analysisInfo,
		EVMDialect const& _dialect,
		BasicBlock& _rootBlock
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
private:
	void popLast(size_t n);
	template<typename T, typename... Args>
	StackSlot const* makeExpression(Args&&... args)
	{
		yulAssert(m_currentBasicBlock, "");
		return &m_currentBasicBlock->subExpressions.emplace_back(StackSlot(T(std::forward<Args>(args)...)));
	}
	template<typename T, typename... Args>
	void pushExpression(Args&&... args)
	{
		m_currentStackLayout.emplace_back(makeExpression<T>(std::forward<Args>(args)...));
	}
	AsmAnalysisInfo& m_asmAnalysisInfo;
	EVMDialect const& m_dialect;
	std::vector<StackSlot const*> m_currentStackLayout;
	BasicBlock* m_currentBasicBlock = nullptr;
	StackSlot const* m_currentExpression = nullptr;
	BasicBlock& m_rootBlock;
	Scope* m_scope = nullptr;
	struct ForLoopInfo { BasicBlock* postBlock = nullptr; BasicBlock* afterLoopBlock = nullptr; };
	ForLoopInfo m_currentForLoopInfo;
	BasicBlock* m_currentFunctionExit = nullptr;
};

}
