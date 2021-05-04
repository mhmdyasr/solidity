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
#include <libyul/DataFlowGraph.h>
#include <libyul/AST.h>
#include <libyul/Utilities.h>

#include <libsolutil/cxx20.h>
#include <libsolutil/Visitor.h>

#include <range/v3/range/conversion.hpp>
#include <range/v3/view/enumerate.hpp>
#include <range/v3/view/reverse.hpp>
#include <range/v3/view/take_last.hpp>

using namespace solidity;
using namespace solidity::yul;
using namespace std;


namespace {
// Debugging
std::string SlotToString(StackSlot const& _slot)
{
	return std::visit(solidity::util::GenericVisitor{
		[](FunctionCallSlot const& _functionCall) { return "CALL(" + _functionCall.call().functionName.name.str() + ")"; },
		[](ExternalIdentifierSlot const& _externalIdentifier) { return "EXT(" + _externalIdentifier.identifier().name.str() + ")"; },
		[](LiteralSlot const& _literal) { return "LITERAL(" + solidity::util::toHex(_literal.value(), solidity::util::HexPrefix::Add) + ")"; },
		[](VariableSlot const& _variable) { return "VAR(" + _variable.variable().name.str() + ")"; },
		[](JunkSlot const&) { return string("JUNK"); },
		[](ReturnLabelSlot const&) { return string("RETURNLABEL"); },
		[](auto const&) { return string("UNKNOWN"); }
	}, _slot.content);
}
void PrintStackLayout(StackLayout const& _layout)
{
	std::cout << "[ ";
	for (auto const& slot: _layout)
		std::cout << SlotToString(*slot) << " ";
	std::cout << "]" << std::endl;
}
}

DataFlowGraphBuilder::DataFlowGraphBuilder(AsmAnalysisInfo& _analysisInfo, EVMDialect const& _dialect, BasicBlock& _rootBlock):
m_asmAnalysisInfo(_analysisInfo), m_dialect(_dialect), m_rootBlock(_rootBlock)
{
	m_currentBasicBlock = &m_rootBlock;
}

void DataFlowGraphBuilder::operator()(Literal const& _literal)
{
	pushExpression<LiteralSlot>(valueOfLiteral(_literal));
	m_currentExpression = m_currentStackLayout.back();
}

void DataFlowGraphBuilder::operator()(Identifier const& _identifier)
{
	yulAssert(m_scope, "");
	if (!m_scope->lookup(_identifier.name, util::GenericVisitor{
		[](Scope::Function const&) {
			yulAssert(false, "");
		},
		[&](Scope::Variable const& _var) {
			pushExpression<VariableSlot>(&_var);
		}
	}))
		pushExpression<ExternalIdentifierSlot>(&_identifier);
	m_currentExpression = m_currentStackLayout.back();
}

void DataFlowGraphBuilder::operator()(FunctionCall const& _functionCall)
{
	yulAssert(m_scope, "");

	std::cout << "BEFORE FUNCTION CALL: " << _functionCall.functionName.name.str() << std::endl;
	PrintStackLayout(m_currentStackLayout);

	BuiltinFunctionForEVM const* builtin = m_dialect.builtin(_functionCall.functionName.name);
	auto isLiteralArgument = [builtin](size_t i) -> bool {
		return builtin && builtin->literalArgument(i);
	};

	size_t properArgumentCount = 0;
	for (auto&& [idx, arg]: ranges::views::enumerate(_functionCall.arguments) | ranges::views::reverse)
		if (!isLiteralArgument(idx))
		{
			++properArgumentCount;
			visit(arg);
		}

	std::cout << "FUNCTION CALL WITH ARGS: " << _functionCall.functionName.name.str() << std::endl;
	PrintStackLayout(m_currentStackLayout);

	StackLayout arguments = m_currentStackLayout | ranges::views::take_last(properArgumentCount) | ranges::to<StackLayout>;
	popLast(arguments.size());

	size_t numReturnValues = 0;
	if (builtin)
	{
		// TODO: handle reverts here or in ExpressionStatement
		m_currentExpression = makeExpression<FunctionCallSlot>(_functionCall, builtin, std::move(arguments));
		numReturnValues = builtin->returns.size();
	}
	else
		yulAssert(m_scope->lookup(_functionCall.functionName.name, util::GenericVisitor{
			[&](Scope::Function const& _fun) {
				m_currentExpression = makeExpression<FunctionCallSlot>(_functionCall, &_fun, std::move(arguments));
				numReturnValues = _fun.returns.size();
			},
			[](Scope::Variable const&) {
				yulAssert(false, "");
			}
		}), "");

	for (size_t i = 0; i < numReturnValues; ++i)
		m_currentStackLayout.emplace_back(m_currentExpression);

	std::cout << "AFTER FUNCTION CALL: " << _functionCall.functionName.name.str() << ": " << properArgumentCount << " -> " << numReturnValues << std::endl;
	PrintStackLayout(m_currentStackLayout);
}

void DataFlowGraphBuilder::popLast(size_t n)
{
	while (n--)
		m_currentStackLayout.pop_back();
}

void DataFlowGraphBuilder::visit(Expression const& _expression)
{
	std::visit(*this, _expression);
}

void DataFlowGraphBuilder::operator()(VariableDeclaration const& _variableDeclaration)
{
	yulAssert(m_scope, "");
	yulAssert(m_currentBasicBlock, "");
	yulAssert(!m_currentExpression, "");
	if (_variableDeclaration.value)
	{
		visit(*_variableDeclaration.value);
		BasicBlockEntry& basicBlockEntry = m_currentBasicBlock->content.emplace_back(
			m_currentExpression,
			StackLayout{}
		);
		m_currentExpression = nullptr;
		for (auto&& [slot, var]: ranges::views::zip(
			m_currentStackLayout | ranges::views::take_last(_variableDeclaration.variables.size()),
			_variableDeclaration.variables
		))
			yulAssert(m_scope->lookup(var.name, util::GenericVisitor{
				[](Scope::Function const&) {
					yulAssert(false, "");
				},
				[&](Scope::Variable const& _var) {
					slot = makeExpression<VariableSlot>(&_var);
				}
			}), "");
		basicBlockEntry.outputLayout = m_currentStackLayout;
	}
	else
		for (auto const& var: _variableDeclaration.variables)
			yulAssert(m_scope->lookup(var.name, util::GenericVisitor{
				[](Scope::Function const&) {
					yulAssert(false, "");
				},
				[&](Scope::Variable const& _var) {
					pushExpression<LiteralSlot>(0);
					BasicBlockEntry& basicBlockEntry = m_currentBasicBlock->content.emplace_back(
						m_currentExpression,
						StackLayout{}
					);
					m_currentStackLayout.pop_back();

					pushExpression<VariableSlot>(&_var);
					basicBlockEntry.outputLayout = m_currentStackLayout;
					m_currentExpression = nullptr;
				}
			}), "");
}

void DataFlowGraphBuilder::operator()(Assignment const& _assignment)
{
	yulAssert(m_scope, "");
	yulAssert(m_currentBasicBlock, "");

	if (auto const* identifier = std::get_if<Identifier>(_assignment.value.get()))
	{
		yulAssert(_assignment.variableNames.size() == 1, "");
		if (m_scope->lookup(identifier->name, util::GenericVisitor{
			[&](Scope::Function const&) {
				yulAssert(false, "");
			},
			[&](Scope::Variable const& _lhs) {
				if (!m_scope->lookup(_assignment.variableNames.front().name, util::GenericVisitor{
					[](Scope::Function const&) {
						yulAssert(false, "");
					},
					[&](Scope::Variable const& _rhs) {
						for (auto& stackEntry: m_currentStackLayout)
							if (VariableSlot const* varNode = get_if<VariableSlot>(&stackEntry->content))
								if (&varNode->variable() == &_lhs)
									stackEntry = makeExpression<VariableSlot>(&_rhs);
					}
				}))
					yulAssert(false, "Assignment from external identifier unimplemented.");
			}
		}))
		{
			m_currentBasicBlock->content.emplace_back(
				nullptr,
				m_currentStackLayout
			);
			return;
		}
		// Assignments to external identifiers are handled below.
	}

	visit(*_assignment.value);
	BasicBlockEntry& basicBlockEntry = m_currentBasicBlock->content.emplace_back(
		m_currentExpression,
		StackLayout{}
	);

	std::vector<Scope::Variable const*> variables;
	for (auto&& [slot, var]: ranges::views::zip(
		m_currentStackLayout | ranges::views::take_last(_assignment.variableNames.size()),
		_assignment.variableNames
	))
	{
		if (!m_scope->lookup(var.name, util::GenericVisitor{
			[](Scope::Function const&) {
				yulAssert(false, "");
			},
			[&](Scope::Variable const& _var) {
				for (auto& stackEntry: m_currentStackLayout)
					if (VariableSlot const* varNode = get_if<VariableSlot>(&stackEntry->content))
						if (&varNode->variable() == &_var)
							stackEntry = makeExpression<JunkSlot>();
				slot = makeExpression<VariableSlot>(&_var);
				std::cout << "Assign to: " << _var.name.str() << std::endl;
			}
		}))
			yulAssert(false, "Multi-assignment to external identifier unimplemented.");
	}
	basicBlockEntry.outputLayout = m_currentStackLayout;
}

void DataFlowGraphBuilder::operator()(ExpressionStatement const& _expressionStatement)
{
	yulAssert(m_currentBasicBlock, "");
	visit(_expressionStatement.expression);
	m_currentBasicBlock->content.emplace_back(
		m_currentExpression,
		m_currentStackLayout
	);
	m_currentExpression = nullptr;
	// TODO: handle reverts here or in FunctionCall
}

void DataFlowGraphBuilder::operator()(If const& _if)
{
	yulAssert(m_currentBasicBlock, "");
	yulAssert(!m_currentExpression, "");
	std::cout << "BEFORE IF ";
	PrintStackLayout(m_currentStackLayout);
	visit(*_if.condition);
	std::cout << "WITH CONDITION ";
	PrintStackLayout(m_currentStackLayout);
	m_currentBasicBlock->content.emplace_back(
		m_currentExpression,
		m_currentStackLayout
	);
	m_currentExpression = nullptr;

	BasicBlock& afterIf = m_currentBasicBlock->subBlocks.emplace_back();
	BasicBlock& ifBranch = m_currentBasicBlock->subBlocks.emplace_back();
	ifBranch.entries.emplace_back(m_currentBasicBlock);
	afterIf.entries.emplace_back(m_currentBasicBlock);
	m_currentBasicBlock->exits = { &afterIf, &ifBranch };
	m_currentStackLayout.pop_back();
	{
		StackLayout stackLayout = m_currentStackLayout;
		ScopedSaveAndRestore saveAndRestoreStack(m_currentStackLayout, std::move(stackLayout));
		m_currentBasicBlock = &ifBranch;
		(*this)(_if.body);
		afterIf.entries.emplace_back(m_currentBasicBlock);
	}
	m_currentBasicBlock = &afterIf;
}

void DataFlowGraphBuilder::operator()(Switch const& _switch)
{
	visit(*_switch.expression);
	// TODO
	yulAssert(false, "");
}

void DataFlowGraphBuilder::operator()(ForLoop const& _forLoop)
{
	// TODO: scoping!
	(*this)(_forLoop.pre);
	visit(*_forLoop.condition);

	BasicBlock& afterLoop = m_currentBasicBlock->subBlocks.emplace_back();
	BasicBlock& bodyBlock = m_currentBasicBlock->subBlocks.emplace_back();
	BasicBlock& postBlock = m_currentBasicBlock->subBlocks.emplace_back();

	ScopedSaveAndRestore forLoopInfo(m_currentForLoopInfo, ForLoopInfo{&postBlock, &afterLoop});

	afterLoop.entries.emplace_back(m_currentBasicBlock);
	m_currentBasicBlock->exits.emplace_back(&afterLoop);
	m_currentBasicBlock->exits.emplace_back(&bodyBlock);
	{

	}

	// TODO
	yulAssert(false, "");
}

void DataFlowGraphBuilder::operator()(FunctionDefinition const& _functionDefinition)
{
	std::cout << "ENTER FUNCTION: " << _functionDefinition.name.str() << std::endl;
	DataFlowGraphBuilder subGraphBuilder{
		m_asmAnalysisInfo,
		m_dialect,
		m_rootBlock
	};
	Scope* virtualFunctionScope = m_asmAnalysisInfo.scopes.at(m_asmAnalysisInfo.virtualBlocks.at(&_functionDefinition).get()).get();
	StackSlot const* returnLabelNode = makeExpression<ReturnLabelSlot>();
	StackLayout entryLayout;
	entryLayout.emplace_back(returnLabelNode);
	for (auto const& parameter: _functionDefinition.parameters | ranges::views::reverse)
	{
		Scope::Variable const& var = std::get<Scope::Variable>(virtualFunctionScope->identifiers.at(parameter.name));
		entryLayout.emplace_back(makeExpression<VariableSlot>(&var));
	}
	StackLayout exitLayout;
	for (auto const& returnVariable: _functionDefinition.returnVariables) // TODO: reverse?
	{
		Scope::Variable const& var = std::get<Scope::Variable>(virtualFunctionScope->identifiers.at(returnVariable.name));
		exitLayout.emplace_back(makeExpression<VariableSlot>(&var));
	}
	exitLayout.emplace_back(returnLabelNode);

	BasicBlock& functionRootBlock = m_rootBlock.subBlocks.emplace_back();
	subGraphBuilder.m_currentBasicBlock = &functionRootBlock;
	subGraphBuilder.m_scope = m_scope;
	subGraphBuilder.m_currentStackLayout = entryLayout;
	subGraphBuilder(_functionDefinition.body);

	yulAssert(m_scope->identifiers.count(_functionDefinition.name), "");
	Scope::Function& function = std::get<Scope::Function>(m_scope->identifiers.at(_functionDefinition.name));

	m_rootBlock.functions.emplace(std::make_pair(&function, FunctionInfo{
		&functionRootBlock,
		move(entryLayout),
		move(exitLayout)
	}));
	std::cout << "EXIT FUNCTION: " << _functionDefinition.name.str() << std::endl;
}

void DataFlowGraphBuilder::operator()(Break const&)
{
	yulAssert(false, "");
}


void DataFlowGraphBuilder::operator()(Leave const&)
{
	yulAssert(false, "");
}

void DataFlowGraphBuilder::operator()(Continue const&)
{
	yulAssert(false, "");
}


void DataFlowGraphBuilder::operator()(Block const& _block)
{
	ScopedSaveAndRestore scopedSaveAndRestore(m_scope, m_asmAnalysisInfo.scopes.at(&_block).get());

	std::cout << "BLOCK ";
	PrintStackLayout(m_currentStackLayout);

	for (auto const& stmt: _block.statements)
	{
		std::visit(*this, stmt);
		PrintStackLayout(m_currentStackLayout);
	}
}