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
#include <libyul/AsmPrinter.h>

#include <libsolutil/cxx20.h>
#include <libsolutil/Visitor.h>

#include <range/v3/range/conversion.hpp>
#include <range/v3/view/enumerate.hpp>
#include <range/v3/view/reverse.hpp>
#include <range/v3/view/take_last.hpp>
#include <range/v3/view/transform.hpp>
#include <boost/algorithm/string/join.hpp>

using namespace solidity;
using namespace solidity::yul;
using namespace std;


// Debugging only.
namespace {
std::string SlotToString(StackSlot const& _slot)
{
	return std::visit(solidity::util::GenericVisitor{
		[](IntermediateValueSlot const& _intermediateValue) { return "TMP(" + _intermediateValue.call().functionName.name.str() + "[" + to_string(_intermediateValue.index()) + "])"; },
		[](ExternalIdentifierSlot const& _externalIdentifier) { return "EXT(" + _externalIdentifier.identifier().name.str() + ")"; },
		[](LiteralSlot const& _literal) { return "LITERAL(" + solidity::util::toHex(_literal.value(), solidity::util::HexPrefix::Add) + ")"; },
		[](VariableSlot const& _variable) { return "VAR(" + _variable.variable().name.str() + ")"; },
		[](JunkSlot const&) { return string("JUNK"); },
		[](ReturnLabelSlot const&) { return string("RETURNLABEL"); },
		[](auto const&) { return string("UNKNOWN"); }
	}, _slot.content);
}
}

// Debugging only.
std::string yul::StackLayoutToString(StackLayout const& _layout)
{
	std::string result("[ ");
	for (auto const& slot: _layout)
		result += SlotToString(*slot) + " ";
	result += "]";
	return result;
}

DataFlowGraphBuilder::DataFlowGraphBuilder(DataFlowGraph& _graph, AsmAnalysisInfo& _analysisInfo, EVMDialect const& _dialect, BasicBlock* _root):
m_graph(_graph), m_asmAnalysisInfo(_analysisInfo), m_dialect(_dialect)
{
	m_currentBasicBlock = _root;
}

void DataFlowGraphBuilder::operator()(Literal const& _literal)
{
	pushExpression<LiteralSlot>(valueOfLiteral(_literal));
	std::cout << std::string(m_currentIndent, ' ') << "// " << _literal.value.str() << std::endl;
}

void DataFlowGraphBuilder::operator()(Identifier const& _identifier)
{
	yulAssert(m_scope, "");
	if (!m_scope->lookup(_identifier.name, util::GenericVisitor{
		[](Scope::Function const&) {
			yulAssert(false, "");
		},
		[&](Scope::Variable const& _var) {
			pushExpression<VariableSlot>(_var);
		}
	}))
		pushExpression<ExternalIdentifierSlot>(_identifier);

	std::cout << std::string(m_currentIndent, ' ') << "// " << _identifier.name.str() << std::endl;
}

void DataFlowGraphBuilder::operator()(FunctionCall const& _functionCall)
{
	yulAssert(m_scope, "");
	yulAssert(m_currentBasicBlock, "");

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

	std::cout << std::string(m_currentIndent, ' ') << "// " << StackLayoutToString(m_currentStackLayout) << std::endl;
	std::cout << std::string(m_currentIndent, ' ') << "// " << _functionCall.functionName.name.str() << std::endl;

	m_currentBasicBlock->content.emplace_back(m_currentStackLayout); // Request stack layout.

	popLast(properArgumentCount);

	if (builtin)
	{
		for (size_t i = 0; i < builtin->returns.size(); ++i)
			pushExpression<IntermediateValueSlot>(_functionCall, i);
		m_currentBasicBlock->content.emplace_back(BuiltinCallOperation(*builtin, _functionCall, properArgumentCount), m_currentStackLayout);
	}
	else
		yulAssert(m_scope->lookup(_functionCall.functionName.name, util::GenericVisitor{
			[&](Scope::Function const& _fun) {
				for (size_t i = 0; i < _fun.returns.size(); ++i)
					pushExpression<IntermediateValueSlot>(_functionCall, i);
				m_currentBasicBlock->content.emplace_back(FunctionCallOperation(_fun, _functionCall), m_currentStackLayout);
			},
			[](Scope::Variable const&) {
				yulAssert(false, "");
			}
		}), "");


	std::cout << std::string(m_currentIndent, ' ') << "// " << StackLayoutToString(m_currentStackLayout) << std::endl;
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
	if (_variableDeclaration.value)
		visit(*_variableDeclaration.value);
	else
		for (size_t i = 0; i < _variableDeclaration.variables.size(); ++i)
			pushExpression<LiteralSlot>(0);

	for (auto&& [slot, var]: ranges::views::zip(
		m_currentStackLayout | ranges::views::take_last(_variableDeclaration.variables.size()),
		_variableDeclaration.variables
	))
		yulAssert(m_scope->lookup(var.name, util::GenericVisitor{
			[](Scope::Function const&) {
				yulAssert(false, "");
			},
			[&](Scope::Variable const& _var) {
				slot = makeExpression<VariableSlot>(_var);
			}
		}), "");
	m_currentBasicBlock->content.emplace_back(m_currentStackLayout); // Request stack layout.

	std::cout << std::string(m_currentIndent, ' ') << AsmPrinter{m_dialect}(_variableDeclaration) << std::endl;
	std::cout << std::string(m_currentIndent, ' ') << "// " << StackLayoutToString(m_currentStackLayout) << std::endl;
}

void DataFlowGraphBuilder::operator()(Assignment const& _assignment)
{
	yulAssert(m_scope, "");
	yulAssert(m_currentBasicBlock, "");

	visit(*_assignment.value);

	yulAssert(!m_currentBasicBlock->content.empty(), "");

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
				slot = makeExpression<VariableSlot>(_var);
			}
		}))
			yulAssert(false, "Multi-assignment to external identifier unimplemented.");
	}
	m_currentBasicBlock->content.back().outputLayout = m_currentStackLayout;

	std::cout << std::string(m_currentIndent, ' ') << AsmPrinter{m_dialect}(_assignment) << std::endl;
	std::cout << std::string(m_currentIndent, ' ') << "// " << StackLayoutToString(m_currentStackLayout) << std::endl;
}

void DataFlowGraphBuilder::operator()(ExpressionStatement const& _expressionStatement)
{
	yulAssert(m_currentBasicBlock, "");
	visit(_expressionStatement.expression);
	// TODO: handle reverts here or in FunctionCall

	std::cout << std::string(m_currentIndent, ' ') << AsmPrinter{m_dialect}(_expressionStatement) << std::endl;
	std::cout << std::string(m_currentIndent, ' ') << "// " << StackLayoutToString(m_currentStackLayout) << std::endl;
}

void DataFlowGraphBuilder::operator()(If const& _if)
{
	yulAssert(m_currentBasicBlock, "");
	visit(*_if.condition);

	std::cout << std::string(m_currentIndent, ' ') << "// " << StackLayoutToString(m_currentStackLayout) << std::endl;
	std::cout << std::string(m_currentIndent, ' ') << "if " << std::visit(AsmPrinter{m_dialect}, *_if.condition) << std::endl;

	m_currentBasicBlock->finalLayout = m_currentStackLayout;

	BasicBlock& afterIf = m_graph.blocks.emplace_back();
	BasicBlock& ifBranch = m_graph.blocks.emplace_back();
	ifBranch.entries.emplace_back(m_currentBasicBlock);
	afterIf.entries.emplace_back(m_currentBasicBlock);
	m_currentBasicBlock->exits = { &afterIf, &ifBranch };
	m_currentStackLayout.pop_back();
	afterIf.initialLayout = m_currentStackLayout;
	ifBranch.initialLayout = m_currentStackLayout;
	{
		StackLayout stackLayout = m_currentStackLayout;
		ScopedSaveAndRestore saveAndRestoreStack(m_currentStackLayout, std::move(stackLayout));
		m_currentBasicBlock = &ifBranch;
		(*this)(_if.body);
		m_currentBasicBlock->finalLayout = m_currentStackLayout;
		m_currentBasicBlock->exits = { &afterIf };
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

	BasicBlock& afterLoop = m_graph.blocks.emplace_back();
	BasicBlock& bodyBlock = m_graph.blocks.emplace_back();
	BasicBlock& postBlock = m_graph.blocks.emplace_back();

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
	std::cout << std::endl;
	std::cout << std::string(m_currentIndent, ' ') << "function " << _functionDefinition.name.str() << "(" <<
	boost::algorithm::join(
		_functionDefinition.parameters | ranges::views::transform(
			[this](TypedName argument) { return argument.name.str(); }
		),
		", "
	)
	<< ") -> " <<
  boost::algorithm::join(
	  _functionDefinition.returnVariables | ranges::views::transform(
		  [this](TypedName argument) { return argument.name.str(); }
	  ),
	  ", "
  )
	<< std::endl;
	Scope* virtualFunctionScope = m_asmAnalysisInfo.scopes.at(m_asmAnalysisInfo.virtualBlocks.at(&_functionDefinition).get()).get();
	StackSlot const* returnLabelNode = makeExpression<ReturnLabelSlot>();
	StackLayout entryLayout;
	entryLayout.emplace_back(returnLabelNode);
	for (auto const& parameter: _functionDefinition.parameters | ranges::views::reverse)
	{
		Scope::Variable const& var = std::get<Scope::Variable>(virtualFunctionScope->identifiers.at(parameter.name));
		entryLayout.emplace_back(makeExpression<VariableSlot>(var));
	}
	StackLayout exitLayout;
	for (auto const& returnVariable: _functionDefinition.returnVariables) // TODO: reverse?
	{
		Scope::Variable const& var = std::get<Scope::Variable>(virtualFunctionScope->identifiers.at(returnVariable.name));
		exitLayout.emplace_back(makeExpression<VariableSlot>(var));
	}
	exitLayout.emplace_back(returnLabelNode);

	BasicBlock& functionRootBlock = m_graph.blocks.emplace_back();
	DataFlowGraphBuilder subGraphBuilder{
		m_graph,
		m_asmAnalysisInfo,
		m_dialect,
		&functionRootBlock
	};
	functionRootBlock.initialLayout = entryLayout;
	functionRootBlock.finalLayout = exitLayout;
	subGraphBuilder.m_currentBasicBlock = &functionRootBlock;
	subGraphBuilder.m_scope = m_scope;
	subGraphBuilder.m_currentStackLayout = entryLayout;
	subGraphBuilder.m_currentIndent = m_currentIndent;
	subGraphBuilder(_functionDefinition.body);
	subGraphBuilder.m_currentBasicBlock->isFunctionExit = true;

	yulAssert(m_scope->identifiers.count(_functionDefinition.name), "");
	Scope::Function& function = std::get<Scope::Function>(m_scope->identifiers.at(_functionDefinition.name));

	std::cout << std::string(m_currentIndent, ' ') << "// " << StackLayoutToString(exitLayout) << std::endl;
	std::cout << std::endl;
	m_graph.functions[&function] = std::make_pair(&functionRootBlock, subGraphBuilder.m_currentBasicBlock);
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
	std::cout << std::string(m_currentIndent, ' ') << "{ // " << StackLayoutToString(m_currentStackLayout) << std::endl;
	m_currentIndent += 2;

	ScopedSaveAndRestore scopedSaveAndRestore(m_scope, m_asmAnalysisInfo.scopes.at(&_block).get());

	for (auto const& stmt: _block.statements)
		std::visit(*this, stmt);

	m_currentIndent -= 2;
	std::cout << std::string(m_currentIndent, ' ') << "} // " << StackLayoutToString(m_currentStackLayout) << std::endl;

}