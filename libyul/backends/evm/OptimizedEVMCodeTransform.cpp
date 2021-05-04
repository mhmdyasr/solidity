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

#include <libyul/backends/evm/OptimizedEVMCodeTransform.h>
#include <libyul/DataFlowGraph.h>

#include <libsolutil/Visitor.h>

#include <range/v3/view/enumerate.hpp>
#include <range/v3/view/reverse.hpp>

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

OptimizedCodeTransform::OptimizedCodeTransform(AbstractAssembly& _assembly, BuiltinContext& _builtinContext, bool _useNamedLabelsForFunctions):
m_assembly(_assembly), m_builtinContext(_builtinContext), m_useNamedLabelsForFunctions(_useNamedLabelsForFunctions)
{

}

void OptimizedCodeTransform::run(
	AbstractAssembly& _assembly,
	AsmAnalysisInfo& _analysisInfo,
	Block const& _block,
	EVMDialect const& _dialect,
	BuiltinContext& _builtinContext,
	ExternalIdentifierAccess const&,
	bool _useNamedLabelsForFunctions
)
{
	BasicBlock rootBlock;
	DataFlowGraphBuilder{_analysisInfo, _dialect, rootBlock}(_block);

	std::cout << std::endl << std::endl << "START CODEGEN" << std::endl << std::endl;

	std::map<Scope::Function const*, AbstractAssembly::LabelID> stagedFunctions;
	{
		OptimizedCodeTransform transform{_assembly, _builtinContext, _useNamedLabelsForFunctions};
		transform.visit(rootBlock);
		_assembly.appendInstruction(evmasm::Instruction::STOP);
		stagedFunctions = std::move(transform.m_neededFunctions);
	}
	std::set<Scope::Function const*> generatedFunctions;
	while (!stagedFunctions.empty())
	{
		auto [function, labelId] = *stagedFunctions.begin();
		stagedFunctions.erase(stagedFunctions.begin());
		generatedFunctions.emplace(function);

		OptimizedCodeTransform transform{_assembly, _builtinContext, _useNamedLabelsForFunctions};
		_assembly.appendLabel(labelId);
		FunctionInfo const& info = rootBlock.functions.at(function);
		transform.m_currentStack = info.entryLayout;
		_assembly.setStackHeight(static_cast<int>(info.entryLayout.size()));
		std::cout << "BEGIN FUNCTION " << function->name.str() << std::endl;
		transform.visit(*info.body);
		std::cout << "EXIT FUNCTION BODY OF " << function->name.str() << std::endl;
		transform.changeCurrentStackLayout(info.exitLayout);
		std::cout << "END FUNCTION " << function->name.str() << std::endl;
		_assembly.appendJump(0 /* TODO: -static_cast<int>(info.exitLayout.size()) */, AbstractAssembly::JumpType::OutOfFunction);
		_assembly.setStackHeight(0);

		for (auto&& [function, labelId]: transform.m_neededFunctions)
			if (!generatedFunctions.count(function))
				stagedFunctions.emplace(make_pair(function, labelId));
	}
}

void OptimizedCodeTransform::operator()(FunctionCallSlot const& _functionCall)
{
	AbstractAssembly::LabelID returnLabel;
	if (Scope::Function const* function = _functionCall.function())
	{
		returnLabel = m_assembly.newLabelId();
		m_assembly.appendLabelReference(returnLabel);
		for (auto const* argument: _functionCall.arguments())
			visit(*argument);
		AbstractAssembly::LabelID id;
		if (AbstractAssembly::LabelID* stagedId = util::valueOrNullptr(m_neededFunctions, function))
			id = *stagedId;
		else
		{
			id = m_useNamedLabelsForFunctions ?
				m_assembly.namedLabel(_functionCall.call().functionName.name.str()) :
				m_assembly.newLabelId();
			m_neededFunctions[function] = id;
		}

		m_assembly.appendJumpTo(
			m_neededFunctions[function],
			static_cast<int>(function->returns.size() - function->arguments.size()) - 1,
			AbstractAssembly::JumpType::IntoFunction
		);
		m_assembly.appendLabel(returnLabel);
		popLast(_functionCall.arguments().size());
		for(size_t i = 0; i < function->returns.size(); ++i)
			m_currentStack.push_back(m_currentExpressionNode);
	}
	else
	{
		BuiltinFunctionForEVM const* builtin = _functionCall.builtin();
		yulAssert(builtin, "");

		size_t nonLiteralArgumentCount = 0;
		for (auto&& [n, argument]: ranges::views::enumerate(_functionCall.arguments()))
			if (!builtin->literalArgument(n))
			{
				visit(*argument);
				++nonLiteralArgumentCount;
			}

		builtin->generateCode(_functionCall.call(), m_assembly, m_builtinContext, [](auto const&){});
		popLast(nonLiteralArgumentCount);
		for (size_t i = 0; i < builtin->returns.size(); ++i)
			m_currentStack.push_back(m_currentExpressionNode);
	}
}

void OptimizedCodeTransform::changeCurrentStackLayout(StackLayout const& _targetLayout)
{
	std::cout << "Stack shuffling:" << std::endl;
	std::cout << "From: ";
	PrintStackLayout(m_currentStack);
	std::cout << "To:   ";
	PrintStackLayout(_targetLayout);

	std::vector<int> stackLayout(m_currentStack.size(), -1);
	for(auto&& [targetPosition, targetElement]: ranges::views::enumerate(_targetLayout))
		for(auto&& [currentPosition, currentElement]: ranges::views::enumerate(m_currentStack))
			if (std::visit(util::GenericVisitor{
				[&](ReturnLabelSlot const&, ReturnLabelSlot const&) { return true; },
				[&](VariableSlot const& a, VariableSlot const& b)
				{
					return &a.variable() == &b.variable();
				},
				[](auto const&, auto const&) { return false; }
			}, targetElement->content, currentElement->content))
				stackLayout[currentPosition] = static_cast<int>(targetPosition);

	for (auto l: stackLayout)
		std::cout << l << " ";
	std::cout << std::endl;

	// TODO: better shuffling
	while (!stackLayout.empty() && stackLayout.back() != static_cast<int>(stackLayout.size() - 1))
		if (stackLayout.back() < 0)
		{
			m_assembly.appendInstruction(evmasm::Instruction::POP);
			stackLayout.pop_back();
		}
		else
		{
			m_assembly.appendInstruction(evmasm::swapInstruction(static_cast<unsigned>(stackLayout.size()) - static_cast<unsigned>(stackLayout.back())));
			swap(stackLayout[static_cast<size_t>(stackLayout.back())], stackLayout.back());
		}
}

void OptimizedCodeTransform::operator()(ExternalIdentifierSlot const&)
{
	yulAssert(false, "");
}
void OptimizedCodeTransform::operator()(LiteralSlot const& _literalNode)
{
	m_assembly.appendConstant(_literalNode.value());
	m_currentStack.emplace_back(m_currentExpressionNode);
}
void OptimizedCodeTransform::operator()(VariableSlot const& _variable)
{
	Scope::Variable const& var = _variable.variable();
	for (auto&& [depth, stackElement]: ranges::views::enumerate(m_currentStack | ranges::views::reverse))
		if (VariableSlot const* stackVar = std::get_if<VariableSlot>(&stackElement->content))
			if (&stackVar->variable() == &var)
			{
				m_assembly.appendInstruction(evmasm::dupInstruction(static_cast<unsigned>(depth + 1)));
				m_currentStack.emplace_back(m_currentExpressionNode);
				return;
			}

	yulAssert(false, "Variable not found on stack.");
}


void OptimizedCodeTransform::visit(BasicBlockEntry const& _basicBlock)
{
	if (_basicBlock.operation)
		visit(*_basicBlock.operation);
	std::cout << "Block transition:" << std::endl;
	std::cout << "  From: ";
	PrintStackLayout(m_currentStack);
	std::cout << "  To:   ";
	PrintStackLayout(_basicBlock.outputLayout);
	yulAssert(m_currentStack.size() == _basicBlock.outputLayout.size(), "");
	m_currentStack = _basicBlock.outputLayout;
}

void OptimizedCodeTransform::visit(StackSlot const& _expression)
{
	ScopedSaveAndRestore scopedSaveAndRestore(m_currentExpressionNode, &_expression);
	m_assembly.setSourceLocation(_expression.location);
	std::visit(*this, _expression.content);
}

void OptimizedCodeTransform::visit(BasicBlock const& _basicBlock)
{
	std::cout << "BEGIN OF BASIC BLOCK ";
	PrintStackLayout(m_currentStack);
	for (auto const& entry: _basicBlock.content)
		visit(entry);

	std::cout << "END OF BASIC BLOCK ";
	PrintStackLayout(m_currentStack);
	std::cout << "NUM EXITS: " << _basicBlock.exits.size() << std::endl;
}