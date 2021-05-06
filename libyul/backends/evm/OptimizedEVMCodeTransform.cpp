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

#include <range/v3/view/drop_last.hpp>
#include <range/v3/view/enumerate.hpp>
#include <range/v3/view/map.hpp>
#include <range/v3/view/reverse.hpp>
#include <range/v3/view/take.hpp>

using namespace solidity;
using namespace solidity::yul;
using namespace std;

OptimizedCodeTransform::OptimizedCodeTransform(
	OptimizedCodeTransformContext& _context,
	AbstractAssembly& _assembly,
	BuiltinContext& _builtinContext,
	bool _useNamedLabelsForFunctions):
m_context(_context),
m_assembly(_assembly),
m_builtinContext(_builtinContext),
m_useNamedLabelsForFunctions(_useNamedLabelsForFunctions)
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
	OptimizedCodeTransformContext context{
		DataFlowGraphBuilder::build(_analysisInfo, _dialect, _block),
		{},
		{},
		{},
		{}
	};
	{
		OptimizedCodeTransform codeTransform{
			context,
			_assembly,
			_builtinContext,
			_useNamedLabelsForFunctions
		};
		codeTransform(*context.dfg->entry);
		_assembly.appendInstruction(evmasm::Instruction::STOP);
	}
	while (!context.stagedFunctions.empty())
	{
		Scope::Function const* function = *context.stagedFunctions.begin();
		context.stagedFunctions.pop_front();

		DFG::FunctionInfo const& info = context.dfg->functions.at(function);
		OptimizedCodeTransform codeTransform{
			context,
			_assembly,
			_builtinContext,
			_useNamedLabelsForFunctions
		};
		_assembly.setStackHeight(1 + static_cast<int>(info.parameters.size()));
		_assembly.setSourceLocation(locationOf(info));
		_assembly.appendLabel(context.functionEntries.at(function));
		codeTransform.m_stack.emplace_back(ReturnLabelSlot{});
		codeTransform.m_stack += info.parameters | ranges::views::transform([](DFG::Variable const& _var) -> StackSlot {
			return VariableSlot{&_var};
		});
		codeTransform.m_unallocatedReturnVariables += info.returnVariables | ranges::views::transform([](DFG::Variable const& _var) {
			return _var.variable;
		});
		codeTransform(*info.entry);
		for (auto const& returnVar: info.returnVariables) // TODO: order?
			if (codeTransform.m_unallocatedReturnVariables.count(returnVar.variable))
			{
				_assembly.appendConstant(0);
				codeTransform.m_stack.emplace_back(VariableSlot{&returnVar});
			}
		std::cout << "LAYOUT BEFORE: " << stackToString(codeTransform.m_stack) << std::endl;
		vector<StackSlot> exitLayout = info.returnVariables | ranges::views::transform([](DFG::Variable const& _var) {
			return VariableSlot{&_var};
		}) | ranges::to<vector<StackSlot>>;
		exitLayout.emplace_back(ReturnLabelSlot{});
		codeTransform.shuffleStackTo(exitLayout);
		std::cout << "LAYOUT AFTER: " << stackToString(codeTransform.m_stack) << std::endl;
		_assembly.appendJump(-static_cast<int>(info.returnVariables.size()), AbstractAssembly::JumpType::OutOfFunction);
	}
}

void OptimizedCodeTransform::operator()(DFG::BasicBlock const& _block)
{
	for(DFG::Statement const& statement: _block.statements)
		std::visit(*this, statement);

	auto jump = [&](DFG::BasicBlock const* _target) {
		yulAssert(_target, "");
		yulAssert(_target->entries.size() >= 1, "");

		if (auto const* label = util::valueOrNullptr(m_jumpLabels, _target))
			m_assembly.appendJumpTo(*label, 0);
		else
		{
			if (_target->entries.size() > 1)
				m_assembly.appendLabel(m_jumpLabels[_target] = m_assembly.newLabelId());
			(*this)(*_target);
		}
	};
	std::visit(util::GenericVisitor{
		[](std::monostate) { std::cout << "MONOSTATE EXIT" << std::endl; },
		[&](DFG::BasicBlock::Jump const& _jump)
		{
			std::cout << "JUMP EXIT" << std::endl;
			jump(_jump.target);
		},
		[&](DFG::BasicBlock::ConditionalJump const& _conditonalJump)
		{
			std::visit(*this, _conditonalJump.condition);
			if (auto const* label = util::valueOrNullptr(m_jumpLabels, _conditonalJump.nonZero))
				m_assembly.appendJumpToIf(*label);
			else
			{
				m_assembly.appendJumpToIf(m_jumpLabels[_conditonalJump.nonZero] = m_assembly.newLabelId());
			}

			std::cout << "CONDITIONAL JUMP EXIT" << std::endl;
		},
		[](DFG::BasicBlock::FunctionReturn const&) { std::cout << "FUNCTION RETURN EXIT" << std::endl; },
		[](DFG::BasicBlock::Stop const&) { std::cout << "STOP EXIT" << std::endl; },
		[](DFG::BasicBlock::Revert const&) { std::cout << "REVERT EXIT" << std::endl; }
	}, _block.exit);
	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
}

void OptimizedCodeTransform::operator()(DFG::Declaration const& _declaration)
{
	m_assembly.setSourceLocation(locationOf(_declaration));

	if (_declaration.value)
		std::visit(*this, *_declaration.value);
	else
		for (size_t i = 0; i < _declaration.variables.size(); ++i)
		{
			m_assembly.appendConstant(0);
			m_stack.emplace_back(LiteralSlot{0});
		}

	pop(_declaration.variables.size());
	for (auto const& var: _declaration.variables)
		m_stack.emplace_back(VariableSlot{&var});

	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
}

void OptimizedCodeTransform::operator()(DFG::Assignment const& _assignment)
{
	m_assembly.setSourceLocation(locationOf(_assignment));
	for(DFG::Variable const& var: _assignment.variables | ranges::views::reverse)
		if (m_unallocatedReturnVariables.count(var.variable))
		{
			m_assembly.appendConstant(0);
			m_stack.push_back(VariableSlot{&var});
			m_unallocatedReturnVariables.erase(var.variable);
		}

	std::cout << " before assign: " << stackToString(m_stack) << std::endl;

	std::visit(*this, _assignment.value);
	for(DFG::Variable const& var: _assignment.variables | ranges::views::reverse)
	{
		m_assembly.appendInstruction(evmasm::swapInstruction(static_cast<unsigned>(variableStackDepth(var))));
		m_assembly.appendInstruction(evmasm::Instruction::POP);
		m_stack.pop_back();
	}

	std::cout << " after assign: " << stackToString(m_stack) << std::endl;
	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
}

void OptimizedCodeTransform::operator()(DFG::ExpressionStatement const& _stmt)
{
	m_assembly.setSourceLocation(locationOf(_stmt));
	std::visit(util::GenericVisitor{
		[&](DFG::FunctionCall const& _call) {
			size_t stackHeightBefore = m_stack.size();
			(*this)(_call);
			yulAssert(stackHeightBefore == m_stack.size(), "");
		},
		[&](DFG::BuiltinCall const& _call) {
			yulAssert(_call.returns == 0, "");
			size_t stackHeightBefore = m_stack.size();
			(*this)(_call);
			yulAssert(stackHeightBefore == m_stack.size(), "");
		},
		[&](auto const&) { yulAssert(false, "Expected function or builtin call."); }
	}, _stmt.expression);
	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
}

void OptimizedCodeTransform::operator()(DFG::BuiltinCall const& _builtinCall)
{
	m_assembly.setSourceLocation(locationOf(_builtinCall));

	for(auto const& argument: _builtinCall.arguments | ranges::views::reverse)
		std::visit(*this, argument);

	std::cout << _builtinCall.functionCall->functionName.name.str() << "  " << stackToString(m_stack) << std::endl;

	_builtinCall.builtin->generateCode(*_builtinCall.functionCall, m_assembly, m_builtinContext, [](auto&&){});
	pop(_builtinCall.arguments.size());
	for (size_t i = 0; i < _builtinCall.builtin->returns.size(); ++i)
		m_stack.emplace_back(TemporarySlot{&_builtinCall, i});

	std::cout << "  => " << stackToString(m_stack) << std::endl;
	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
}

void OptimizedCodeTransform::operator()(DFG::FunctionCall const& _functionCall)
{
	m_assembly.setSourceLocation(locationOf(_functionCall));
	AbstractAssembly::LabelID label = m_assembly.newLabelId();
	m_assembly.appendLabelReference(label);
	m_stack.emplace_back(ReturnLabelSlot{&_functionCall});

	for(auto const& argument: _functionCall.arguments | ranges::views::reverse)
		std::visit(*this, argument);

	std::cout << _functionCall.function->name.str() << "  " << stackToString(m_stack) << std::endl;

	m_assembly.appendJumpTo(
		getFunctionLabel(*_functionCall.function),
		static_cast<int>(_functionCall.function->returns.size() - _functionCall.function->arguments.size()) - 1,
		AbstractAssembly::JumpType::IntoFunction
	);
	m_assembly.appendLabel(label);
	pop(_functionCall.arguments.size() + 1);
	for (size_t i = 0; i < _functionCall.function->returns.size(); ++i)
		m_stack.emplace_back(TemporarySlot{&_functionCall, i});

	std::cout << "  => " << stackToString(m_stack) << std::endl;
	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
}

void OptimizedCodeTransform::operator()(DFG::Literal const& _literal)
{
	m_assembly.setSourceLocation(locationOf(_literal));
	m_assembly.appendConstant(_literal.value);
	m_stack.emplace_back(LiteralSlot{_literal.value});
	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
}

void OptimizedCodeTransform::operator()(DFG::Variable const& _variable)
{
	m_assembly.setSourceLocation(locationOf(_variable));
	size_t depth = variableStackDepth(_variable);
	m_assembly.appendInstruction(evmasm::dupInstruction(static_cast<unsigned>(depth + 1)));
	m_stack.emplace_back(VariableSlot{&_variable});
	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
}

AbstractAssembly::LabelID OptimizedCodeTransform::getFunctionLabel(Scope::Function const& _function)
{
	if (auto const* entry = util::valueOrNullptr(m_context.functionEntries, &_function))
		return *entry;
	AbstractAssembly::LabelID entry = m_context.functionEntries[&_function] = m_useNamedLabelsForFunctions ?
		m_assembly.namedLabel(_function.name.str(), _function.arguments.size(), _function.returns.size(), {}) :
		m_assembly.newLabelId();
	m_context.stagedFunctions.emplace_back(&_function);
	return entry;
}

size_t OptimizedCodeTransform::variableStackDepth(DFG::Variable const& _var)
{
	for (auto&& [depth, stackSlot]: (m_stack | ranges::views::reverse) | ranges::views::enumerate)
		if (VariableSlot* varSlot = get_if<VariableSlot>(&stackSlot))
			if (varSlot->variable->variable == _var.variable)
				return depth;
	yulAssert(false, "Variable not found on stack.");
}

void OptimizedCodeTransform::shuffleStackTo(std::vector<StackSlot> const& _target)
{
	auto matches = util::GenericVisitor{
		[](ReturnLabelSlot const& _r1, ReturnLabelSlot const& _r2) {
			return _r1.call == _r2.call;
		},
		[](VariableSlot const& _v1, VariableSlot const& _v2) {
			return _v1.variable->variable == _v2.variable->variable;
		},
		[](auto const&, auto const&) { return false; }
	};
	auto findSlot = [&](StackSlot const& _target) -> size_t {
		for (auto&& [depth, slot]: ranges::views::enumerate(m_stack | ranges::views::reverse))
			if (std::visit(matches, _target, slot))
				return depth;
		yulAssert(false, "Slot not found on stack.");
	};

	yulAssert(m_stack.size() >= _target.size(), "");
	// TODO: take into account that we might be able to pop the top early.
	for (auto&& [idx, slots]: ranges::zip_view(m_stack | ranges::views::take(_target.size()), _target) | ranges::views::enumerate)
	{
		size_t targetDepth = m_stack.size() - idx - 1;
		auto&& [currentSlot, targetSlot] = slots;

		if (!std::visit(matches, currentSlot, targetSlot))
		{
			size_t sourceDepth = findSlot(targetSlot);
			if (sourceDepth > 0)
			{
				std::swap(m_stack.back(), m_stack[m_stack.size() - sourceDepth - 1]);
				m_assembly.appendInstruction(evmasm::swapInstruction(static_cast<unsigned>(sourceDepth)));
			}
			if (targetDepth > 0)
			{
				std::swap(m_stack.back(), m_stack[m_stack.size() - targetDepth - 1]);
				m_assembly.appendInstruction(evmasm::swapInstruction(static_cast<unsigned>(targetDepth)));
			}
		}
	}
	pop(m_stack.size() - _target.size(), true);
	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
}

string OptimizedCodeTransform::stackSlotToString(StackSlot const& _slot)
{
	return std::visit(util::GenericVisitor{
		[](ReturnLabelSlot const&) -> string { return "RET"; },
		[](VariableSlot const& _var) { return _var.variable->variable->name.str() + "(" + to_string(intptr_t(_var.variable->variable)) + ")"; },
		[](LiteralSlot const& _lit) { return util::toCompactHexWithPrefix(_lit.value); },
		[](TemporarySlot const&) -> string { return "TMP"; }
	}, _slot);
}

string OptimizedCodeTransform::stackToString(vector<StackSlot> const& _stack)
{
	string result("[ ");
	for (auto const& slot: _stack)
		result += stackSlotToString(slot) + ' ';
	result += ']';
	return result;
}