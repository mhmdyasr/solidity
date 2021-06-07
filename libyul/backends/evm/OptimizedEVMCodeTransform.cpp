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
#include <libyul/Utilities.h>

#include <libsolutil/Permutations.h>
#include <libsolutil/Visitor.h>
#include <libsolutil/cxx20.h>

#include <range/v3/view/drop.hpp>
#include <range/v3/view/drop_last.hpp>
#include <range/v3/view/enumerate.hpp>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/iota.hpp>
#include <range/v3/view/map.hpp>
#include <range/v3/view/reverse.hpp>
#include <range/v3/view/take.hpp>
#include <range/v3/view/take_last.hpp>

using namespace solidity;
using namespace solidity::yul;
using namespace std;

namespace
{
struct PreviousSlot { size_t slot; };

#if 1
#define DEBUG(x) x
string stackSlotToString(StackSlot const& _slot)
{
	return std::visit(util::GenericVisitor{
		[](ReturnLabelSlot const& _ret) -> string { return "RET[" + (_ret.callID ? to_string(*_ret.callID) : "") + "]"; },
		[](VariableSlot const& _var) { return _var.variable->name.str(); },
		[](LiteralSlot const& _lit) { return util::toCompactHexWithPrefix(_lit.value); },
		[](TemporarySlot const& _tmp) -> string { return "TMP[" + to_string(_tmp.callID) + ", " + to_string(_tmp.idx) + "]"; },
		[](JunkSlot const&) -> string { return "JUNK"; }
	}, _slot);
}

string stackToString(Stack const& _stack)
{
	string result("[ ");
	for (auto const& slot: _stack)
		result += stackSlotToString(slot) + ' ';
	result += ']';
	return result;
}
#else
#define DEBUG(x) (void)0;
#endif

template<typename Range, typename Value>
set<unsigned> findAllOffsets(Range&& _range, Value&& _value)
{
	set<unsigned> result;
	auto begin = std::begin(_range);
	auto end = std::end(_range);
	auto it = begin;
	while (it != end)
	{
		it = std::find(it, end, std::forward<Value>(_value));
		if (it == end)
			return result;
		result.emplace(static_cast<unsigned>(std::distance(begin, it)));
		++it;
	}
	return result;
}

template<typename Swap, typename Dup, typename Pop, typename PushSlot>
void createStackLayout(Stack& _currentStack, Stack const& _targetStack, Swap _swap, Dup _dup, PushSlot _push, Pop _pop, bool _silent = false)
{
	_silent = true;
	if (_currentStack == _targetStack)
		return;
	if (!_silent)
		DEBUG(cout << "CREATE STACK LAYOUT: " << stackToString(_targetStack) << " FROM " << stackToString(_currentStack) << std::endl;)

	if (_currentStack.empty())
	{
		while(_currentStack.size() < _targetStack.size())
		{
			StackSlot newSlot = _targetStack.at(_currentStack.size());
			_push(newSlot);
			_currentStack.emplace_back(newSlot);
		}
		yulAssert(_currentStack == _targetStack, "");
		return;
	}

	auto topTargets = findAllOffsets(_targetStack, _currentStack.back());
	if (topTargets.size() < findAllOffsets(_currentStack, _currentStack.back()).size())
	{
		if (!_silent)
			DEBUG(cout << "POP TOP" << std::endl;)
		_pop();
		_currentStack.pop_back();
		createStackLayout(_currentStack, _targetStack, _swap, _dup, _push, _pop, _silent);
		return;
	}
	else if (_targetStack.size() >= _currentStack.size() && _targetStack.at(_currentStack.size() - 1) == _currentStack.back())
	{
		if (!_silent)
			DEBUG(cout << "TOP is in place" << std::endl;)

		// Current top is in place.
		// Dup deepest one to be dupped (TODO: choose optimal).
		for(auto&& [offset, slot]: _currentStack | ranges::views::enumerate)
		{
			if (findAllOffsets(_currentStack, slot).size() < findAllOffsets(_targetStack, slot).size())
			{
				if (!_silent)
					DEBUG(cout << "DUP" << std::endl;)

				auto leastDeepOccurrence = util::findOffset(_currentStack | ranges::views::reverse, slot);
				yulAssert(leastDeepOccurrence, "");
				_dup(static_cast<unsigned>(*leastDeepOccurrence + 1));
				//_dup(static_cast<unsigned>(_currentStack.size() - offset));

				_currentStack.emplace_back(_currentStack.at(offset));
				createStackLayout(_currentStack, _targetStack, _swap, _dup, _push, _pop, _silent);
				return;
			}
		}
		// Nothing to dup. Find anything to be pushed and push it.
		for(auto const& slot: _targetStack)
		{
			if (!util::findOffset(_currentStack, slot))
			{
				if (!_silent)
					DEBUG(cout << "PUSH" << std::endl;)
				_push(slot);
				_currentStack.emplace_back(slot);
				createStackLayout(_currentStack, _targetStack, _swap, _dup, _push, _pop, _silent);
				return;
			}
		}
		// Nothing to push or dup.
		// Swap the deepest one that's not in place up.
		for (auto&& [offset, slot]: _currentStack | ranges::views::enumerate)
		{
			if (!(slot == _targetStack.at(offset)) && !(slot == _currentStack.back()))
			{
				if (!_silent)
					DEBUG(cout << "SWAP " << offset << std::endl;)
				_swap(static_cast<unsigned>(_currentStack.size() - offset - 1));
				std::swap(_currentStack.back(), _currentStack.at(offset));
				createStackLayout(_currentStack, _targetStack, _swap, _dup, _push, _pop, _silent);
				return;
			}
		}
		// Nothing to push or dup and nothing out of place => done.
		yulAssert(_currentStack == _targetStack, "");
		return;
	}
	else
	{
		if (!_silent)
			DEBUG(cout << "TOP is not in place" << std::endl;)

		for (unsigned deepestTopTarget: topTargets)
		{
			if (deepestTopTarget >= _currentStack.size())
				break;
			if (!(_currentStack.at(deepestTopTarget) == _targetStack.at(deepestTopTarget)))
			{
				if (!_silent)
					DEBUG(cout << "Move into place " << deepestTopTarget << std::endl;)

				// Move top into place.
				_swap(static_cast<unsigned>(_currentStack.size() - deepestTopTarget - 1));
				std::swap(_currentStack.back(), _currentStack.at(deepestTopTarget));
				createStackLayout(_currentStack, _targetStack, _swap, _dup, _push, _pop, _silent);
				return;
			}
		}

		// There needs to be something to dup or push. Try dupping. (TODO: suboptimal)
		for(auto&& [offset, slot]: _currentStack | ranges::views::enumerate)
		{
			if (findAllOffsets(_currentStack, slot).size() < findAllOffsets(_targetStack, slot).size())
			{
				if (!_silent)
					DEBUG(cout << "DUP " << offset << std::endl;)

				auto leastDeepOccurrence = util::findOffset(_currentStack | ranges::views::reverse, slot);
				yulAssert(leastDeepOccurrence, "");
				_dup(static_cast<unsigned>(*leastDeepOccurrence + 1));
				// _dup(static_cast<unsigned>(_currentStack.size() - offset));

				_currentStack.emplace_back(_currentStack.at(offset));
				createStackLayout(_currentStack, _targetStack, _swap, _dup, _push, _pop, _silent);
				return;
			}
		}
		// Nothing to dup. Find anything to be pushed and push it.
		for(auto const& slot: _targetStack)
		{
			if (!util::findOffset(_currentStack, slot))
			{
				if (!_silent)
					DEBUG(cout << "PUSH" << std::endl;)
				_push(slot);
				_currentStack.template emplace_back(slot);
				createStackLayout(_currentStack, _targetStack, _swap, _dup, _push, _pop, _silent);
				return;
			}
		}
		yulAssert(false, "");
	}

	if (!_silent)
		DEBUG(cout << "CREATED STACK LAYOUT: " << stackToString(_currentStack) << std::endl;)
	yulAssert(_currentStack == _targetStack, "");

}

}

class CodeGenerator
{
public:
	static void run(
		AbstractAssembly& _assembly,
		BuiltinContext& _builtinContext,
		bool _useNamedLabelsForFunctions,
		OptimizedCodeTransformContext const& _info,
		DFG::BasicBlock const& _entry
	)
	{
		CodeGenerator generator(_assembly, _builtinContext, _useNamedLabelsForFunctions,  _info);
		generator(_entry);
		generator.generateStaged();

	}
private:
	CodeGenerator(
		AbstractAssembly& _assembly,
		BuiltinContext& _builtinContext,
		bool _useNamedLabelsForFunctions,
		OptimizedCodeTransformContext const& _info
	):
	m_assembly(_assembly),
	m_builtinContext(_builtinContext),
	m_useNamedLabelsForFunctions(_useNamedLabelsForFunctions),
	m_info(_info)
	{
	}
public:

	AbstractAssembly::LabelID getFunctionLabel(Scope::Function const& _function)
	{
		ScopedSaveAndRestore restoreStack(m_stack, {});
		DFG::FunctionInfo const& functionInfo = m_info.dfg->functions.at(&_function);
		if (!m_functionLabels.count(&functionInfo))
		{
			m_functionLabels[&functionInfo] = m_useNamedLabelsForFunctions ?
				m_assembly.namedLabel(
					functionInfo.function->name.str(),
					functionInfo.function->arguments.size(),
					functionInfo.function->returns.size(),
					{}
				) : m_assembly.newLabelId();

			m_stagedFunctions.emplace_back(&functionInfo);
		}
		return m_functionLabels[&functionInfo];
	}

	void operator()(DFG::FunctionInfo const& _functionInfo)
	{
		yulAssert(!m_currentFunctionInfo, "");
		m_currentFunctionInfo = &_functionInfo;

		BlockGenerationInfo const& info = m_info.blockInfos.at(_functionInfo.entry);

		DEBUG(cout << std::endl;)
		DEBUG(cout << "F: start of function " << _functionInfo.function->name.str() << std::endl;)
		m_stack.clear();
		m_stack.emplace_back(ReturnLabelSlot{});
		for (auto const& param: _functionInfo.parameters | ranges::views::reverse)
			m_stack.emplace_back(param);
		m_assembly.setStackHeight(static_cast<int>(m_stack.size()));
		m_assembly.setSourceLocation(locationOf(_functionInfo));
		yulAssert(m_functionLabels.count(&_functionInfo), "");

		m_assembly.appendLabel(m_functionLabels.at(&_functionInfo));
		createStackLayout(info.entryLayout);

		(*this)(*_functionInfo.entry);

		m_currentFunctionInfo = nullptr;
	}

	void validateSlot(StackSlot const& _slot, Expression const& _expression)
	{
		std::visit(util::GenericVisitor{
			[&](yul::Literal const& _literal) {
				auto* literalSlot = get_if<LiteralSlot>(&_slot);
				yulAssert(literalSlot && valueOfLiteral(_literal) == literalSlot->value, "");
			},
			[&](yul::Identifier const& _identifier) {
				auto* variableSlot = get_if<VariableSlot>(&_slot);
				yulAssert(variableSlot && variableSlot->variable->name == _identifier.name, "");
			},
			[&](yul::FunctionCall const& _call) {
				auto* temporarySlot = get_if<TemporarySlot>(&_slot);
				yulAssert(temporarySlot && m_info.dfg->functionCallsByID.at(temporarySlot->callID) == &_call, "");
			}
		}, _expression);
	}

	void operator()(DFG::FunctionCall const& _call)
	{
		// Assert that we got a correct stack for the call.
		for (auto&& [arg, slot]: ranges::zip_view(
			_call.functionCall->arguments | ranges::views::reverse,
			m_stack | ranges::views::take_last(_call.functionCall->arguments.size())
		))
			validateSlot(slot, arg);

		auto entryLabel = getFunctionLabel(*_call.function);
		DEBUG(cout << "F: function call " << _call.functionCall->functionName.name.str() << " pre: " << stackToString(m_stack) << std::endl;)
		m_assembly.setSourceLocation(locationOf(_call));
		m_assembly.appendJumpTo(
			entryLabel,
			static_cast<int>(_call.function->returns.size() - _call.function->arguments.size()) - 1,
			AbstractAssembly::JumpType::IntoFunction
		);
		m_assembly.appendLabel(m_returnLabels.at(_call.functionCall));
		for (size_t i = 0; i < _call.function->arguments.size() + 1; ++i)
			m_stack.pop_back();
		for (size_t i = 0; i < _call.function->returns.size(); ++i)
			m_stack.emplace_back(TemporarySlot{_call.functionCallID, i});
		DEBUG(cout << "F: function call " << _call.functionCall->functionName.name.str() << " post: " << stackToString(m_stack) << std::endl;)
	}

	void operator()(DFG::BuiltinCall const& _call)
	{
		// Assert that we got a correct stack for the call.
		for (auto&& [arg, slot]: ranges::zip_view(
			_call.functionCall->arguments | ranges::views::enumerate |
			ranges::views::filter(util::mapTuple([&](size_t idx, auto&) -> bool { return !_call.builtin->literalArgument(idx); })) |
			ranges::views::reverse | ranges::views::values,
			m_stack | ranges::views::take_last(_call.arguments)
		))
			validateSlot(slot, arg);

		DEBUG(cout << "F: builtin call " << _call.functionCall->functionName.name.str() << " pre: " << stackToString(m_stack) << std::endl;)
		m_assembly.setSourceLocation(locationOf(_call));
		_call.builtin->generateCode(*_call.functionCall, m_assembly, m_builtinContext, [](auto&&){});
		for (size_t i = 0; i < _call.arguments; ++i)
			m_stack.pop_back();
		for (size_t i = 0; i < _call.builtin->returns.size(); ++i)
			m_stack.emplace_back(TemporarySlot{_call.functionCallID, i});
		DEBUG(cout << "F: builtin call " << _call.functionCall->functionName.name.str() << " post: " << stackToString(m_stack) << std::endl;)
	}

	void operator()(DFG::Assignment const& _assignment)
	{
		m_assembly.setSourceLocation(locationOf(_assignment));
		DEBUG(cout << "F: assign (";)
		for (auto var: _assignment.variables)
			DEBUG(cout << var.variable->name.str() << " ";)
		DEBUG(cout << ") pre: " << stackToString(m_stack) << std::endl;)

		for(auto& currentSlot: m_stack)
			if (VariableSlot const* varSlot = get_if<VariableSlot>(&currentSlot))
				if (util::findOffset(_assignment.variables, *varSlot))
					currentSlot = JunkSlot{};

		for (auto&& [currentSlot, varSlot]: ranges::zip_view(m_stack | ranges::views::take_last(_assignment.variables.size()), _assignment.variables))
			currentSlot = varSlot;

		DEBUG(cout << "F: assign (";)
		for (auto var: _assignment.variables)
			DEBUG(cout << var.variable->name.str() << " ";)
		DEBUG(cout << ") post: " << stackToString(m_stack) << std::endl;)
	}

	void operator()(DFG::BasicBlock const& _block)
	{
		if (m_generated.count(&_block))
			return;
		m_generated.insert(&_block);

		BlockGenerationInfo const& info = m_info.blockInfos.at(&_block);

		if (auto label = util::valueOrNullptr(m_blockLabels, &_block))
			m_assembly.appendLabel(*label);

		{
			auto label = util::valueOrNullptr(m_blockLabels, &_block);
			(void)label;
			DEBUG(cout << "F: GENERATING: " << &_block << " (label: " << (label ? std::to_string(*label) : "NONE") << ")" << std::endl;)
		}

		yulAssert(m_stack == info.entryLayout, "");

		DEBUG(cout << "F: CREATING ENTRY LAYOUT " << stackToString(info.entryLayout) << " FROM " << stackToString(m_stack) << std::endl;)
		createStackLayout(info.entryLayout);

		for (auto const& operation: _block.operations)
		{
			OptimizedCodeTransformContext::OperationInfo const& operationInfo = m_info.operationStacks.at(&operation);
			createStackLayout(operationInfo.entryStack);
			std::visit(*this, operation.operation);
			// TODO: is this actually necessary each time? Last time is probably enough, if at all needed.
			// createStackLayout(operationInfo.exitStack);
		}
		createStackLayout(info.exitLayout);

		DEBUG(cout << std::endl << std::endl;)
		DEBUG(cout << "F: EXIT LAYOUT (" << &_block << "): " << stackToString(info.exitLayout) << " == " << stackToString(m_stack) << std::endl;)
		// TODO: conditions!
		//		yulAssert(info.exitLayout == m_stack, "");


		std::visit(util::GenericVisitor{
			[&](std::monostate)
			{
				DEBUG(cout << "F: MAIN EXIT" << std::endl;)
				m_assembly.appendInstruction(evmasm::Instruction::STOP);
			},
			[&](DFG::BasicBlock::Jump const& _jump)
			{
				DEBUG(cout << "F: JUMP EXIT TO: " << _jump.target << std::endl;)

				BlockGenerationInfo const& targetInfo = m_info.blockInfos.at(_jump.target);
				DEBUG(cout << "F: CURRENT " << stackToString(m_stack) << " => " << stackToString(targetInfo.entryLayout) << std::endl;)
				createStackLayout(targetInfo.entryLayout);
				/*
				 Actually this should be done, but since the stack shuffling doesn't allow anything for Junk slots, but explicitly "creates"
				 them this actually *costs* currently:
				 Similarly for the conditional case.
				 Probably even better to do it when assigning the entry layouts.
				createStackLayout(targetInfo.entryLayout | ranges::views::transform([&](StackSlot const& _slot) -> StackSlot {
					if (!_jump.target->operations.empty())
					{
						OptimizedCodeTransformContext::OperationInfo const& operationInfo = m_info.operationStacks.at(&_jump.target->operations.front());
						if (!util::findOffset(operationInfo.entryStack, _slot))
							return JunkSlot{};
					}
					return _slot;
				}) | ranges::to<Stack>);
				m_stack = targetInfo.entryLayout;
				 */

				if (!m_blockLabels.count(_jump.target) && _jump.target->entries.size() == 1)
					(*this)(*_jump.target);
				else
				{
					if (!m_blockLabels.count(_jump.target))
						m_blockLabels[_jump.target] = m_assembly.newLabelId();

					yulAssert(m_stack == m_info.blockInfos.at(_jump.target).entryLayout, "");
					m_assembly.appendJumpTo(m_blockLabels[_jump.target]);
					if (!m_generated.count(_jump.target))
						m_stagedBlocks.emplace_back(_jump.target);
				}
			},
			[&](DFG::BasicBlock::ConditionalJump const& _conditionalJump)
			{
				DEBUG(cout << "F: CONDITIONAL JUMP EXIT TO: " << _conditionalJump.nonZero << " / " << _conditionalJump.zero << std::endl;)
				DEBUG(cout << "F: CURRENT EXIT LAYOUT: " << stackToString(info.exitLayout) << std::endl;)
				BlockGenerationInfo const& nonZeroInfo = m_info.blockInfos.at(_conditionalJump.nonZero);
				(void)nonZeroInfo;
				BlockGenerationInfo const& zeroInfo = m_info.blockInfos.at(_conditionalJump.zero);
				(void)zeroInfo;
				DEBUG(cout << "F: non-zero entry layout: " << stackToString(nonZeroInfo.entryLayout) << std::endl;)
				DEBUG(cout << "F: zero entry layout: " << stackToString(zeroInfo.entryLayout) << std::endl;)

				for (auto const* nonZeroEntry: _conditionalJump.nonZero->entries)
				{
					BlockGenerationInfo const& entryInfo = m_info.blockInfos.at(nonZeroEntry);
					(void)entryInfo;
					DEBUG(cout << "  F: non-zero entry exit: " << stackToString(entryInfo.exitLayout) << std::endl;)
				}
				for (auto const* zeroEntry: _conditionalJump.zero->entries)
				{
					BlockGenerationInfo const& entryInfo = m_info.blockInfos.at(zeroEntry);
					(void)entryInfo;
					DEBUG(cout << "  F: zero entry exit: " << stackToString(entryInfo.exitLayout) << std::endl;)
				}
/*
 * TODO!
				yulAssert(nonZeroInfo.entryLayout == zeroInfo.entryLayout, "");
				yulAssert((m_stack | ranges::views::drop_last(1) | ranges::to<Stack>) == nonZeroInfo.entryLayout, "");
*/
				if (!m_blockLabels.count(_conditionalJump.nonZero))
					m_blockLabels[_conditionalJump.nonZero] = m_assembly.newLabelId();
				m_assembly.appendJumpToIf(m_blockLabels[_conditionalJump.nonZero]);
				m_stack.pop_back();
				// TODO: assert this?
				yulAssert(m_stack == m_info.blockInfos.at(_conditionalJump.nonZero).entryLayout, "");
				yulAssert(m_stack == m_info.blockInfos.at(_conditionalJump.zero).entryLayout, "");

				if (!m_generated.count(_conditionalJump.nonZero))
					m_stagedBlocks.emplace_back(_conditionalJump.nonZero);

				if (!m_blockLabels.count(_conditionalJump.zero))
					m_blockLabels[_conditionalJump.zero] = m_assembly.newLabelId();
				if (m_generated.count(_conditionalJump.zero))
					m_assembly.appendJumpTo(m_blockLabels[_conditionalJump.zero]);
				else
					(*this)(*_conditionalJump.zero);
			},
			[&](DFG::BasicBlock::FunctionReturn const& _functionReturn)
			{
				yulAssert(m_currentFunctionInfo == _functionReturn.info, "");
				DEBUG(cout << "F: Function return exit: " << _functionReturn.info->function->name.str() << std::endl;)

				yulAssert(m_currentFunctionInfo, "");
				Stack exitStack = m_currentFunctionInfo->returnVariables | ranges::views::transform([](auto const& _varSlot){
					return StackSlot{_varSlot};
				}) | ranges::to<Stack>;
				exitStack.emplace_back(ReturnLabelSlot{});

				DEBUG(cout << "Return from function " << m_currentFunctionInfo->function->name.str() << std::endl;)
				DEBUG(cout << "EXIT STACK: " << stackToString(exitStack) << std::endl;)
				createStackLayout(exitStack);
				m_assembly.setSourceLocation(locationOf(*m_currentFunctionInfo));
				m_assembly.appendJump(0, AbstractAssembly::JumpType::OutOfFunction); // TODO: stack height diff.
				m_assembly.setStackHeight(0);
				m_stack.clear();

			},
			[&](DFG::BasicBlock::Terminated const&)
			{
				DEBUG(cout << "F: TERMINATED" << std::endl;)
			}
		}, _block.exit);
	}

	bool tryCreateStackLayout(Stack _targetStack)
	{
		Stack commonPrefix;
		for (auto&& [slot1, slot2]: ranges::zip_view(m_stack, _targetStack))
		{
			if (!(slot1 == slot2))
				break;
			commonPrefix.emplace_back(slot1);
		}
		Stack temporaryStack = m_stack | ranges::views::drop(commonPrefix.size()) | ranges::to<Stack>;

		bool good = true;
		::createStackLayout(temporaryStack, _targetStack  | ranges::views::drop(commonPrefix.size()) | ranges::to<Stack>, [&](unsigned _i) {
			if (_i > 16)
				good = false;
		}, [&](unsigned _i) {
			if (_i > 16)
				good = false;
		}, [&](StackSlot const& _slot) {
			Stack currentFullStack = commonPrefix;
			for (auto slot: temporaryStack)
				currentFullStack.emplace_back(slot);
			if (auto depth = util::findOffset(currentFullStack | ranges::views::reverse, _slot))
			{
				if (*depth + 1 > 16)
					good = false;
				return;
			}
		}, [&]() {});
		return good;
	}

	void compressStack()
	{
		DEBUG(std::cout << "COMPRESS STACK" << std::endl;)
		static constexpr auto canBeRegenerated = [](StackSlot const& _slot) -> bool {
			if (auto* returnSlot = get_if<ReturnLabelSlot>(&_slot))
				if (returnSlot->callID)
					return true;
			if (holds_alternative<LiteralSlot>(_slot))
				return true;
			return false;
		};
		while (!m_stack.empty())
		{
			auto top = m_stack.back();
			if (canBeRegenerated(top))
			{
				m_assembly.appendInstruction(evmasm::Instruction::POP);
				m_stack.pop_back();
				continue;
			}
			if (auto offset = util::findOffset(m_stack, top))
				if (*offset < m_stack.size() - 1)
				{
					m_assembly.appendInstruction(evmasm::Instruction::POP);
					m_stack.pop_back();
					continue;
				}

			size_t topSize = m_stack.size() > 16 ? 16 : m_stack.size();
			for (auto&& [offset, slot]: (m_stack | ranges::views::take_last(topSize)) | ranges::views::enumerate)
			{
				if (offset == topSize - 1)
					return;
				if (canBeRegenerated(slot))
				{
					std::swap(m_stack.back(), m_stack.at(m_stack.size() - topSize + offset));
					m_assembly.appendInstruction(evmasm::swapInstruction(static_cast<unsigned>(topSize - 1 - offset)));
					m_stack.pop_back();
					m_assembly.appendInstruction(evmasm::Instruction::POP);
					break;
				}
			}
		}
	}

	void createStackLayout(Stack _targetStack)
	{
		DEBUG(cout << "F: CREATE " << stackToString(_targetStack) << " FROM " << stackToString(m_stack) << std::endl;)

		Stack commonPrefix;
		for (auto&& [slot1, slot2]: ranges::zip_view(m_stack, _targetStack))
		{
			if (!(slot1 == slot2))
				break;
			commonPrefix.emplace_back(slot1);
		}

		Stack temporaryStack = m_stack | ranges::views::drop(commonPrefix.size()) | ranges::to<Stack>;

		if (!tryCreateStackLayout(_targetStack))
		{
			// TODO: check if we can do better.
			// Maybe switching to a general "fix everything deep first" algorithm.
			std::map<unsigned, StackSlot> slotsByDepth;
			for (auto slot: _targetStack | ranges::views::take_last(_targetStack.size() - commonPrefix.size()))
			{
				auto offset = util::findOffset(m_stack | ranges::views::reverse | ranges::to<Stack>, slot);
				if (offset)
					slotsByDepth.insert(std::make_pair(*offset, slot));
			}
			for (auto slot: slotsByDepth | ranges::views::reverse | ranges::views::values)
			{
				if (!util::findOffset(temporaryStack, slot))
				{
					auto offset = util::findOffset(m_stack | ranges::views::reverse | ranges::to<Stack>, slot);
					m_stack.emplace_back(slot);
					DEBUG(
						if (*offset + 1 > 16)
							std::cout << "Cannot reach slot: " << stackSlotToString(slot) << std::endl;
					)
					m_assembly.appendInstruction(evmasm::dupInstruction(static_cast<unsigned>(*offset + 1)));
				}
			}

			temporaryStack = m_stack | ranges::views::drop(commonPrefix.size()) | ranges::to<Stack>;
		}


		DEBUG(cout << "F: CREATE " << stackToString(_targetStack) << " FROM " << stackToString(m_stack) << std::endl;)
		DEBUG(if (!commonPrefix.empty())
			cout << "   (USE " << stackToString(_targetStack | ranges::views::drop(commonPrefix.size()) | ranges::to<Stack>) << " FROM " << stackToString(temporaryStack) << std::endl;)
		::createStackLayout(temporaryStack, _targetStack  | ranges::views::drop(commonPrefix.size()) | ranges::to<Stack>, [&](unsigned _i) {
			m_assembly.appendInstruction(evmasm::swapInstruction(_i));
		}, [&](unsigned _i) {
			m_assembly.appendInstruction(evmasm::dupInstruction(_i));
		}, [&](StackSlot const& _slot) {
			Stack currentFullStack = commonPrefix;
			for (auto slot: temporaryStack)
				currentFullStack.emplace_back(slot);
			if (auto depth = util::findOffset(currentFullStack | ranges::views::reverse, _slot))
			{
				m_assembly.appendInstruction(evmasm::dupInstruction(static_cast<unsigned>(*depth + 1)));
				return;
			}
			std::visit(util::GenericVisitor{
				[&](LiteralSlot const &_literal)
				{
					m_assembly.setSourceLocation(locationOf(_literal));
					m_assembly.appendConstant(_literal.value);
				},
				[&](ReturnLabelSlot const& _returnLabel)
				{
					yulAssert(_returnLabel.callID, "Cannot produce function return label.");
					yul::FunctionCall const* call = m_info.dfg->functionCallsByID.at(*_returnLabel.callID);
					// TODO: maybe actually use IDs to index into returnLabels.
					if (!m_returnLabels.count(call))
						m_returnLabels[call] = m_assembly.newLabelId();
					m_assembly.appendLabelReference(m_returnLabels.at(call));
				},
				[&](VariableSlot const& _variable)
				{
					if (m_currentFunctionInfo)
						if (util::contains(m_currentFunctionInfo->returnVariables, _variable))
						{
							// TODO: maybe track uninitialized return variables.
							m_assembly.appendConstant(0);
							return;
						}
					DEBUG(cout << "SLOT: " << stackSlotToString(StackSlot{_slot}) << std::endl;)
					DEBUG(cout << "THIS SHOULD NOT HAPPEN!" << std::endl;)
					// Also on control flow joins some slots are not correctly marked as junk slots and end up here so far.
					m_assembly.appendInstruction(evmasm::Instruction::CALLDATASIZE);
				},
				[&](TemporarySlot const&)
				{
					yulAssert(false, "");
				},
				[&](JunkSlot const&)
				{
					m_assembly.appendInstruction(evmasm::Instruction::CALLDATASIZE);
				},
				[&](auto const& _slot)
				{
					DEBUG(cout << "SLOT: " << stackSlotToString(StackSlot{_slot}) << std::endl;)
					DEBUG(cout << "THIS SHOULD NOT HAPPEN!" << std::endl;) // Actually it appears to happen for uninitialized return variables.
					// Also on control flow joins some slots are not correctly marked as junk slots and end up here so far.
					// TODO: Both should be fixed.
					m_assembly.appendInstruction(evmasm::Instruction::CALLDATASIZE);
					//yulAssert(false, "Slot not found.");
				}
			}, _slot);
		}, [&]() { m_assembly.appendInstruction(evmasm::Instruction::POP); });
		m_stack = commonPrefix;
		for (auto slot: temporaryStack)
			m_stack.emplace_back(slot);
	}
private:

	void generateStaged()
	{
		while (!m_stagedBlocks.empty())
		{
			DFG::BasicBlock const* _block = *m_stagedBlocks.begin();
			m_stagedBlocks.pop_front();
			m_stack = m_info.blockInfos.at(_block).entryLayout;
			m_assembly.setStackHeight(static_cast<int>(m_stack.size()));
			(*this)(*_block);
			// TODO: assert that this block jumped at exit.
		}
		while (!m_stagedFunctions.empty())
		{
			while (!m_stagedFunctions.empty())
			{
				DFG::FunctionInfo const* _functionInfo = *m_stagedFunctions.begin();
				m_stagedFunctions.pop_front();
				if (!m_generatedFunctions.count(_functionInfo))
				{
					m_generatedFunctions.emplace(_functionInfo);
					(*this)(*_functionInfo);
				}
				yulAssert(!m_currentFunctionInfo, "");
				m_currentFunctionInfo = _functionInfo;
				while (!m_stagedBlocks.empty())
				{
					DFG::BasicBlock const* _block = *m_stagedBlocks.begin();
					m_stagedBlocks.pop_front();
					m_stack = m_info.blockInfos.at(_block).entryLayout;
					m_assembly.setStackHeight(static_cast<int>(m_stack.size()));
					(*this)(*_block);
					// TODO: assert that this block jumped at exit.
				}
				m_currentFunctionInfo = nullptr;
			}
		}
	}

	AbstractAssembly& m_assembly;
	BuiltinContext& m_builtinContext;
	bool m_useNamedLabelsForFunctions = true;
	OptimizedCodeTransformContext const& m_info;
	Stack m_stack;
	std::map<yul::FunctionCall const*, AbstractAssembly::LabelID> m_returnLabels;
	std::map<DFG::BasicBlock const*, AbstractAssembly::LabelID> m_blockLabels;
	std::map<DFG::FunctionInfo const*, AbstractAssembly::LabelID> m_functionLabels;
	std::set<DFG::BasicBlock const*> m_generated;
	std::deque<DFG::BasicBlock const*> m_stagedBlocks;
	std::list<DFG::FunctionInfo const*> m_stagedFunctions;
	std::set<DFG::FunctionInfo const*> m_generatedFunctions;
	DFG::FunctionInfo const* m_currentFunctionInfo = nullptr;
};

class StackLayoutGenerator
{
public:
	StackLayoutGenerator(OptimizedCodeTransformContext& _context);

	Stack operator()(DFG::BasicBlock const& _block, Stack _initialExitLayout);

	void operator()(DFG::Operation const& _operation);

	void operator()(DFG::BuiltinCall const& _builtinCall);
	void operator()(DFG::FunctionCall const& _functionCall);
	void operator()(DFG::Assignment const& _literal);

private:

	OptimizedCodeTransformContext& m_context;

	Stack* m_stack;

	// TODO: name!
	std::map<DFG::BasicBlock const*, Stack> m_initialExitLayoutOnLastVisit;

	Stack combineStack(Stack const& _stack1, Stack const& _stack2);
};

StackLayoutGenerator::StackLayoutGenerator(OptimizedCodeTransformContext& _context): m_context(_context)
{

}

void StackLayoutGenerator::operator()(DFG::FunctionCall const& _call)
{
	(void)_call;
	DEBUG(cout << "B: function call " << _call.functionCall->functionName.name.str() << ": " << stackToString(*m_stack) << std::endl;)
}

void StackLayoutGenerator::operator()(DFG::BuiltinCall const& _call)
{
	(void)_call;
	DEBUG(cout << "B: bultin call " << _call.functionCall->functionName.name.str() << ": " << stackToString(*m_stack) << std::endl;)
}

void StackLayoutGenerator::operator()(DFG::Assignment const& _assignment)
{
	for (auto& stackSlot: *m_stack)
		if (auto const* varSlot = get_if<VariableSlot>(&stackSlot))
			if (util::findOffset(_assignment.variables, *varSlot))
				stackSlot = JunkSlot{};
	DEBUG(cout << "B: assignment (";)
	for (auto var: _assignment.variables)
		DEBUG(cout << var.variable->name.str() << " ";)
	DEBUG(cout << ") pre: " << stackToString(*m_stack) << std::endl;)
}

namespace
{

Stack createIdealLayout(Stack const& post, vector<variant<PreviousSlot, set<unsigned>>> layout)
{
	// TODO: argue that util::permuteDup works with this kind of _getTargetPosition.
	// Or even better... rewrite this as custom algorithm matching createStackLayout exactly and make it work
	// for all cases, including duplicates and removals of slots that can be generated on the fly, etc.
	util::permuteDup(static_cast<unsigned>(layout.size()), [&](unsigned _i) -> set<unsigned> {
		// For call return values the target position is known.
		if (set<unsigned>* pos = get_if<set<unsigned>>(&layout.at(_i)))
			return *pos;
		// Previous arguments can stay where they are.
		return {_i};
	}, [&](unsigned _i) {
		std::swap(layout.back(), layout.at(layout.size() - _i - 1));
	}, [&](unsigned _i) {
		auto positions = get_if<set<unsigned>>(&layout.at(layout.size() - _i));
		yulAssert(positions, "");
		if(positions->count(static_cast<unsigned>(layout.size())))
		{
			positions->erase(static_cast<unsigned>(layout.size()));
			layout.emplace_back(set<unsigned>{static_cast<unsigned>(layout.size())});
		}
		else
		{
			optional<unsigned> duppingOffset;
			for (unsigned pos: *positions)
			{
				if (pos != layout.size() - _i)
				{
					duppingOffset = pos;
					break;
				}
			}
			yulAssert(duppingOffset, "");
			positions->erase(*duppingOffset);
			layout.emplace_back(set<unsigned>{*duppingOffset});
		}
	}, [&]() {
		yulAssert(false, "");
	}, [&]() {
		layout.pop_back();
	});

	// Now we can construct the ideal layout before the operation.
	// "layout" has the declared variables in the desired position and
	// for any PreviousSlot{x}, x yields the ideal place of the slot before the declaration.
	vector<optional<StackSlot>> idealLayout(post.size(), nullopt);
	for (auto const& [slot, idealPosition]: ranges::zip_view(post, layout))
		if (PreviousSlot* previousSlot = std::get_if<PreviousSlot>(&idealPosition))
			idealLayout.at(previousSlot->slot) = slot;

	while(!idealLayout.empty() && !idealLayout.back())
		idealLayout.pop_back();

	return idealLayout | ranges::views::transform([](optional<StackSlot> s) {
		yulAssert(s, "");
		return *s;
	}) | ranges::to<Stack>;
}
}

void StackLayoutGenerator::operator()(DFG::Operation const& _operation)
{
	yulAssert(m_stack, "");

	OptimizedCodeTransformContext::OperationInfo& operationInfo = m_context.operationStacks[&_operation];
	operationInfo.exitStack = *m_stack;

	DEBUG(cout << "OPERATION post:   " << stackToString(*m_stack) << std::endl
	          << "          input:  " << stackToString(_operation.input) << std::endl
			  << "          output: " << stackToString(_operation.output) << std::endl);

	vector<set<unsigned>> targetPositions(_operation.output.size(), set<unsigned>{});
	size_t numToKeep = 0;
	for (size_t idx: ranges::views::iota(0u, targetPositions.size()))
	{
		auto offsets = findAllOffsets(*m_stack, _operation.output.at(idx));
		for (unsigned offset: offsets)
		{
			targetPositions[idx].emplace(offset);
			++numToKeep;
		}

	}

	vector<variant<PreviousSlot, set<unsigned>>> layout;
	size_t idx = 0;
	for (auto slot: *m_stack | ranges::views::drop_last(numToKeep))
	{
		layout.emplace_back(PreviousSlot{idx++});
	}
	// The call produces values with known target positions.
	layout += targetPositions;

	*m_stack = createIdealLayout(*m_stack, layout);

	std::visit(*this, _operation.operation);

	for (StackSlot const& input: _operation.input)
		m_stack->emplace_back(input);

	operationInfo.entryStack = *m_stack;

	DEBUG(cout << "Operation pre before compress: " << stackToString(*m_stack) << std::endl;)

	cxx20::erase_if(*m_stack, [](StackSlot const& _slot) {
		if (auto const* returnLabelSlot = std::get_if<ReturnLabelSlot>(&_slot))
		{
			if (returnLabelSlot->callID)
				return true;
		}
		return false;
	});

	for (auto&& [idx, slot]: *m_stack | ranges::views::enumerate | ranges::views::reverse)
		// We can always push literals.
		if (std::holds_alternative<LiteralSlot>(slot) || std::holds_alternative<JunkSlot>(slot))
			m_stack->pop_back(); // TODO: verify that this is fine during range traversal
		// We can always push return labels of function calls.
		else if (auto const* returnLabelSlot = std::get_if<ReturnLabelSlot>(&slot))
		{
			if (returnLabelSlot->callID)
				m_stack->pop_back();
			else
				break;
		}
		// We can always dup values already on stack.
		else if (util::findOffset(*m_stack | ranges::views::take(idx), slot))
			m_stack->pop_back();
		else
			break;
	DEBUG(cout << "Operation pre after compress: " << stackToString(*m_stack) << "   " << m_stack << std::endl;)

	if (m_stack->size() > 12)
	{
		Stack newStack;
		for (auto slot: *m_stack)
		{
			if (holds_alternative<LiteralSlot>(slot))
				continue;
			if (auto* retLabel = get_if<ReturnLabelSlot>(&slot))
				if (retLabel->callID)
					continue;
			if (util::findOffset(newStack, slot))
				continue;
			newStack.emplace_back(slot);
		}
		*m_stack = newStack;
	}
}

Stack StackLayoutGenerator::operator()(DFG::BasicBlock const& _block, Stack _initialExitLayout)
{
	if (auto* lastSeenExitLayout = util::valueOrNullptr(m_initialExitLayoutOnLastVisit, &_block))
	{
		bool allSeen = true;
		for (auto slot: _initialExitLayout)
			if (!util::findOffset(*lastSeenExitLayout, slot))
			{
				allSeen = false;
				break;
			}
		if (allSeen)
			return m_context.blockInfos.at(&_block).entryLayout;
		bool allPresent = true;
		for (auto slot: *lastSeenExitLayout)
			if (!util::findOffset(_initialExitLayout, slot))
//				_initialExitLayout.emplace_back(slot); // TODO: think through when this happens. Maybe use combineStack.
			{
				allPresent = false;
				break;
			}
		if (!allPresent)
			_initialExitLayout = combineStack(_initialExitLayout, *lastSeenExitLayout);
			//yulAssert(util::findOffset(_initialExitLayout, slot), "How to deal with this?");
	}
	m_initialExitLayoutOnLastVisit[&_block] = _initialExitLayout;


	ScopedSaveAndRestore stackRestore(m_stack, nullptr);
	BlockGenerationInfo& info = m_context.blockInfos[&_block];

	Stack currentStack = _initialExitLayout;

	DEBUG(cout << "Block: " << &_block << std::endl;)

	std::visit(util::GenericVisitor{
		[&](std::monostate)
		{
			currentStack.clear();
		},
		[&](DFG::BasicBlock::Jump const& _jump)
		{
			if (!_jump.backwards)
			{
				currentStack = (*this)(*_jump.target, {});
				currentStack = combineStack(_initialExitLayout, currentStack); // TODO: why is this necessary?
/*				for (auto slot: _initialExitLayout)
					yulAssert(util::findOffset(currentStack, slot), "1");*/
			}

			DEBUG(cout << "B: JUMP EXIT: " << _jump.target << std::endl;)
			/*
			if (!targetInfo.entryLayout.has_value())
			{
				// We're in a loop. We must have jumped here conditionally:
				auto const* conditionalJump = get_if<DFG::BasicBlock::ConditionalJump>(&_jump.target->exit);
				yulAssert(conditionalJump, "");
				DEBUG(cout << "Current " << &_block << " conditonals: " << conditionalJump->zero << " " << conditionalJump->nonZero << std::endl;)
				{
					BlockGenerationInfo& zeroEntryInfo = m_context.blockInfos.at(conditionalJump->zero);
					BlockGenerationInfo& nonZeroEntryInfo = m_context.blockInfos.at(conditionalJump->nonZero);
					// One branch must already have an entry layout, the other is the looping branch.
					yulAssert(zeroEntryInfo.entryLayout.has_value() != nonZeroEntryInfo.entryLayout.has_value(), "");

					auto layout = zeroEntryInfo.entryLayout ? zeroEntryInfo.entryLayout : nonZeroEntryInfo.entryLayout;
					yulAssert(layout.has_value(), "");
					DEBUG(cout << "Looping entry layout: " << stackToString(*layout) << std::endl;)
					DEBUG(cout << "Zero has value: " << zeroEntryInfo.entryLayout.has_value() << std::endl;)
					DEBUG(cout << "Non-Zero has value: " << nonZeroEntryInfo.entryLayout.has_value() << std::endl;)


				}
			}
			yulAssert(targetInfo.entryLayout.has_value(), "");
			currentStack = *targetInfo.entryLayout;*/
		},
		[&](DFG::BasicBlock::ConditionalJump const& _conditionalJump)
		{
			Stack zeroEntryStack = (*this)(*_conditionalJump.zero, {});
			Stack nonZeroEntryStack = (*this)(*_conditionalJump.nonZero, {});
			currentStack = combineStack(zeroEntryStack, nonZeroEntryStack);
			/*BlockGenerationInfo& zeroInfo = m_context.blockInfos.at(_conditionalJump.zero);
			BlockGenerationInfo& nonZeroInfo = m_context.blockInfos.at(_conditionalJump.nonZero);
			zeroInfo.entryLayout = currentStack;
			nonZeroInfo.entryLayout = currentStack;*/
			currentStack = combineStack(_initialExitLayout, currentStack);
/*			for (auto slot: _initialExitLayout)
			{
				currentStack.emplace_front(slot);
				continue;
				std::cout << stackToString(_initialExitLayout) << " vs "  << stackToString(currentStack) << std::endl;
				yulAssert(util::findOffset(currentStack, slot), "2");
			}*/

			currentStack.emplace_back(_conditionalJump.condition);


			DEBUG(cout << "B: CONDITIONAL JUMP EXIT: " << _conditionalJump.zero << " / " << _conditionalJump.nonZero << std::endl;)
		},
		[&](DFG::BasicBlock::FunctionReturn const& _functionReturn)
		{
			yulAssert(_functionReturn.info, "");
			currentStack = _functionReturn.info->returnVariables | ranges::views::transform([](auto const& _varSlot){
				return StackSlot{_varSlot};
			}) | ranges::to<Stack>;
			currentStack.emplace_back(ReturnLabelSlot{});
		},
		[&](DFG::BasicBlock::Terminated const&) { currentStack.clear(); },
	}, _block.exit);

	DEBUG(cout << "B: BLOCK: " << &_block << std::endl;)

	m_stack = &currentStack;
	info.exitLayout = currentStack;

	DEBUG(cout << "B: EXIT LAYOUT (" << &_block << "): " << stackToString(currentStack) << std::endl;)

	for(auto& operation: _block.operations | ranges::views::reverse)
		(*this)(operation);

	DEBUG(cout << "B: ENTRY LAYOUT (" << &_block << "): " << stackToString(currentStack) << std::endl;)

	info.entryLayout = currentStack;

	for (auto const* backEntry: _block.entries)
		(*this)(*backEntry, currentStack);

	return info.entryLayout;
}

Stack StackLayoutGenerator::combineStack(Stack const& _stack1, Stack const& _stack2)
{
	if (_stack1.empty())
		return _stack2;
	if (_stack2.empty())
		return _stack1;

	// TODO: there is probably a better way than brute-forcing. This has n! complexity or worse, so
	// we can't keep it like this.

	Stack commonPrefix;
	for (auto&& [slot1, slot2]: ranges::zip_view(_stack1, _stack2))
	{
		if (!(slot1 == slot2))
			break;
		commonPrefix.emplace_back(slot1);
	}
	Stack stack1 = _stack1 | ranges::views::drop(commonPrefix.size()) | ranges::to<Stack>;
	Stack stack2 = _stack2 | ranges::views::drop(commonPrefix.size()) | ranges::to<Stack>;
	cxx20::erase_if(stack1, [](StackSlot const& slot) {
		if (ReturnLabelSlot const* returnSlot = get_if<ReturnLabelSlot>(&slot))
			if (returnSlot->callID)
				return true;
		if (holds_alternative<LiteralSlot>(slot))
			return true;
		return false;
	});
	cxx20::erase_if(stack2, [](StackSlot const& slot) {
		if (ReturnLabelSlot const* returnSlot = get_if<ReturnLabelSlot>(&slot))
			if (returnSlot->callID)
				return true;
		if (holds_alternative<LiteralSlot>(slot))
			return true;
		return false;
	});

	Stack candidate;
	for (auto slot: stack1)
		if (!util::findOffset(candidate, slot))
			candidate.emplace_back(slot);
	for (auto slot: stack2)
		if (!util::findOffset(candidate, slot))
			candidate.emplace_back(slot);
	std::map<size_t, Stack> sortedCandidates;

/*	if (candidate.size() > 8)
		return candidate;*/

	DEBUG(cout << "COMBINE STACKS: " << stackToString(stack1) << " + " << stackToString(stack2) << std::endl;)

	auto evaluate = [&](Stack const& _candidate) -> size_t {
		unsigned numOps = 0;
		Stack testStack = _candidate;
		createStackLayout(
			testStack,
			stack1,
			[&](unsigned _swapDepth) { ++numOps; if (_swapDepth > 16) numOps += 1000; },
			[&](unsigned _dupDepth) { ++numOps; if (_dupDepth > 16) numOps += 1000; },
			[&](StackSlot const&) {},
			[&](){},
			true
		);
		testStack = _candidate;
		createStackLayout(
			testStack,
			stack2,
			[&](unsigned _swapDepth) { ++numOps; if (_swapDepth > 16) numOps += 1000;  },
			[&](unsigned _dupDepth) { ++numOps; if (_dupDepth > 16) numOps += 1000; },
			[&](StackSlot const&) {},
			[&](){},
			true
		);
		// DEBUG(cout << "  CANDIDATE: " << stackToString(_candidate) << ": " << numOps << " swaps." << std::endl;)
		return numOps;
	};

	// See https://en.wikipedia.org/wiki/Heap's_algorithm
	size_t n = candidate.size();
	sortedCandidates.insert(std::make_pair(evaluate(candidate), candidate));
	std::vector<size_t> c(n, 0);
	size_t i = 1;
	while (i < n)
	{
		if (c[i] < i)
		{
			if (i & 1)
				std::swap(candidate.front(), candidate[i]);
			else
				std::swap(candidate[c[i]], candidate[i]);
			sortedCandidates.insert(std::make_pair(evaluate(candidate), candidate));
			++c[i];
			++i;
		}
		else
		{
			c[i] = 0;
			++i;
		}
	}

	DEBUG(cout << " BEST: " << stackToString(sortedCandidates.begin()->second) << " (" << sortedCandidates.begin()->first << " swaps)" << std::endl;)

	for (auto slot: sortedCandidates.begin()->second)
		commonPrefix.emplace_back(slot);
	return commonPrefix;
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
		{}
	};
	DEBUG(cout << std::endl << std::endl;)
	DEBUG(cout << "GENERATE STACK LAYOUTS" << std::endl;)
	DEBUG(cout << std::endl << std::endl;)

	{
		StackLayoutGenerator stackLayoutGenerator{context};
		stackLayoutGenerator(*context.dfg->entry, {});
		for (auto& functionInfo: context.dfg->functions | ranges::views::values)
		{
			stackLayoutGenerator(*functionInfo.entry, {});
		}
	}


	{
		std::set<DFG::BasicBlock const*> visited;
		auto recurse = [&](DFG::BasicBlock& _block, auto _recurse) -> void {
			if (visited.count(&_block))
				return;
			visited.insert(&_block);
			auto& info = context.blockInfos.at(&_block);
			std::visit(util::GenericVisitor{
				[&](std::monostate)
				{
				},
				[&](DFG::BasicBlock::Jump const& _jump)
				{
					/*auto& targetInfo = context.blockInfos.at(_jump.target);
					// TODO: Assert correctness, resp. achievability of layout.
					targetInfo.entryLayout = info.exitLayout;*/
					if (!_jump.backwards)
						_recurse(*_jump.target, _recurse);
				},
				[&](DFG::BasicBlock::ConditionalJump const& _conditionalJump)
				{
					auto& zeroTargetInfo = context.blockInfos.at(_conditionalJump.zero);
					auto& nonZeroTargetInfo = context.blockInfos.at(_conditionalJump.nonZero);
					// TODO: Assert correctness, resp. achievability of layout.
					Stack exitLayout = info.exitLayout;
					yulAssert(!exitLayout.empty(), "");
					exitLayout.pop_back();
					zeroTargetInfo.entryLayout = exitLayout;
					nonZeroTargetInfo.entryLayout = exitLayout;
					_recurse(*_conditionalJump.zero, _recurse);
					_recurse(*_conditionalJump.nonZero, _recurse);
				},
				[&](DFG::BasicBlock::FunctionReturn const&)
				{
				},
				[&](DFG::BasicBlock::Terminated const&) { },
			}, _block.exit);
		};
		recurse(*context.dfg->entry, recurse);
		for (auto& functionInfo: context.dfg->functions | ranges::views::values)
		{
			recurse(*functionInfo.entry, recurse);
		}
	}

	DEBUG(cout << std::endl << std::endl;)
	DEBUG(cout << "FORWARD CODEGEN" << std::endl;)
	DEBUG(cout << std::endl << std::endl;)

	CodeGenerator::run(_assembly, _builtinContext, _useNamedLabelsForFunctions, context, *context.dfg->entry);
}
