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

#include <libsolutil/Permutations.h>
#include <libsolutil/Visitor.h>
#include <libsolutil/cxx20.h>

#include <range/v3/view/drop.hpp>
#include <range/v3/view/drop_last.hpp>
#include <range/v3/view/enumerate.hpp>
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

string stackSlotToString(StackSlot const& _slot)
{
	return std::visit(util::GenericVisitor{
		[](ReturnLabelSlot const& _ret) -> string { return "RET[" + (_ret.call ? _ret.call->functionName.name.str() : "") + "]"; },
		[](VariableSlot const& _var) { return _var.variable->name.str(); },
		[](LiteralSlot const& _lit) { return util::toCompactHexWithPrefix(_lit.value); },
		[](TemporarySlot const&) -> string { return "TMP"; },
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
	_silent = false;
	// TODO: this still does not work, if there are duplicates in ``_currentStack``.
	if (_currentStack == _targetStack)
		return;
	if (!_silent)
		std::cout << "CREATE STACK LAYOUT: " << stackToString(_targetStack) << " FROM " << stackToString(_currentStack) << std::endl;

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
	if (topTargets.empty())
	{
		if (!_silent)
			std::cout << "POP TOP" << std::endl;
		_pop();
		_currentStack.pop_back();
		createStackLayout(_currentStack, _targetStack, _swap, _dup, _push, _pop, _silent);
		return;
	}
	else if (_targetStack.size() >= _currentStack.size() && _targetStack.at(_currentStack.size() - 1) == _currentStack.back())
	{
		if (!_silent)
			std::cout << "TOP is in place" << std::endl;

		// Current top is in place.
		// Dup deepest one to be dupped (TODO: choose optimal).
		for(auto&& [offset, slot]: _currentStack | ranges::views::enumerate)
		{
			if (findAllOffsets(_currentStack, slot).size() < findAllOffsets(_targetStack, slot).size())
			{
				if (!_silent)
					std::cout << "DUP" << std::endl;

				_dup(static_cast<unsigned>(_currentStack.size() - offset));
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
					std::cout << "PUSH" << std::endl;
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
					std::cout << "SWAP " << offset << std::endl;
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
			std::cout << "TOP is not in place" << std::endl;

		for (unsigned deepestTopTarget: topTargets)
		{
			if (deepestTopTarget >= _currentStack.size())
				break;
			if (!(_currentStack.at(deepestTopTarget) == _targetStack.at(deepestTopTarget)))
			{
				if (!_silent)
					std::cout << "Move into place " << deepestTopTarget << std::endl;

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
					std::cout << "DUP " << offset << std::endl;
				_dup(static_cast<unsigned>(_currentStack.size() - offset));
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
					std::cout << "PUSH" << std::endl;
				_push(slot);
				_currentStack.template emplace_back(slot);
				createStackLayout(_currentStack, _targetStack, _swap, _dup, _push, _pop, _silent);
				return;
			}
		}
		yulAssert(false, "");
	}

/*
	vector<set<unsigned>> targetPositions;
	for(auto const& currentElement: _currentStack)
		targetPositions.emplace_back(findAllOffsets(_targetStack, currentElement));
	vector<optional<StackSlot>> newElements(_targetStack.size(), nullopt);
	for (auto&& [pos, targetElement]: _targetStack | ranges::views::enumerate)
		if (!util::findOffset(_currentStack, targetElement))
			newElements[pos] = targetElement;

	util::permuteDup(static_cast<unsigned>(targetPositions.size()), [&](unsigned _i) {
		set<unsigned> positions = targetPositions.at(_i);
		cxx20::erase_if(positions, [&](unsigned i) -> bool {
			if (i == _i)
				return false;
			return i < _currentStack.size() && i < _targetStack.size() && _currentStack[i] == _targetStack[i];
		});
		return positions;
	}, [&](unsigned _i) {
		std::swap(targetPositions.back(), targetPositions.at(targetPositions.size() - _i - 1));
		std::swap(_currentStack.back(), _currentStack.at(_currentStack.size() - _i - 1));
		_swap(_i);
	}, [&](unsigned _i) {
		_dup(_i);
		_currentStack.emplace_back(_currentStack.at(_currentStack.size() - _i));
		auto& positions = targetPositions.at(targetPositions.size() - _i);
		if(positions.count(static_cast<unsigned>(targetPositions.size())))
		{
			positions.erase(static_cast<unsigned>(targetPositions.size()));
			targetPositions.emplace_back(set<unsigned>{static_cast<unsigned>(targetPositions.size())});
		}
		else
		{
			optional<unsigned> duppingOffset;
			for (unsigned pos: positions)
			{
				if (pos != targetPositions.size() - _i)
				{
					duppingOffset = pos;
					break;
				}
			}
			yulAssert(duppingOffset, "");
			positions.erase(*duppingOffset);
			targetPositions.emplace_back(set<unsigned>{*duppingOffset});
		}
	}, [&]() {
		if (!_silent)
			std::cout << "PUSHING AT SIZE: " << targetPositions.size() << std::endl;
		if (auto newElement = newElements.at(targetPositions.size()))
		{
			if (!_silent)
				std::cout << "NEW ELEMENT AT CORRECT POSITION " << stackSlotToString(*newElement) << std::endl;
			_push(*newElement);
			_currentStack.emplace_back(*newElement);
			newElement = nullopt;
			targetPositions.emplace_back(set<unsigned>{static_cast<unsigned>(targetPositions.size())});
		}
		else
		{
			if (!_silent)
				std::cout << "Want: " << stackSlotToString(_targetStack[targetPositions.size()]) << std::endl;
			for(auto&& [pos, newElement]: newElements | ranges::views::enumerate)
			{
				if (newElement)
				{
					if (!_silent)
						std::cout << "PUSHING " << stackSlotToString(*newElement) << " TARGETTING " << pos << std::endl;
					_push(*newElement);
					_currentStack.emplace_back(*newElement);
					newElement = nullopt;
					targetPositions.emplace_back(set<unsigned>{static_cast<unsigned>(pos)});
					return;
				}
			}
			yulAssert(false, "");
		}
	}, [&]() { _pop(); targetPositions.pop_back(); _currentStack.pop_back(); }, _silent);

	for (auto newSlot: _targetStack | ranges::views::take_last(_targetStack.size() - _currentStack.size()))
	{
		_push(newSlot);
		_currentStack.emplace_back(newSlot);
	}
*/
#if 0
	auto cleanStackTop = [&]() {
		while (!_currentStack.empty() && (holds_alternative<JunkSlot>(_currentStack.back()) || !util::findOffset(_targetStack, _currentStack.back())))
		{
			_pop();
			_currentStack.pop_back();
		}
	};
	cleanStackTop();

	set<StackSlot> assignedToFree;
	for (auto&& [pos, slot]: _targetStack | ranges::views::enumerate)
	{
		if (!_silent)
			std::cout << "Want " << stackSlotToString(slot) << " at " << pos << std::endl;
		if (pos < _currentStack.size() && _currentStack.at(pos) == slot)
		{
			if (!_silent)
				std::cout << "OK" << std::endl;
		}
		else if (auto depth = util::findOffset(_currentStack | ranges::views::reverse, slot))
		{
			if (!_silent)
				std::cout << "At depth " << *depth << std::endl;
			size_t offset = _currentStack.size() - *depth - 1;
			if (offset < pos)
			{
				_dup(static_cast<unsigned>(*depth + 1));
				_currentStack.emplace_back(_currentStack.at(offset));

				if (_currentStack.size() - pos - 1 > 0)
				{
					if (!_silent)
						std::cout << "swap " << _currentStack.size() - pos - 1 << std::endl;
					_swap(static_cast<unsigned>(_currentStack.size() - pos - 1));
					std::swap(_currentStack.back(), _currentStack.at(pos));
					cleanStackTop();
				}
			}
			else
			{
				if (*depth > 0)
				{
					if (!_silent)
						std::cout << "swap up: " << *depth << std::endl;
					_swap(static_cast<unsigned>(*depth));
					std::swap(_currentStack.back(), _currentStack.at(_currentStack.size() - *depth - 1));
				}

				if (_currentStack.size() - pos - 1 > 0)
				{
					if (!_silent)
						std::cout << "swap " << _currentStack.size() - pos - 1 << std::endl;
					_swap(static_cast<unsigned>(_currentStack.size() - pos - 1));
					std::swap(_currentStack.back(), _currentStack.at(pos));
					cleanStackTop();
				}
			}
		}
		else
		{
			if (!_silent)
				std::cout << "Push slot" << std::endl;
			_push(slot);
			_currentStack.emplace_back(slot);
			if (_currentStack.size() - pos - 1 > 0)
			{
				_swap(static_cast<unsigned>(_currentStack.size() - pos - 1));
				std::swap(_currentStack.back(), _currentStack.at(pos));
				cleanStackTop();
			}
		}
	}
	while(_currentStack.size() > _targetStack.size())
	{
		_pop();
		_currentStack.pop_back();
	}
#endif
	if (!_silent)
		std::cout << "CREATED STACK LAYOUT: " << stackToString(_currentStack) << std::endl;
	yulAssert(_currentStack == _targetStack, "");

}

}

class CodeGenerator
{
public:
	static void run(AbstractAssembly& _assembly, BuiltinContext& _builtinContext, bool _useNamedLabelsForFunctions, OptimizedCodeTransformContext const& _info, DFG::BasicBlock const& _entry)
	{
		CodeGenerator generator(_assembly, _builtinContext, _useNamedLabelsForFunctions,  _info);
		generator(_entry);
		generator.generateStaged();
	}
private:
	CodeGenerator(AbstractAssembly& _assembly, BuiltinContext& _builtinContext, bool _useNamedLabelsForFunctions, OptimizedCodeTransformContext const& _info):
	m_assembly(_assembly), m_builtinContext(_builtinContext), m_useNamedLabelsForFunctions(_useNamedLabelsForFunctions), m_info(_info) {}
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

		std::cout << std::endl;
		std::cout << "F: start of function " << _functionInfo.function->name.str() << std::endl;
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

	void operator()(DFG::FunctionCall const& _call)
	{
		// TODO: it'd probably be wise to assert that the top of the stack matches.
		auto entryLabel = getFunctionLabel(*_call.function);
		std::cout << "F: function call " << _call.functionCall->functionName.name.str() << " pre: " << stackToString(m_stack) << std::endl;
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
			m_stack.emplace_back(TemporarySlot{_call.functionCall, i});
		std::cout << "F: function call " << _call.functionCall->functionName.name.str() << " post: " << stackToString(m_stack) << std::endl;
	}

	void operator()(DFG::BuiltinCall const& _call)
	{
		// TODO: it'd probably be wise to assert that the top of the stack matches.
		std::cout << "F: builtin call " << _call.functionCall->functionName.name.str() << " pre: " << stackToString(m_stack) << std::endl;
		m_assembly.setSourceLocation(locationOf(_call));
		_call.builtin->generateCode(*_call.functionCall, m_assembly, m_builtinContext, [](auto&&){});
		for (size_t i = 0; i < _call.arguments; ++i)
			m_stack.pop_back();
		for (size_t i = 0; i < _call.builtin->returns.size(); ++i)
			m_stack.emplace_back(TemporarySlot{_call.functionCall, i});
		std::cout << "F: builtin call " << _call.functionCall->functionName.name.str() << " post: " << stackToString(m_stack) << std::endl;
	}

	void operator()(DFG::Assignment const& _assignment)
	{
		m_assembly.setSourceLocation(locationOf(_assignment));
		std::cout << "F: assign (";
		for (auto var: _assignment.variables)
			std::cout << var.variable->name.str() << " ";
		std::cout << ") pre: " << stackToString(m_stack) << std::endl;

		for(auto& currentSlot: m_stack)
			if (VariableSlot const* varSlot = get_if<VariableSlot>(&currentSlot))
				if (util::findOffset(_assignment.variables, *varSlot))
					currentSlot = JunkSlot{};

		for (auto&& [currentSlot, varSlot]: ranges::zip_view(m_stack | ranges::views::take_last(_assignment.variables.size()), _assignment.variables))
			currentSlot = varSlot;

		std::cout << "F: assign (";
		for (auto var: _assignment.variables)
			std::cout << var.variable->name.str() << " ";
		std::cout << ") post: " << stackToString(m_stack) << std::endl;
	}



	// TODO: get rid of _insertLabel hack.
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
			std::cout << "F: GENERATING: " << &_block << " (label: " << (label ? std::to_string(*label) : "NONE") << ")" << std::endl;
		}

		std::cout << "F: CREATING ENTRY LAYOUT " << stackToString(info.entryLayout) << " FROM " << stackToString(m_stack) << std::endl;
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

		std::cout << std::endl << std::endl;
		std::cout << "F: EXIT LAYOUT (" << &_block << "): " << stackToString(info.exitLayout) << " == " << stackToString(m_stack) << std::endl;
		// TODO: conditions!
		//		yulAssert(info.exitLayout == m_stack, "");


		std::visit(util::GenericVisitor{
			[&](std::monostate)
			{
				std::cout << "F: MAIN EXIT" << std::endl;
				m_assembly.appendInstruction(evmasm::Instruction::STOP);
			},
			[&](DFG::BasicBlock::Jump const& _jump)
			{
				std::cout << "F: JUMP EXIT TO: " << _jump.target << std::endl;

				BlockGenerationInfo const& targetInfo = m_info.blockInfos.at(_jump.target);
				std::cout << "F: CURRENT " << stackToString(m_stack) << " => " << stackToString(targetInfo.entryLayout) << std::endl;
				createStackLayout(targetInfo.entryLayout);

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
				std::cout << "F: CONDITIONAL JUMP EXIT TO: " << _conditionalJump.nonZero << " / " << _conditionalJump.zero << std::endl;
				std::cout << "F: CURRENT EXIT LAYOUT: " << stackToString(info.exitLayout) << std::endl;
				BlockGenerationInfo const& nonZeroInfo = m_info.blockInfos.at(_conditionalJump.nonZero);
				BlockGenerationInfo const& zeroInfo = m_info.blockInfos.at(_conditionalJump.zero);
				std::cout << "F: non-zero entry layout: " << stackToString(nonZeroInfo.entryLayout) << std::endl;
				std::cout << "F: zero entry layout: " << stackToString(zeroInfo.entryLayout) << std::endl;

				for (auto const* nonZeroEntry: _conditionalJump.nonZero->entries)
				{
					BlockGenerationInfo const& entryInfo = m_info.blockInfos.at(nonZeroEntry);
					std::cout << "  F: non-zero entry exit: " << stackToString(entryInfo.exitLayout) << std::endl;
				}
				for (auto const* zeroEntry: _conditionalJump.zero->entries)
				{
					BlockGenerationInfo const& entryInfo = m_info.blockInfos.at(zeroEntry);
					std::cout << "  F: zero entry exit: " << stackToString(entryInfo.exitLayout) << std::endl;
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
				yulAssert(m_stack == m_info.blockInfos.at(_conditionalJump.nonZero).entryLayout, "");

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
				std::cout << "F: Function return exit: " << _functionReturn.info->function->name.str() << std::endl;

				yulAssert(m_currentFunctionInfo, "");
				Stack exitStack = m_currentFunctionInfo->returnVariables | ranges::views::transform([](auto const& _varSlot){
					return StackSlot{_varSlot};
				}) | ranges::to<Stack>;
				exitStack.emplace_back(ReturnLabelSlot{});

				std::cout << "Return from function " << m_currentFunctionInfo->function->name.str() << std::endl;
				std::cout << "EXIT STACK: " << stackToString(exitStack) << std::endl;
				createStackLayout(exitStack);
				m_assembly.setSourceLocation(locationOf(*m_currentFunctionInfo));
				m_assembly.appendJump(0, AbstractAssembly::JumpType::OutOfFunction); // TODO: stack height diff.
				m_assembly.setStackHeight(0);
				m_stack.clear();

			},
			[&](DFG::BasicBlock::Revert const&)
			{
				std::cout << "F: REVERT EXIT" << std::endl;
				yulAssert(false, "");

			},
			[&](DFG::BasicBlock::Stop const&)
			{
				std::cout << "F: STOP EXIT" << std::endl;
				yulAssert(false, "");
			}
		}, _block.exit);
	}


	void createStackLayout(Stack _targetStack)
	{
		std::cout << "F: CREATE " << stackToString(_targetStack) << " FROM " << stackToString(m_stack) << std::endl;
		::createStackLayout(m_stack, _targetStack, [&](unsigned _i) {
			m_assembly.appendInstruction(evmasm::swapInstruction(_i));
		}, [&](unsigned _i) {
			m_assembly.appendInstruction(evmasm::dupInstruction(_i));
		}, [&](StackSlot const& _slot) {
			if (auto offset = util::findOffset(m_stack, _slot))
			{
				m_assembly.appendInstruction(evmasm::dupInstruction(static_cast<unsigned>(m_stack.size() - *offset)));
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
					yulAssert(_returnLabel.call, "Cannot produce function return label.");
					if (!m_returnLabels.count(_returnLabel.call))
						m_returnLabels[_returnLabel.call] = m_assembly.newLabelId();
					m_assembly.appendLabelReference(m_returnLabels.at(_returnLabel.call));
				},
				[&](auto const& _slot)
				{
					std::cout << "SLOT: " << stackSlotToString(StackSlot{_slot}) << std::endl;
					std::cout << "THIS SHOULD NOT HAPPEN!" << std::endl;
					m_assembly.appendInstruction(evmasm::Instruction::CALLDATASIZE);
					//yulAssert(false, "Slot not found.");
				}
			}, _slot);
		}, [&]() { m_assembly.appendInstruction(evmasm::Instruction::POP); });
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

	Stack operator()(DFG::BasicBlock const& _block, Stack _exitLayout = {});

	void operator()(DFG::Operation const& _operation);

	void operator()(DFG::BuiltinCall const& _builtinCall);
	void operator()(DFG::FunctionCall const& _functionCall);
	void operator()(DFG::Assignment const& _literal);

private:

	OptimizedCodeTransformContext& m_context;

	Stack* m_stack;

	// TODO: name!
	std::map<DFG::BasicBlock const*, std::set<StackSlot>> m_exitSlotsSeen;

	Stack combineStack(Stack const& _stack1, Stack const& _stack2);
};

StackLayoutGenerator::StackLayoutGenerator(OptimizedCodeTransformContext& _context): m_context(_context)
{

}

void StackLayoutGenerator::operator()(DFG::FunctionCall const& _call)
{
	std::cout << "B: function call " << _call.functionCall->functionName.name.str() << ": " << stackToString(*m_stack) << std::endl;

	ScopedSaveAndRestore restoreStack(m_stack, nullptr);
	DFG::FunctionInfo const& functionInfo = m_context.dfg->functions.at(_call.function);
	(*this)(*functionInfo.entry);
}

void StackLayoutGenerator::operator()(DFG::BuiltinCall const& _call)
{
	std::cout << "B: bultin call " << _call.functionCall->functionName.name.str() << ": " << stackToString(*m_stack) << std::endl;
}

void StackLayoutGenerator::operator()(DFG::Assignment const& _assignment)
{
	for (auto& stackSlot: *m_stack)
		if (auto const* varSlot = get_if<VariableSlot>(&stackSlot))
			if (util::findOffset(_assignment.variables, *varSlot))
				stackSlot = JunkSlot{};
	std::cout << "B: assignment (";
	for (auto var: _assignment.variables)
		std::cout << var.variable->name.str() << " ";
	std::cout << ") pre: " << stackToString(*m_stack) << std::endl;
}


void StackLayoutGenerator::operator()(DFG::Operation const& _operation)
{
	yulAssert(m_stack, "");

	OptimizedCodeTransformContext::OperationInfo& operationInfo = m_context.operationStacks[&_operation];
	operationInfo.exitStack = *m_stack;

	std::cout << "OPERATION post:   " << stackToString(*m_stack) << std::endl
	          << "          input:  " << stackToString(_operation.input) << std::endl
			  << "          output: " << stackToString(_operation.output) << std::endl;

	vector<set<unsigned>> targetPositions(_operation.output.size(), set<unsigned>{});
	size_t numToKeep = 0;
	for (size_t idx: ranges::views::iota(0u, targetPositions.size()))
		for (unsigned offset: findAllOffsets(*m_stack, _operation.output.at(idx)))
		{
			targetPositions[idx].emplace(offset);
			++numToKeep;
		}



	struct PreviousSlot { size_t slot; };
	vector<variant<PreviousSlot, set<unsigned>>> layout;
	// Before the call the desired stack (below the call arguments) has size m_stack->size() - numToKeep
	for (size_t slot: ranges::views::iota(0u, m_stack->size() - numToKeep))
		layout.emplace_back(PreviousSlot{slot});
	// The call produces values with known target positions.
	layout += targetPositions;

	// TODO: argue that util::permuteDup works with this kind of _getTargetPosition.
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
	vector<optional<StackSlot>> idealLayout(m_stack->size() - numToKeep, nullopt);
	for (auto const& [slot, idealPosition]: ranges::zip_view(*m_stack, layout))
		if (PreviousSlot* previousSlot = std::get_if<PreviousSlot>(&idealPosition))
			idealLayout.at(previousSlot->slot) = slot;

	m_stack->resize(idealLayout.size());
	for (auto&& [slot, idealSlot]: ranges::zip_view(*m_stack, idealLayout))
	{
		yulAssert(idealSlot.has_value(), "");
		slot = *idealSlot;
	}

	std::visit(*this, _operation.operation);

	for (StackSlot const& input: _operation.input)
		m_stack->emplace_back(input);

	operationInfo.entryStack = *m_stack;

	std::cout << "Operation pre before compress: " << stackToString(*m_stack) << std::endl;

	for (auto&& [idx, slot]: *m_stack | ranges::views::enumerate | ranges::views::reverse)
		// We can always push literals.
		if (std::holds_alternative<LiteralSlot>(slot) || std::holds_alternative<JunkSlot>(slot))
			m_stack->pop_back(); // TODO: verify that this is fine during range traversal
		// We can always push return labels of function calls.
		else if (auto const* returnLabelSlot = std::get_if<ReturnLabelSlot>(&slot))
		{
			if (returnLabelSlot->call)
				m_stack->pop_back();
			else
				break;
		}
		// We can always dup values already on stack.
		else if (util::findOffset(*m_stack | ranges::views::take(idx), slot))
			m_stack->pop_back();
		else
			break;
	std::cout << "Operation pre after compress: " << stackToString(*m_stack) << "   " << m_stack << std::endl;
}

Stack StackLayoutGenerator::operator()(DFG::BasicBlock const& _block, Stack _exitLayout)
{
	ScopedSaveAndRestore stackRestore(m_stack, nullptr);
	BlockGenerationInfo& info = m_context.blockInfos[&_block];

	Stack currentStack = _exitLayout;
	if (auto const* slotsSeen = util::valueOrNullptr(m_exitSlotsSeen, &_block))
	{
		bool seenAll = true;
		for (auto& slot: _exitLayout)
		{
			if (!slotsSeen->count(slot))
			{
				std::cout << "UNSEEN SLOT: " << stackSlotToString(slot) << std::endl;
				seenAll = false;
			}
		}
		if (seenAll)
			return info.entryLayout;
	}
	{
		auto& exitSlotsSeen = m_exitSlotsSeen[&_block];
		for (auto& slot: _exitLayout)
			exitSlotsSeen.insert(slot);
	}

	std::visit(util::GenericVisitor{
		[&](std::monostate)
		{
		},
		[&](DFG::BasicBlock::Jump const& _jump)
		{
			currentStack = combineStack(currentStack, (*this)(*_jump.target, {}));
			BlockGenerationInfo& targetInfo = m_context.blockInfos.at(_jump.target);
			targetInfo.entryLayout = currentStack;

			std::cout << "B: JUMP EXIT: " << _jump.target << std::endl;
			/*
			if (!targetInfo.entryLayout.has_value())
			{
				// We're in a loop. We must have jumped here conditionally:
				auto const* conditionalJump = get_if<DFG::BasicBlock::ConditionalJump>(&_jump.target->exit);
				yulAssert(conditionalJump, "");
				std::cout << "Current " << &_block << " conditonals: " << conditionalJump->zero << " " << conditionalJump->nonZero << std::endl;
				{
					BlockGenerationInfo& zeroEntryInfo = m_context.blockInfos.at(conditionalJump->zero);
					BlockGenerationInfo& nonZeroEntryInfo = m_context.blockInfos.at(conditionalJump->nonZero);
					// One branch must already have an entry layout, the other is the looping branch.
					yulAssert(zeroEntryInfo.entryLayout.has_value() != nonZeroEntryInfo.entryLayout.has_value(), "");

					auto layout = zeroEntryInfo.entryLayout ? zeroEntryInfo.entryLayout : nonZeroEntryInfo.entryLayout;
					yulAssert(layout.has_value(), "");
					std::cout << "Looping entry layout: " << stackToString(*layout) << std::endl;
					std::cout << "Zero has value: " << zeroEntryInfo.entryLayout.has_value() << std::endl;
					std::cout << "Non-Zero has value: " << nonZeroEntryInfo.entryLayout.has_value() << std::endl;


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
			BlockGenerationInfo& zeroInfo = m_context.blockInfos.at(_conditionalJump.zero);
			BlockGenerationInfo& nonZeroInfo = m_context.blockInfos.at(_conditionalJump.nonZero);
			zeroInfo.entryLayout = currentStack;
			nonZeroInfo.entryLayout = currentStack;
			currentStack.emplace_back(_conditionalJump.condition);

			std::cout << "B: CONDITIONAL JUMP EXIT: " << _conditionalJump.zero << " / " << _conditionalJump.nonZero << std::endl;
		},
		[&](DFG::BasicBlock::FunctionReturn const& _functionReturn)
		{
			yulAssert(_functionReturn.info, "");
			currentStack = _functionReturn.info->returnVariables | ranges::views::transform([](auto const& _varSlot){
				return StackSlot{_varSlot};
			}) | ranges::to<Stack>;
			currentStack.emplace_back(ReturnLabelSlot{});
		},
		[](DFG::BasicBlock::Stop const&) { yulAssert(false, "STOP EXIT"); },
		[](DFG::BasicBlock::Revert const&) { yulAssert(false, "REVERT EXIT"); }
	}, _block.exit);

	std::cout << "B: BLOCK: " << &_block << std::endl;

	m_stack = &currentStack;
	info.exitLayout = currentStack;

	std::cout << "B: EXIT LAYOUT (" << &_block << "): " << stackToString(currentStack) << std::endl;

	for(auto& operation: _block.operations | ranges::views::reverse)
		(*this)(operation);

	std::cout << "B: ENTRY LAYOUT (" << &_block << "): " << stackToString(currentStack) << std::endl;

	info.entryLayout = currentStack;

	for (auto const* entry: _block.entries)
		if (holds_alternative<DFG::BasicBlock::ConditionalJump>(entry->exit))
		{
			(*this)(*entry, currentStack);
			auto exitLayout = m_context.blockInfos.at(entry).exitLayout;
			if (!exitLayout.empty())
				exitLayout.pop_back();
			info.entryLayout = combineStack(info.entryLayout, exitLayout);
		}
		else if (holds_alternative<DFG::BasicBlock::Jump>(entry->exit))
		{
			(*this)(*entry, currentStack);
			auto exitLayout = m_context.blockInfos.at(entry).exitLayout;
			info.entryLayout = combineStack(info.entryLayout, exitLayout);
		}

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
	std::set<StackSlot> slotSet;
	slotSet += _stack1;
	slotSet += _stack2;
	Stack candidate(slotSet.begin(), slotSet.end());
	std::map<Stack, size_t> requiredSwaps;

	std::cout << "COMBINE STACKS: " << stackToString(_stack1) << " + " << stackToString(_stack2) << std::endl;

	auto evaluate = [&](Stack const& _candidate) -> size_t {
		unsigned numOps = 0;
		Stack testStack = _candidate;
		createStackLayout(
			testStack,
			_stack1,
			[&](unsigned) { ++numOps; },
			[&](unsigned) { ++numOps; },
			[&](StackSlot const&) {},
			[&](){},
			true
		);
		testStack = _candidate;
		createStackLayout(
			testStack,
			_stack2,
			[&](unsigned) { ++numOps; },
			[&](unsigned) { ++numOps; },
			[&](StackSlot const&) {},
			[&](){},
			true
		);
		// std::cout << "  CANDIDATE: " << stackToString(_candidate) << ": " << numOps << " swaps." << std::endl;
		return numOps;
	};

	// See https://en.wikipedia.org/wiki/Heap's_algorithm
	size_t n = candidate.size();
	requiredSwaps[candidate] = evaluate(candidate);
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
			requiredSwaps[candidate] = evaluate(candidate);
			++c[i];
			++i;
		}
		else
		{
			c[i] = 0;
			++i;
		}
	}

	std::cout << " BEST: " << stackToString(requiredSwaps.begin()->first) << " (" << requiredSwaps.begin()->second << " swaps)" << std::endl;

	return requiredSwaps.begin()->first;
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
	std::cout << std::endl << std::endl;
	std::cout << "GENERATE STACK LAYOUTS" << std::endl;
	std::cout << std::endl << std::endl;

	{
		StackLayoutGenerator stackLayoutGenerator{context};
		stackLayoutGenerator(*context.dfg->entry);
	}

	std::cout << std::endl << std::endl;
	std::cout << "FORWARD CODEGEN" << std::endl;
	std::cout << std::endl << std::endl;

	CodeGenerator::run(_assembly, _builtinContext, _useNamedLabelsForFunctions, context, *context.dfg->entry);
}
