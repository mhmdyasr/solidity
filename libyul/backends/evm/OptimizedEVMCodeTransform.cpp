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

#if 0
template<typename Range, typename Value>
vector<int> findAllOffsets(Range&& _range, Value&& _value)
{
	vector<int> result;
	auto begin = std::begin(_range);
	auto end = std::end(_range);
	auto it = begin;
	while (it != end)
	{
		it = std::find(it, end, std::forward<Value>(_value));
		if (it == end)
			return result;
		result.emplace_back(static_cast<int>(std::distance(begin, it)));
		++it;
	}
	return result;
}
#endif

template<typename Swap, typename Dup, typename Pop, typename PushSlot>
void createStackLayout(Stack& _currentStack, Stack const& _targetStack, Swap _swap, Dup _dup, PushSlot _push, Pop _pop, bool _silent = false)
{
	if (_currentStack == _targetStack)
		return;
	if (!_silent)
		std::cout << "CREATE STACK LAYOUT: " << stackToString(_targetStack) << " FROM " << stackToString(_currentStack) << std::endl;

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

	if (!_silent)
		std::cout << "CREATED STACK LAYOUT: " << stackToString(_currentStack) << std::endl;
}

}

class CodeGenerator
{
public:
	static void run(AbstractAssembly& _assembly, BuiltinContext& _builtinContext, OptimizedCodeTransformContext const& _info, DFG::BasicBlock const& _entry)
	{
		CodeGenerator generator(_assembly, _builtinContext, _info);
		generator(_entry);
		generator.generateStagedFunctions();
	}
private:
	CodeGenerator(AbstractAssembly& _assembly, BuiltinContext& _builtinContext, OptimizedCodeTransformContext const& _info):
	m_assembly(_assembly), m_builtinContext(_builtinContext), m_info(_info) {}
public:

	AbstractAssembly::LabelID getFunctionLabel(Scope::Function const& _function)
	{
		ScopedSaveAndRestore restoreStack(m_stack, {});
		DFG::FunctionInfo const& functionInfo = m_info.dfg->functions.at(&_function);
		BlockGenerationInfo& info = m_info.blockInfos.at(functionInfo.entry);
		info.label = m_useNamedLabelsForFunctions ?
			m_assembly.namedLabel(
				functionInfo.function->name.str(),
				functionInfo.function->arguments.size(),
				functionInfo.function->returns.size(),
				{}
			) : m_assembly.newLabelId();

		m_stagedFunctions.emplace_back(&functionInfo);
		return *info.label;
	}

	void operator()(DFG::FunctionInfo const& _functionInfo)
	{
		BlockGenerationInfo& info = m_info.blockInfos.at(_functionInfo.entry);

		std::cout << std::endl;
		std::cout << "F: start of function " << _functionInfo.function->name.str() << std::endl;
		m_stack.clear();
		m_stack.emplace_back(ReturnLabelSlot{});
		for (auto const& param: _functionInfo.parameters)
			m_stack.emplace_back(param);
		m_assembly.setStackHeight(static_cast<int>(m_stack.size()));
		m_assembly.setSourceLocation(locationOf(_functionInfo));
		yulAssert(info.label.has_value(), "");

		m_assembly.appendLabel(*info.label);
		createStackLayout(*info.entryLayout);

		(*this)(*_functionInfo.entry, false);

		Stack exitStack = _functionInfo.returnVariables | ranges::views::transform([](auto const& _varSlot){
			return StackSlot{_varSlot};
		}) | ranges::to<Stack>;
		exitStack.emplace_back(ReturnLabelSlot{});

		std::cout << "Return from function " << _functionInfo.function->name.str() << std::endl;
		std::cout << "EXIT STACK: " << stackToString(exitStack) << std::endl;
		createStackLayout(exitStack);
		m_assembly.setSourceLocation(locationOf(_functionInfo));
		m_assembly.appendJump(0, AbstractAssembly::JumpType::OutOfFunction); // TODO: stack height diff.
		m_assembly.setStackHeight(0);
		m_stack.clear();
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
	void operator()(DFG::BasicBlock const& _block, bool _insertLabel = true)
	{
		if (m_generated.count(&_block))
			return;
		m_generated.insert(&_block);

		BlockGenerationInfo& info = m_info.blockInfos.at(&_block);

		if (_insertLabel && info.label)
			m_assembly.appendLabel(*info.label);

		std::cout << "F: GENERATING: " << &_block << std::endl;

		for (auto const& entry: _block.entries)
		{
			BlockGenerationInfo& entryInfo = m_info.blockInfos.at(entry);
			std::cout << " F: EXIT LAYOUT OF ENTRY: " << stackToString(*entryInfo.exitLayout) << std::endl;
		}

		// TODO: rather assert that this is the current layout (and make sure it always is)?
		m_stack = *info.entryLayout;
		m_assembly.setStackHeight(static_cast<int>(m_stack.size()));
		std::cout << "F: ASSUMED ENTRY LAYOUT: " << stackToString(m_stack) << std::endl;

		for (auto const& operation: _block.operations)
		{
			OptimizedCodeTransformContext::OperationInfo const& operationInfo = m_info.operationStacks.at(&operation);
			createStackLayout(operationInfo.entryStack);
			std::visit(*this, operation.operation);
			// TODO: is this actually necessary each time? Last time is probably enough, if at all needed.
			createStackLayout(operationInfo.exitStack);
		}

		std::cout << std::endl << std::endl;
		std::cout << "F: EXIT LAYOUT (" << &_block << "): " << stackToString(*info.exitLayout) << " == " << stackToString(m_stack) << std::endl;
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

				BlockGenerationInfo& targetInfo = m_info.blockInfos.at(_jump.target);
				std::cout << "F: CURRENT " << stackToString(m_stack) << " => " << stackToString(*targetInfo.entryLayout) << std::endl;
				createStackLayout(*targetInfo.entryLayout);

				if (!targetInfo.label && _jump.target->entries.size() == 1)
					(*this)(*_jump.target);
				else
				{
					if (!targetInfo.label)
						targetInfo.label = m_assembly.newLabelId();

					if (m_generated.count(_jump.target))
						m_assembly.appendJumpTo(*targetInfo.label);
					else
						(*this)(*_jump.target);
				}
			},
			[&](DFG::BasicBlock::ConditionalJump const& _conditionalJump)
			{
				std::cout << "F: CONDITIONAL JUMP EXIT TO: " << _conditionalJump.nonZero << " / " << _conditionalJump.zero << std::endl;
				std::cout << "F: CURRENT EXIT LAYOUT: " << stackToString(*info.exitLayout) << std::endl;
				BlockGenerationInfo& nonZeroInfo = m_info.blockInfos.at(_conditionalJump.nonZero);
				BlockGenerationInfo& zeroInfo = m_info.blockInfos.at(_conditionalJump.zero);
				std::cout << "F: non-zero entry layout: " << stackToString(*nonZeroInfo.entryLayout) << std::endl;
				std::cout << "F: zero entry layout: " << stackToString(*zeroInfo.entryLayout) << std::endl;

				for (auto const* nonZeroEntry: _conditionalJump.nonZero->entries)
				{
					BlockGenerationInfo& entryInfo = m_info.blockInfos.at(nonZeroEntry);
					std::cout << "  F: non-zero entry exit: " << stackToString(*entryInfo.exitLayout) << std::endl;
				}
				for (auto const* zeroEntry: _conditionalJump.zero->entries)
				{
					BlockGenerationInfo& entryInfo = m_info.blockInfos.at(zeroEntry);
					std::cout << "  F: zero entry exit: " << stackToString(*entryInfo.exitLayout) << std::endl;
				}
/*
 * TODO!
				yulAssert(nonZeroInfo.entryLayout == zeroInfo.entryLayout, "");
				yulAssert((m_stack | ranges::views::drop_last(1) | ranges::to<Stack>) == nonZeroInfo.entryLayout, "");
*/
				if (!nonZeroInfo.label)
					nonZeroInfo.label = m_assembly.newLabelId();
				m_assembly.appendJumpToIf(*nonZeroInfo.label);


				if (!zeroInfo.label && _conditionalJump.zero->entries.size() == 1)
					(*this)(*_conditionalJump.zero);
				else
				{
					if (!zeroInfo.label)
						zeroInfo.label = m_assembly.newLabelId();

					if (m_generated.count(_conditionalJump.zero))
						m_assembly.appendJumpTo(*zeroInfo.label);
					else
						(*this)(*_conditionalJump.zero);
				}

				(*this)(*_conditionalJump.nonZero);
			},
			[&](DFG::BasicBlock::FunctionReturn const& _functionReturn)
			{
				std::cout << "F: Function return exit: " << _functionReturn.info->function->name.str() << std::endl;
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
		::createStackLayout(m_stack, move(_targetStack), [&](unsigned _i) {
			m_assembly.appendInstruction(evmasm::swapInstruction(_i));
		}, [&](unsigned _i) {
			m_assembly.appendInstruction(evmasm::dupInstruction(_i));
		}, [&](StackSlot const& _slot) {
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
					yulAssert(false, "Slot not found.");
				}
			}, _slot);
		}, [&]() { m_assembly.appendInstruction(evmasm::Instruction::POP); });
	}
private:

	void generateStagedFunctions()
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
		}
	}

	AbstractAssembly& m_assembly;
	BuiltinContext& m_builtinContext;
	bool m_useNamedLabelsForFunctions = true; // TODO: pass in
	OptimizedCodeTransformContext const& m_info;
	Stack m_stack;
	std::map<yul::FunctionCall const*, AbstractAssembly::LabelID> m_returnLabels;
	std::set<DFG::BasicBlock const*> m_generated;
	std::list<DFG::FunctionInfo const*> m_stagedFunctions;
	std::set<DFG::FunctionInfo const*> m_generatedFunctions;
};

class StackLayoutGenerator
{
public:
	StackLayoutGenerator(OptimizedCodeTransformContext& _context, AbstractAssembly& _assembly, BuiltinContext& _builtinContext, bool _useNamedLabelsForFunctions);

	void operator()(DFG::BasicBlock const& _block);

	void operator()(DFG::Operation const& _operation);

	void operator()(DFG::BuiltinCall const& _builtinCall);
	void operator()(DFG::FunctionCall const& _functionCall);
	void operator()(DFG::Assignment const& _literal);

private:

	void visit(DFG::BasicBlock const& _block);

	OptimizedCodeTransformContext& m_context;
	AbstractAssembly& m_assembly;
	BuiltinContext& m_builtinContext;
	bool const m_useNamedLabelsForFunctions = true;

	BlockGenerationInfo* m_currentBlockInfo;
	Stack* m_stack;

	Stack combineStack(Stack const& _stack1, Stack const& _stack2);
};

StackLayoutGenerator::StackLayoutGenerator(
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
		{}
	};
	std::cout << std::endl << std::endl;
	std::cout << "BACKWARD CODEGEN" << std::endl;
	std::cout << std::endl << std::endl;

	{
		StackLayoutGenerator stackLayoutGenerator{
			context,
			_assembly,
			_builtinContext,
			_useNamedLabelsForFunctions
		};
		stackLayoutGenerator(*context.dfg->entry);
	}

	std::cout << std::endl << std::endl;
	std::cout << "FORWARD CODEGEN" << std::endl;
	std::cout << std::endl << std::endl;

	CodeGenerator::run(_assembly, _builtinContext, context, *context.dfg->entry);
}

void StackLayoutGenerator::operator()(DFG::FunctionCall const& _call)
{
	std::cout << "B: function call " << _call.functionCall->functionName.name.str() << ": " << stackToString(*m_stack) << std::endl;

	ScopedSaveAndRestore restoreStack(m_stack, nullptr);
	ScopedSaveAndRestore restoreBlockInfo(m_currentBlockInfo, nullptr);
	DFG::FunctionInfo const& functionInfo = m_context.dfg->functions.at(_call.function);
	(*this)(*functionInfo.entry);
}

void StackLayoutGenerator::operator()(DFG::BuiltinCall const& _call)
{
	std::cout << "B: bultin call " << _call.functionCall->functionName.name.str() << ": " << stackToString(*m_stack) << std::endl;
}

void StackLayoutGenerator::operator()(DFG::Assignment const& _assignment)
{
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

	vector<int> targetPositions(_operation.output.size(), -1);
	size_t numToKeep = 0;
	for (size_t idx: ranges::views::iota(0u, targetPositions.size()))
		if (auto offset = util::findOffset(*m_stack, _operation.output.at(idx)))
		{
			targetPositions[idx] = static_cast<int>(*offset);
			++numToKeep;
		}

	struct PreviousSlot { size_t slot; };
	vector<variant<PreviousSlot, int>> layout;
	// Before the call the desired stack (below the call arguments) has size m_stack->size() - numToKeep
	for (size_t slot: ranges::views::iota(0u, m_stack->size() - numToKeep))
		layout.emplace_back(PreviousSlot{slot});
	// The call produces values with known target positions.
	layout += targetPositions;

	// TODO: argue that util::permute works with this kind of _getTargetPosition.
	util::permute(static_cast<unsigned>(layout.size()), [&](unsigned _i) {
		// For call return values the target position is known.
		if (int* pos = get_if<int>(&layout.at(_i)))
			return *pos;
		// Previous arguments can stay where they are.
		return static_cast<int>(_i);
	}, [&](unsigned _i) {
		std::swap(layout.back(), layout[layout.size() - _i - 1]);
	}, [&]() {
		layout.pop_back();
	});

	// Now we can construct the ideal layout before the declaration.
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
		if (std::holds_alternative<LiteralSlot>(slot))
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
	// TODO: how can we be sure that there are no more duplicates after compression?
	std::cout << "Operation pre after compress: " << stackToString(*m_stack) << "   " << m_stack << std::endl;
}

void StackLayoutGenerator::operator()(DFG::BasicBlock const& _block)
{
	if (m_context.blockInfos.count(&_block))
		return;
	ScopedSaveAndRestore currentBlockInfoRestore(m_currentBlockInfo, &m_context.stagedBlocks.emplace_back(BlockGenerationInfo{&_block}));
	ScopedSaveAndRestore stackRestore(m_stack, nullptr);
	m_context.blockInfos.emplace(&_block, *m_currentBlockInfo);

	Stack currentStack;

	std::cout << "B: BLOCK: " << &_block << std::endl;
	{
		std::visit(util::GenericVisitor{
			[&](std::monostate)
			{
				m_currentBlockInfo->exitLayout = currentStack;
			},
			[&](DFG::BasicBlock::Jump const& _jump)
			{
				(*this)(*_jump.target);
				BlockGenerationInfo& targetInfo = m_context.blockInfos.at(_jump.target);
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
				currentStack = *targetInfo.entryLayout;
				m_currentBlockInfo->exitLayout = currentStack;
			},
			[&](DFG::BasicBlock::ConditionalJump const& _conditionalJump)
			{
				(*this)(*_conditionalJump.zero);
				auto& zeroInfo = m_context.blockInfos.at(_conditionalJump.zero);
				(*this)(*_conditionalJump.nonZero);
				auto& nonZeroInfo = m_context.blockInfos.at(_conditionalJump.nonZero);
				yulAssert(zeroInfo.entryLayout.has_value() && nonZeroInfo.entryLayout.has_value(), "");
				currentStack = combineStack(*zeroInfo.entryLayout, *nonZeroInfo.entryLayout);
				m_currentBlockInfo->exitLayout = currentStack;
				currentStack.emplace_back(_conditionalJump.condition);
			},
			[&](DFG::BasicBlock::FunctionReturn const& _functionReturn)
			{
				// TODO: probably not needed here.
				yulAssert(_functionReturn.info, "");
				currentStack = _functionReturn.info->returnVariables | ranges::views::transform([](auto const& _varSlot){
					return StackSlot{_varSlot};
				}) | ranges::to<Stack>;
				currentStack.emplace_back(ReturnLabelSlot{});
				m_currentBlockInfo->exitLayout = currentStack;
			},
			[](DFG::BasicBlock::Stop const&) { yulAssert(false, "STOP EXIT"); },
			[](DFG::BasicBlock::Revert const&) { yulAssert(false, "REVERT EXIT"); }
		}, _block.exit);
	}

	m_stack = &currentStack;

	std::cout << "B: EXIT LAYOUT (" << &_block << "): " << stackToString(currentStack) << std::endl;

	for(auto& operation: _block.operations | ranges::views::reverse)
		(*this)(operation);

	std::cout << "B: ENTRY LAYOUT (" << &_block << "): " << stackToString(currentStack) << std::endl;

	m_currentBlockInfo->entryLayout = currentStack;
}

Stack StackLayoutGenerator::combineStack(Stack const& _stack1, Stack const& _stack2)
{
	// TODO: there is probably a better way than brute-forcing.
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
		std::cout << "  CANDIDATE: " << stackToString(_candidate) << ": " << numOps << " swaps." << std::endl;
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
