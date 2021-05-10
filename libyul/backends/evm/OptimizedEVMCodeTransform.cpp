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

#include <range/v3/view/drop_last.hpp>
#include <range/v3/view/enumerate.hpp>
#include <range/v3/view/map.hpp>
#include <range/v3/view/reverse.hpp>
#include <range/v3/view/take.hpp>
#include <range/v3/view/take_last.hpp>

using namespace solidity;
using namespace solidity::yul;
using namespace std;

namespace
{
template<typename TargetLayout, typename Swap, typename Pop>
void shuffleStack(std::vector<StackSlot>& _stack, TargetLayout&& _targetLayout, Swap _swap, Pop _pop)
{
	// TODO: make resilient against duplicates.
	std::vector<int> permutation(_stack.size(), -1);
	for (auto&& [idx, currentSlot]: _stack | ranges::views::enumerate)
		if (auto offset = util::findOffset(_targetLayout, currentSlot))
			permutation[idx] = static_cast<int>(*offset);

	util::permute(
		static_cast<unsigned>(permutation.size()),
		[&](unsigned _i) { return permutation.at(_i); },
		[&](unsigned _i)
		{
			std::swap(permutation.back(), permutation.at(permutation.size() - _i - 1));
			std::swap(_stack.back(), _stack.at(_stack.size() - _i - 1));
			_swap(_i);
		},
		[&]()
		{
			permutation.pop_back();
			_stack.pop_back();
			_pop();
		}
	);
	//yulAssert(_stack == _targetLayout, "");
}
void createStackLayout(std::vector<StackSlot>& _currentStack, std::vector<StackSlot> const& _targetStack, AbstractAssembly& _assembly)
{
	// TODO: make resilient against duplicates.
	if (_currentStack == _targetStack)
		return;
	std::cout << "CREATE STACK LAYOUT: " << OptimizedCodeTransform::stackToString(_targetStack) << " FROM " << OptimizedCodeTransform::stackToString(_currentStack) << std::endl;
	shuffleStack(_currentStack, _targetStack | ranges::views::take(_currentStack.size()), [&](unsigned _i) {
		_assembly.appendInstruction(evmasm::swapInstruction(_i));
	}, [&]() { _assembly.appendInstruction(evmasm::Instruction::POP); });
	for (auto const& newSlot: _targetStack | ranges::views::take_last(_targetStack.size() - _currentStack.size()))
	{
		if (auto offset = util::findOffset(_currentStack, newSlot))
			_assembly.appendInstruction(evmasm::dupInstruction(static_cast<unsigned>(_currentStack.size() - *offset)));
		else
		{
			auto const* literal = get_if<LiteralSlot>(&newSlot);
			yulAssert(literal, "");
			_assembly.setSourceLocation(locationOf(*literal));
			_assembly.appendConstant(literal->value);
		}
		_currentStack.emplace_back(newSlot);
	}
	std::cout << "CREATED STACK LAYOUT: " << OptimizedCodeTransform::stackToString(_currentStack) << std::endl;
}
}

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
	}

	CodeGenerationContext generationContext{_assembly, _builtinContext, {}};
	for(auto& block: context.stagedBlocks)
		for (auto const& generator: block.generators | ranges::views::reverse)
			(*generator)(generationContext);

	/*
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
		std::cout << "[function " << function->name.str() << "]" << std::endl;
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
		std::cout << "[end function " << function->name.str() << "]" << std::endl;
		_assembly.appendJump(-static_cast<int>(info.returnVariables.size()), AbstractAssembly::JumpType::OutOfFunction);
	}
	 */
}

void OptimizedCodeTransform::operator()(DFG::BasicBlock const& _block)
{
	if (m_context.blockInfos.count(&_block))
		return;
	m_currentBlockInfo = &m_context.stagedBlocks.emplace_back(BlockGenerationInfo{});
	m_context.blockInfos.emplace(&_block, *m_currentBlockInfo);

	std::visit(util::GenericVisitor{
		[](std::monostate) {},
		[&](DFG::BasicBlock::Jump const& _jump)
		{
			(*this)(*_jump.target);
			m_currentBlockInfo->exitLayout = m_context.blockInfos.at(_jump.target).entryLayout;
		},
		[&](DFG::BasicBlock::ConditionalJump const& _conditonalJump)
		{
			(*this)(*_conditonalJump.zero);
			auto& zeroLayout = m_context.blockInfos.at(_conditonalJump.zero).entryLayout;
			(*this)(*_conditonalJump.nonZero);
			auto& nonZeroLayout = m_context.blockInfos.at(_conditonalJump.nonZero).entryLayout;
			m_currentBlockInfo->exitLayout = combineStack(zeroLayout, nonZeroLayout);
		},
		[&](DFG::BasicBlock::FunctionReturn const& _functionReturn)
		{
			yulAssert(_functionReturn.info, "");
			m_currentBlockInfo->exitLayout = _functionReturn.info->returnVariables | ranges::views::transform([](DFG::Variable const& _var){
				return VariableSlot{&_var};
			}) | ranges::to<Stack>;
			m_currentBlockInfo->exitLayout.emplace_back(ReturnLabelSlot{});
		},
		[](DFG::BasicBlock::Stop const&) { },
		[](DFG::BasicBlock::Revert const&) { }
	}, _block.exit);

	m_currentBlockInfo->entryLayout = m_currentBlockInfo->exitLayout;
	m_stack = &m_currentBlockInfo->entryLayout;

	for(auto& statement: _block.statements | ranges::views::reverse)
	{
		std::visit(*this, statement);

		std::cout << "B: before stack compression " << stackToString(*m_stack) << std::endl;
		size_t notNeeded = 0;
		for (auto&& [position, stackSlot]: (*m_stack | ranges::views::enumerate) | ranges::views::reverse)
		{
			auto offset = util::findOffset(*m_stack, stackSlot);
			if ((offset && *offset < position) || holds_alternative<LiteralSlot>(stackSlot))
				++notNeeded;
			else
				break;
		}
		stage([layout = *m_stack](CodeGenerationContext& _context) {
			std::cout << "F statement preparation." << std::endl;
			createStackLayout(_context.stack, layout, _context.assembly);
		});
		pop(notNeeded);
		std::cout << "B: after stack compression " << stackToString(*m_stack) << std::endl;
	}

	stage([entryLayout = *m_stack](CodeGenerationContext& _context) {
		std::cout << "F block: current layout: " << stackToString(_context.stack) << " wanted: " << stackToString(entryLayout) << std::endl;
		createStackLayout(_context.stack, entryLayout, _context.assembly);
		yulAssert(_context.stack == entryLayout, "");
	});

}

void OptimizedCodeTransform::operator()(DFG::Declaration const& _declaration)
{
	std::cout << "B declaration ";
	for (auto const& v: _declaration.variables)
		std::cout << v.variable->name.str() << " ";
	std::cout << " pre: " << stackToString(*m_stack) << std::endl;
	// Determine the target positions of each declared variable and count the number of variables to be kept.
	// auto& targetPositions = _declaration.targetPositions.emplace(vector<int>(_declaration.variables.size(), -1));
	vector<int> targetPositions(_declaration.variables.size(), -1);
	size_t numToKeep = 0;
	for (auto&& [i, var]: _declaration.variables | ranges::views::enumerate)
		if (auto targetPosition = util::findOffset(*m_stack, StackSlot{VariableSlot{&var}}))
		{
			std::cout << "B: decl: KEEP: " << var.variable->name.str() << " " << i << " -> " << *targetPosition << std::endl;
			targetPositions[i] = static_cast<int>(*targetPosition);
			++numToKeep;
		}

	struct PreviousSlot { size_t x; };
	vector<std::variant<PreviousSlot, int>> layout;
	// Before the declaration the stack has size m_currentStack->size() - numToKeep
	for(size_t i = 0; i < m_stack->size() - numToKeep; ++i)
		layout.emplace_back(PreviousSlot{i});
	// Evaluating the declaration value adds variables with known target positions.
	layout += targetPositions;

	std::cout << "Layout: ";
	for (auto x: layout)
	{
		std::visit(util::GenericVisitor{
			[](PreviousSlot y) { std::cout << "P(" << y.x << ") "; },
			[](int y) { std::cout << y << " "; }
		}, x);
	}
	std::cout << std::endl;

	// Create a permutation of the declared variables to their target positions.
	// Previous variables are assumed to be already in place.
	std::vector<evmasm::Instruction> instructions;
	util::permute(static_cast<unsigned>(layout.size()), [&](unsigned _i) {
		if (int* x = get_if<int>(&layout.at(_i)))
			return *x;
		return static_cast<int>(_i);
	}, [&](unsigned _i) {
		std::cout << "B decl SWAP" << _i << std::endl;
		instructions.emplace_back(evmasm::swapInstruction(_i));
		std::swap(layout.back(), layout[layout.size() - _i - 1]);
	}, [&]() { std::cout << "B decl POP" << std::endl; layout.pop_back(); instructions.emplace_back(evmasm::Instruction::POP); });
	stage([declaration = &_declaration, instructions = std::move(instructions)](CodeGenerationContext& _context) {
		std::cout << "F: Before declare: " << stackToString(_context.stack) << std::endl;
		for (auto&& [slot, var]: ranges::zip_view(_context.stack | ranges::views::take_last(declaration->variables.size()), declaration->variables))
			slot = VariableSlot{&var};
		std::cout << "F: After initial assign: " << stackToString(_context.stack) << std::endl;
		for (auto instruction: instructions)
		{
			_context.assembly.appendInstruction(instruction);
			if (instruction == evmasm::Instruction::POP)
				_context.stack.pop_back();
			else
				std::swap(_context.stack.back(), _context.stack.at(_context.stack.size() - evmasm::getSwapNumber(instruction)));
		}
		std::cout << "F: After swapping: " << stackToString(_context.stack) << std::endl;
	});

	// Now we can construct the ideal layout before the declaration.
	// "layout" has the declared variables in the desired position and
	// for any PreviousSlot{x}, x yields the ideal place of the slot before the declaration.
	vector<optional<StackSlot>> idealLayout(m_stack->size() - numToKeep, nullopt);
	for (auto const& [slot, idealPosition]: ranges::zip_view(*m_stack, layout))
		if (PreviousSlot* previousSlot = std::get_if<PreviousSlot>(&idealPosition))
			idealLayout.at(previousSlot->x) = slot;

	m_stack->resize(idealLayout.size());
	for (auto&& [slot, idealSlot]: ranges::zip_view(*m_stack, idealLayout))
	{
		yulAssert(idealSlot.has_value(), "");
		slot = *idealSlot;
	}

	if (_declaration.value)
		std::visit(*this, *_declaration.value);


	std::cout << "B declaration ";
	for (auto const& v: _declaration.variables)
		std::cout << v.variable->name.str() << " ";
	std::cout << " post: " << stackToString(*m_stack) << std::endl;

}

void OptimizedCodeTransform::operator()(DFG::Assignment const& _assignment)
{
	std::visit(*this, _assignment.value);

	/*
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
		auto depth = variableStackDepth(var);
		if (depth)
			m_assembly.appendInstruction(evmasm::swapInstruction(static_cast<unsigned>(*depth)));
		m_assembly.appendInstruction(evmasm::Instruction::POP);
		m_stack.pop_back();
	}

	std::cout << " after assign: " << stackToString(m_stack) << std::endl;
	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
	*/
}

void OptimizedCodeTransform::operator()(DFG::ExpressionStatement const& _stmt)
{
	std::visit(*this, _stmt.expression);
	stage([loc = locationOf(_stmt)](CodeGenerationContext const& _context) {
		std::cout << "F: Expr stmt pre: " << stackToString(_context.stack) << std::endl;
		_context.assembly.setSourceLocation(loc);
	});
	/*
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
	 */
}

void OptimizedCodeTransform::operator()(DFG::BuiltinCall const& _call)
{
	stage([call = &_call](CodeGenerationContext& _context) {
		std::cout << "F: Builtin call " << call->functionCall->functionName.name.str() << " pre: " << stackToString(_context.stack) << std::endl;
		_context.assembly.setSourceLocation(locationOf(*call));
		call->builtin->generateCode(*call->functionCall, _context.assembly, _context.builtinContext, [](auto&&){});
		for (size_t i = 0; i < call->arguments.size(); ++i)
			_context.stack.pop_back();
		for (size_t i = 0; i < call->returns; ++i)
			_context.stack.emplace_back(TemporarySlot{call, i});
		std::cout << "F: Builtin call " << call->functionCall->functionName.name.str() << " post: " << stackToString(_context.stack) << std::endl;
	});

	std::cout << "B: Builtin call " << _call.functionCall->functionName.name.str() << " post: " << stackToString(*m_stack) << std::endl;

	for(auto& argument: _call.arguments)
		std::visit(*this, argument);

	/*stage([requiredStackTop = *m_stack | ranges::views::take(_call.arguments.size()) | ranges::to<Stack>](CodeGenerationContext& _context) {
		std::cout << "F: verify stack " << stackToString(_context.stack) << " has top " << stackToString(requiredStackTop) << std::endl;
		// TODO: better ranges equality comparsion.
		yulAssert((_context.stack | ranges::views::take_last(requiredStackTop.size()) | ranges::to<Stack>) == requiredStackTop, "");
	});*/

	std::cout << "B: Builtin call " << _call.functionCall->functionName.name.str() << " pre: " << stackToString(*m_stack) << std::endl;

	/*
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
	 */
}

void OptimizedCodeTransform::operator()(DFG::FunctionCall const&)
{
	/*
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
	 */
}

void OptimizedCodeTransform::operator()(DFG::Literal const& _literal)
{
	std::cout << "B: literal post " << stackToString(*m_stack) << std::endl;
	m_stack->emplace_back(LiteralSlot{_literal.value, _literal.debugData});
	std::cout << "B: literal pre " << stackToString(*m_stack) << std::endl;
}

void OptimizedCodeTransform::operator()(DFG::Variable const& _variable)
{
	std::cout << "B: variable " << _variable.variable->name.str() << " post " << stackToString(*m_stack) << std::endl;
//	auto offset = util::findOffset(*m_stack, StackSlot{VariableSlot{&_variable}});

	m_stack->emplace_back(VariableSlot{&_variable});
	std::cout << "B: variable " << _variable.variable->name.str() << " pre " << stackToString(*m_stack) << std::endl;
}

AbstractAssembly::LabelID OptimizedCodeTransform::getFunctionLabel(Scope::Function const& _function)
{
	DFG::FunctionInfo const& functionInfo = m_context.dfg->functions.at(&_function);
	(*this)(functionInfo);
	auto label = m_context.blockInfos.at(functionInfo.entry).label;
	yulAssert(label.has_value(), "");
	return *label;
}

void OptimizedCodeTransform::operator()(DFG::FunctionInfo const& _functionInfo)
{
	if (m_context.blockInfos.count(_functionInfo.entry))
		return;

	(*this)(*_functionInfo.entry);
	BlockGenerationInfo& info = m_context.blockInfos.at(_functionInfo.entry);

	info.label = m_useNamedLabelsForFunctions ?
		m_assembly.namedLabel(
			_functionInfo.function->name.str(),
			_functionInfo.function->arguments.size(),
			_functionInfo.function->returns.size(),
			{}
		) : m_assembly.newLabelId();


	yulAssert(false, "TODO");
}

void OptimizedCodeTransform::shuffleStackTo(std::vector<StackSlot> const&)
{
	yulAssert(false, "");

	/*
	std::cout << "SHUFFLE " << stackToString(m_stack) << std::endl;
	std::cout << "     TO " << stackToString(_target) << std::endl;
	auto matches = util::GenericVisitor{
		[](ReturnLabelSlot const& _r1, ReturnLabelSlot const& _r2) {
			return _r1.call == _r2.call;
		},
		[](VariableSlot const& _v1, VariableSlot const& _v2) {
			return _v1.variable->variable == _v2.variable->variable;
		},
		[](LiteralSlot const& _l1, LiteralSlot const& _l2) {
			return _l1.value == _l2.value;
		},
		[](auto const&, auto const&) { return false; }
	};
	auto findSlot = [&](StackSlot const& _target) -> optional<size_t> {
		for (auto&& [pos, slot]: ranges::views::enumerate(m_stack))
			if (std::visit(matches, _target, slot))
				return pos;
		return nullopt;
	};

	std::vector<int> targetPositions(m_stack.size(), -1);
	{
		std::vector<int> added;
		for (auto&& [targetPosition, slot]: (_target | ranges::views::enumerate) | ranges::views::reverse)
			if (auto currentPosition = findSlot(slot))
				targetPositions.at(*currentPosition) = static_cast<int>(targetPosition);
			else
			{
				std::visit(util::GenericVisitor{
					[&](LiteralSlot const& _l)
					{
						m_stack.emplace_back(_l);
						m_assembly.appendConstant(_l.value);
					},
					[&](VariableSlot const& _l)
					{
						if (m_unallocatedReturnVariables.count(_l.variable->variable))
						{
							m_stack.emplace_back(_l);
							m_assembly.appendConstant(0);
							m_unallocatedReturnVariables.erase(_l.variable->variable);
						}
						else
							yulAssert(false, "");
					},
					[](auto const&) { yulAssert(false, ""); }
				}, slot);
				added.emplace_back(targetPosition);
			}
		targetPositions += move(added);
	}

	std::cout << "   after adding literals: " << stackToString(m_stack) << std::endl;
	std::cout << "   target positions: ";
	for(auto x: targetPositions) std::cout << x << " ";
	std::cout << std::endl;


	yulAssert(m_stack.size() >= _target.size(), "");

	util::permute(static_cast<unsigned>(targetPositions.size()), [&](unsigned _idx) { return targetPositions.at(_idx); }, [&](unsigned _depth) {
		std::swap(targetPositions.back(), targetPositions.at(targetPositions.size() - _depth - 1));
		std::swap(m_stack.back(), m_stack.at(m_stack.size() - _depth - 1));
		m_assembly.appendInstruction(evmasm::swapInstruction(_depth));
	}, [&]() {
		targetPositions.pop_back();
		m_stack.pop_back();
		m_assembly.appendInstruction(evmasm::Instruction::POP);
	});

	yulAssert(static_cast<int>(m_stack.size()) == m_assembly.stackHeight(), "Stack height mismatch.");
	 */
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

string OptimizedCodeTransform::stackToString(Stack const& _stack)
{
	string result("[ ");
	for (auto const& slot: _stack)
		result += stackSlotToString(slot) + ' ';
	result += ']';
	return result;
}

Stack OptimizedCodeTransform::combineStack(Stack const& _stack1, Stack const& _stack2)
{
	// TODO: there is probably a better way than brute-forcing.
	std::set<StackSlot> slotSet;
	slotSet += _stack1;
	slotSet += _stack2;
	std::vector<StackSlot> candidate(slotSet.begin(), slotSet.end());
	std::map<Stack, size_t> requiredSwaps;

	auto evaluate = [&](Stack const& _candidate) -> size_t {
		size_t numSwaps = 0;
		Stack testStack = _stack1;
		ScopedSaveAndRestore restoreStack(m_stack, &testStack);
		shuffleStack(testStack, _candidate, [&](unsigned) { ++numSwaps; }, [](){});
		testStack = _stack2;
		shuffleStack(testStack, _candidate, [&](unsigned) { ++numSwaps; }, [](){});
		return numSwaps;
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

	return requiredSwaps.begin()->first;
}