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
#include <range/v3/view/take_last.hpp>

using namespace solidity;
using namespace solidity::yul;
using namespace std;


// Debugging
namespace {
void PrintStackLayout(StackLayout const& _layout)
{
	std::cout << StackLayoutToString(_layout) << std::endl;
}

void markJunk(BasicBlock&, BasicBlock& _exit)
{
	static StackSlot junkSlot(JunkSlot{});

	std::set<Scope::Variable const*> liveVariables;
	std::cout << "MARK AS JUNK" << std::endl;
	for (auto const* stackSlot: _exit.finalLayout)
		if (VariableSlot const* variable = std::get_if<VariableSlot>(&stackSlot->content))
			liveVariables.insert(&variable->variable());

	PrintStackLayout(_exit.finalLayout);
	size_t operationArguments = 0;
	for (BasicBlockEntry& entry: _exit.content | ranges::views::reverse)
	{
		for (auto const*& stackSlot: entry.outputLayout | ranges::views::drop_last(operationArguments))
			if (VariableSlot const* variable = std::get_if<VariableSlot>(&stackSlot->content))
				if (!liveVariables.count(&variable->variable()))
					stackSlot = &junkSlot;
		for (auto const*& stackSlot: entry.outputLayout | ranges::views::take_last(operationArguments))
			if (VariableSlot const* variable = std::get_if<VariableSlot>(&stackSlot->content))
				liveVariables.insert(&variable->variable());
		operationArguments = 0;
		visit(util::GenericVisitor{
			[&](BuiltinCallOperation const& _builtinCall)
			{
				operationArguments = _builtinCall.properArgumentCount();
			},
			[&](FunctionCallOperation const& _functionCall)
			{
				operationArguments = _functionCall.function().arguments.size();
			},
			[](StackRequestOperation const&) {}
		}, entry.operation);

	}
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
	DataFlowGraph graph;
	BasicBlock* rootBlock = &graph.blocks.emplace_back();
	{
		DataFlowGraphBuilder dataFlowGraphBuilder{graph, _analysisInfo, _dialect, rootBlock};
		dataFlowGraphBuilder(_block);
		yulAssert(dataFlowGraphBuilder.lastBlock(), "");
		markJunk(*rootBlock, *dataFlowGraphBuilder.lastBlock());
		for (auto&& [functionEntry, functionExit]: graph.functions | ranges::views::values)
			markJunk(*functionEntry, *functionExit);
	}



	std::cout << std::endl << std::endl << "START CODEGEN" << std::endl << std::endl;

	OptimizedCodeTransformContext context{
		graph.functions,
		{rootBlock},
		{},
		{}
	};

	while (!context.queuedBlocks.empty())
	{
		BasicBlock const* block = context.queuedBlocks.front();
		context.queuedBlocks.erase(context.queuedBlocks.begin());
		if (context.generatedBlocks.count(block))
			continue;

		OptimizedCodeTransform transform{context, _assembly, _builtinContext, _useNamedLabelsForFunctions};
		transform.visit(*block);
	}
}

void OptimizedCodeTransform::changeCurrentStackLayout(StackLayout const& _targetLayout)
{
	std::cout << "Stack shuffling:" << std::endl;
	std::cout << "  From: " << StackLayoutToString(m_currentStack) << std::endl;
	std::cout << "  To:   " << StackLayoutToString(_targetLayout) << std::endl;

	while(m_assembly.stackHeight() < static_cast<int>(_targetLayout.size()))
		m_assembly.appendConstant(0);
	while(m_assembly.stackHeight() > static_cast<int>(_targetLayout.size()))
		m_assembly.appendInstruction(evmasm::Instruction::POP);
	m_currentStack = _targetLayout;
/*
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
		}*/
}


void OptimizedCodeTransform::visit(BasicBlockEntry const& _basicBlock)
{
	std::visit(util::GenericVisitor{
		[&](FunctionCallOperation const& _functionCall)
		{
			std::cout << "FUNCTION " << _functionCall.call().functionName.name.str() << std::endl;

			Scope::Function const& function = _functionCall.function();

			AbstractAssembly::LabelID returnLabel = m_assembly.newLabelId();
			m_assembly.appendLabelReference(returnLabel);
			AbstractAssembly::LabelID functionLabel;
			BasicBlock const* functionBlock = m_context.functionBlocks.at(&function).first;

			if (AbstractAssembly::LabelID* label = util::valueOrNullptr(m_context.blockLabels, functionBlock))
				functionLabel = *label;
			else
			{
				functionLabel = m_useNamedLabelsForFunctions ?
					m_assembly.namedLabel(_functionCall.call().functionName.name.str()) :
					m_assembly.newLabelId();
				m_context.blockLabels[functionBlock] = functionLabel;
				if (!m_context.generatedBlocks.count(functionBlock))
					m_context.queuedBlocks.push_back(functionBlock);
			}

			m_assembly.appendJumpTo(
				functionLabel,
				static_cast<int>(function.returns.size() - function.arguments.size()) - 1,
				AbstractAssembly::JumpType::IntoFunction
			);
			m_assembly.appendLabel(returnLabel);
		},
		[&](BuiltinCallOperation const& _builtinCallOperation)
		{
			std::cout << "BUILTIN " << _builtinCallOperation.call().functionName.name.str() << std::endl;
			_builtinCallOperation.builtin().generateCode(_builtinCallOperation.call(), m_assembly, m_builtinContext, [](auto const&){});
		},
		[&](StackRequestOperation const&) { changeCurrentStackLayout(_basicBlock.outputLayout); }
	}, _basicBlock.operation);
	m_currentStack = _basicBlock.outputLayout;
}

void OptimizedCodeTransform::visit(BasicBlock const& _basicBlock)
{
	m_context.generatedBlocks.insert(&_basicBlock);
	m_assembly.setStackHeight(static_cast<int>(_basicBlock.initialLayout.size()));
	m_currentStack = _basicBlock.initialLayout;

	if (AbstractAssembly::LabelID* label = util::valueOrNullptr(m_context.blockLabels, &_basicBlock))
		m_assembly.appendLabel(*label);

	std::cout << "BEGIN OF BASIC BLOCK ";
	PrintStackLayout(_basicBlock.initialLayout);

	for (auto const& entry: _basicBlock.content)
		visit(entry);

	std::cout << "END OF BASIC BLOCK ";
	changeCurrentStackLayout(_basicBlock.finalLayout);

	if (_basicBlock.exits.size() > 1)
	{
		yulAssert(_basicBlock.exits.size() == 2, "");
		if (AbstractAssembly::LabelID* id = util::valueOrNullptr(m_context.blockLabels, _basicBlock.exits.back()))
			m_assembly.appendJumpToIf(*id);
		else
		{
			yulAssert(!m_context.generatedBlocks.count(_basicBlock.exits.back()), "");
			m_assembly.appendJumpToIf(m_context.blockLabels[_basicBlock.exits.back()] = m_assembly.newLabelId());
			m_context.queuedBlocks.push_front(_basicBlock.exits.back());
		}
		m_currentStack.pop_back();
	}

	if (_basicBlock.isFunctionExit)
	{
		yulAssert(_basicBlock.exits.empty(), "");
		yulAssert(!m_currentStack.empty(), "");
		// TODO: yulAssert(holds_alternative<ReturnLabelSlot>(m_currentStack.back()->content), "");
		m_assembly.appendInstruction(evmasm::Instruction::JUMP);
	}
	else if (_basicBlock.exits.empty())
		m_assembly.appendInstruction(evmasm::Instruction::STOP);
	else
	{
		if (AbstractAssembly::LabelID* id = util::valueOrNullptr(m_context.blockLabels, _basicBlock.exits.front()))
			m_assembly.appendJumpTo(*id);
		else
		{
			if (_basicBlock.exits.front()->entries.size() > 1)
				m_context.blockLabels[_basicBlock.exits.front()] = m_assembly.newLabelId();
			else
			{
				yulAssert(_basicBlock.exits.front()->entries.size() == 1, "");
				yulAssert(_basicBlock.exits.front()->entries.front() == &_basicBlock, "");
			}
			yulAssert(!m_context.generatedBlocks.count(_basicBlock.exits.front()), "");
			visit(*_basicBlock.exits.front());
		}
	}
}