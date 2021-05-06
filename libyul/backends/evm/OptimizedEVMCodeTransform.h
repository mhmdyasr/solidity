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
/**
 * Code generator for translating Yul / inline assembly to EVM.
 */

#pragma once

#include <libyul/backends/evm/EVMAssembly.h>

#include <libyul/backends/evm/EVMDialect.h>
#include <libyul/DataFlowGraph.h>
#include <libyul/AST.h>
#include <libyul/Scope.h>

#include <optional>
#include <stack>

namespace solidity::langutil
{
class ErrorReporter;
}

namespace solidity::yul
{
struct AsmAnalysisInfo;

struct ReturnLabelSlot { DFG::FunctionCall const* call = nullptr; };
struct VariableSlot { DFG::Variable const* variable = nullptr; };
struct LiteralSlot { u256 value; };
struct TemporarySlot { std::variant<DFG::FunctionCall const*, DFG::BuiltinCall const*> call; size_t idx; };
using StackSlot = std::variant<ReturnLabelSlot, VariableSlot, LiteralSlot, TemporarySlot>;

struct OptimizedCodeTransformContext
{
	std::unique_ptr<DFG> dfg;
	std::map<Scope::Function const*, AbstractAssembly::LabelID> functionEntries;
	std::list<Scope::Function const*> stagedFunctions;
	std::map<DFG::BasicBlock const*, AbstractAssembly::LabelID> blockLabels;
	std::list<std::pair<DFG::BasicBlock const*, std::vector<StackSlot>>> stagedBlocks;
};

class OptimizedCodeTransform
{
public:
	static void run(
		AbstractAssembly& _assembly,
		AsmAnalysisInfo& _analysisInfo,
		Block const& _block,
		EVMDialect const& _dialect,
		BuiltinContext& _builtinContext,
		ExternalIdentifierAccess const& _identifierAccess = ExternalIdentifierAccess(),
		bool _useNamedLabelsForFunctions = false
	);

	void operator()(DFG::BasicBlock const& _block);
	void operator()(DFG::Declaration const& _declaration);
	void operator()(DFG::Assignment const& _assignment);
	void operator()(DFG::ExpressionStatement const& _expression);

	void operator()(DFG::BuiltinCall const& _builtinCall);
	void operator()(DFG::FunctionCall const& _functionCall);
	void operator()(DFG::Literal const& _literal);
	void operator()(DFG::Variable const& _variable);

private:
	OptimizedCodeTransform(OptimizedCodeTransformContext& _context, AbstractAssembly& _assembly, BuiltinContext& _builtinContext, bool _useNamedLabelsForFunctions);

	void visit(DFG::BasicBlock const& _block);

	AbstractAssembly::LabelID getFunctionLabel(Scope::Function const& _function);

	size_t variableStackDepth(DFG::Variable const& _var);

	OptimizedCodeTransformContext& m_context;
	AbstractAssembly& m_assembly;
	BuiltinContext& m_builtinContext;
	bool const m_useNamedLabelsForFunctions = true;

	std::vector<StackSlot> m_stack;
	std::set<Scope::Variable const*> m_unallocatedReturnVariables;

	std::map<DFG::BasicBlock const*, AbstractAssembly::LabelID> m_jumpLabels;
	std::list<std::pair<DFG::BasicBlock const*, std::vector<StackSlot>>> m_stagedBlocks;

	void pop(size_t _amount = 1, bool _generateCode = false)
	{
		yulAssert(m_stack.size() >= _amount, "");
		while (_amount--)
		{
			m_stack.pop_back();
			if (_generateCode)
				m_assembly.appendInstruction(evmasm::Instruction::POP);
		}
	}

	void shuffleStackTo(std::vector<StackSlot> const& _target);

	// Debugging.
	static std::string stackSlotToString(StackSlot const& _slot);
	static std::string stackToString(std::vector<StackSlot> const& _stack);
};

}
