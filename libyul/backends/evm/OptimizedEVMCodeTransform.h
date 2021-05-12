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

struct CodeGenerationContext
{
	AbstractAssembly& assembly;
	BuiltinContext& builtinContext;
	Stack stack;
	std::map<yul::FunctionCall const*, AbstractAssembly::LabelID> returnLabels;
};

struct BlockGenerator
{
	virtual ~BlockGenerator() {}
	virtual void operator()(CodeGenerationContext& _context) const = 0;
};

struct BlockGenerationInfo {
	DFG::BasicBlock const* block = nullptr;
	std::optional<AbstractAssembly::LabelID> label{};
	std::optional<Stack> entryLayout{};
	std::optional<Stack> exitLayout{};
};
struct OptimizedCodeTransformContext
{
	std::unique_ptr<DFG> dfg;
	std::list<BlockGenerationInfo> stagedBlocks;
	std::map<DFG::BasicBlock const*, BlockGenerationInfo&> blockInfos;
	struct OperationInfo
	{
		Stack entryStack;
		Stack exitStack;
	};
	std::map<DFG::Operation const*, OperationInfo> operationStacks;
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

	void operator()(DFG::Operation const& _operation);

	void operator()(DFG::BuiltinCall const& _builtinCall);
	void operator()(DFG::FunctionCall const& _functionCall);
	void operator()(DFG::Assignment const& _literal);

	void operator()(DFG::FunctionInfo const& _functionInfo);

private:
	OptimizedCodeTransform(OptimizedCodeTransformContext& _context, AbstractAssembly& _assembly, BuiltinContext& _builtinContext, bool _useNamedLabelsForFunctions);

	void visit(DFG::BasicBlock const& _block);

	AbstractAssembly::LabelID getFunctionLabel(Scope::Function const& _function);

	OptimizedCodeTransformContext& m_context;
	AbstractAssembly& m_assembly;
	BuiltinContext& m_builtinContext;
	bool const m_useNamedLabelsForFunctions = true;

	BlockGenerationInfo* m_currentBlockInfo;
	Stack* m_stack;

	void pop(size_t _amount = 1)
	{
		yulAssert(m_stack->size() >= _amount, "");
		while (_amount--)
			m_stack->pop_back();
	}

	void compressStack();
	Stack combineStack(Stack const& _stack1, Stack const& _stack2);

	// Debugging.
public:
	static std::string stackSlotToString(StackSlot const& _slot);
	static std::string stackToString(Stack const& _stack);
};

}
