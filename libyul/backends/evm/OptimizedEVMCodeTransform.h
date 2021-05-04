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
public:
	void operator()(FunctionCallSlot const&);
	void operator()(ExternalIdentifierSlot const&);
	void operator()(LiteralSlot const&);
	void operator()(VariableSlot const&);
	void operator()(JunkSlot const&) { yulAssert(false, ""); }
	void operator()(ReturnLabelSlot const&) { yulAssert(false, ""); }
private:
	OptimizedCodeTransform(AbstractAssembly& _assembly, BuiltinContext& _builtinContext, bool _useNamedLabelsForFunctions);

	void visit(BasicBlock const& _basicBlock);
	void visit(BasicBlockEntry const& _basicBlock);
	void visit(StackSlot const& _expression);

	void changeCurrentStackLayout(StackLayout const& _targetLayout);

	AbstractAssembly& m_assembly;
	BuiltinContext& m_builtinContext;
	bool const m_useNamedLabelsForFunctions = true;
	StackSlot const* m_currentExpressionNode = nullptr;
	std::vector<StackSlot const*> m_currentStack;
	std::map<Scope::Function const*, AbstractAssembly::LabelID> m_neededFunctions;
	std::optional<AbstractAssembly::LabelID> m_currentFunctionExit;
	void popLast(size_t n)
	{
		while (n--)
			m_currentStack.pop_back();
	}
};

}
