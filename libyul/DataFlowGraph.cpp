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
#include <libyul/DataFlowGraph.h>
#include <libyul/AST.h>
#include <libyul/Utilities.h>
#include <libyul/AsmPrinter.h>

#include <libsolutil/cxx20.h>
#include <libsolutil/Visitor.h>

#include <range/v3/range/conversion.hpp>
#include <range/v3/view/enumerate.hpp>
#include <range/v3/view/drop_last.hpp>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/reverse.hpp>
#include <range/v3/view/take_last.hpp>
#include <range/v3/view/transform.hpp>
#include <boost/algorithm/string/join.hpp>

using namespace solidity;
using namespace solidity::yul;
using namespace std;

std::unique_ptr<DFG> DataFlowGraphBuilder::build(
	AsmAnalysisInfo& _analysisInfo,
	EVMDialect const& _dialect,
	Block const& _block
)
{
	auto result = std::make_unique<DFG>();
	result->entry = &result->makeBlock();

	DataFlowGraphBuilder builder(*result, _analysisInfo, _dialect);
	builder.m_currentBlock = result->entry;
	builder(_block);
	result->exits = builder.m_exits;
	result->exits.insert(builder.m_currentBlock);
	return result;
}

DataFlowGraphBuilder::DataFlowGraphBuilder(
	DFG& _graph,
	AsmAnalysisInfo& _analysisInfo,
	EVMDialect const& _dialect
):
m_graph(_graph),
m_info(_analysisInfo),
m_dialect(_dialect)
{
}

DFG::Expression DataFlowGraphBuilder::operator()(Literal const& _literal)
{
	return DFG::Literal{_literal.debugData, valueOfLiteral(_literal) };
}

DFG::Expression DataFlowGraphBuilder::operator()(Identifier const& _identifier)
{
	return DFG::Variable{_identifier.debugData, &lookupVariable(_identifier.name)};
}

DFG::Expression DataFlowGraphBuilder::operator()(FunctionCall const& _call)
{
	yulAssert(m_scope, "");

	if (BuiltinFunctionForEVM const* builtin = m_dialect.builtin(_call.functionName.name))
		return DFG::BuiltinCall{
			_call.debugData,
			builtin,
			&_call,
			_call.arguments |
			ranges::views::enumerate |
			ranges::views::filter(util::mapTuple([&](size_t idx, auto const&) {
				return !builtin->literalArgument(idx).has_value();
			})) |
			ranges::views::transform(util::mapTuple([&](size_t, Expression const& _expr) {
				return std::visit(*this, _expr);
			})) |
			ranges::to<vector<DFG::Expression>>,
			builtin->returns.size()
		};
	else
	{
		Scope::Function* function = nullptr;
		yulAssert(m_scope->lookup(_call.functionName.name, util::GenericVisitor{
			[](Scope::Variable&) { yulAssert(false, "Expected function name."); },
			[&](Scope::Function& _function) { function = &_function; }
		}), "Function name not found.");
		yulAssert(function, "");
		return DFG::FunctionCall{
			_call.debugData,
			function,
			_call.arguments |
			ranges::views::transform([&](Expression const& _expr) {
				return std::visit(*this, _expr);
			}) |
			ranges::to<vector<DFG::Expression>>,
			function->returns.size()
		};
	}
}

void DataFlowGraphBuilder::operator()(VariableDeclaration const& _varDecl)
{
	yulAssert(m_currentBlock, "");
	m_currentBlock->statements.emplace_back(DFG::Declaration{
		_varDecl.debugData,
		_varDecl.variables | ranges::views::transform(
			[&](TypedName const &_var)
			{
				return DFG::Variable{_var.debugData, &std::get<Scope::Variable>(m_scope->identifiers.at(_var.name))};
			}
		) | ranges::to<vector<DFG::Variable>>,
		_varDecl.value ? optional(std::visit(*this, *_varDecl.value)) : std::nullopt
	});
}
void DataFlowGraphBuilder::operator()(Assignment const& _assignment)
{
	yulAssert(m_currentBlock, "");
	m_currentBlock->statements.emplace_back(DFG::Assignment{
		_assignment.debugData,
		_assignment.variableNames | ranges::views::transform([&](Identifier const& _var) {
			return DFG::Variable{_var.debugData, &lookupVariable(_var.name)};
		}) | ranges::to<vector<DFG::Variable>>,
		std::visit(*this, *_assignment.value)
	});
}
void DataFlowGraphBuilder::operator()(ExpressionStatement const& _exprStmt)
{
	yulAssert(m_currentBlock, "");
	m_currentBlock->statements.emplace_back(DFG::ExpressionStatement{_exprStmt.debugData, std::visit(*this, _exprStmt.expression)});
}

void DataFlowGraphBuilder::operator()(Block const& _block)
{
	ScopedSaveAndRestore saveScope(m_scope, m_info.scopes.at(&_block).get());
	for(auto const& statement: _block.statements)
		std::visit(*this, statement);
}

std::pair<DFG::BasicBlock*, DFG::BasicBlock*> DataFlowGraphBuilder::makeConditionalJump(DFG::Expression _condition)
{
	DFG::BasicBlock& nonZero = m_graph.makeBlock();
	DFG::BasicBlock& zero = m_graph.makeBlock();
	makeConditionalJump(move(_condition), nonZero, zero);
	return {&nonZero, &zero};
}
void DataFlowGraphBuilder::makeConditionalJump(DFG::Expression _condition, DFG::BasicBlock& _nonZero, DFG::BasicBlock& _zero)
{
	yulAssert(m_currentBlock, "");
	m_currentBlock->exit = DFG::BasicBlock::ConditionalJump{
		move(_condition),
		&_nonZero,
		&_zero
	};
	_nonZero.entries.emplace_back(m_currentBlock);
	_zero.entries.emplace_back(m_currentBlock);
	m_currentBlock = nullptr;
}
void DataFlowGraphBuilder::jump(DFG::BasicBlock& _target)
{
	yulAssert(m_currentBlock, "");
	m_currentBlock->exit = DFG::BasicBlock::Jump{&_target};
	_target.entries.emplace_back(m_currentBlock);
	m_currentBlock = &_target;
}
void DataFlowGraphBuilder::operator()(If const& _if)
{
	auto&& [ifBranch, afterIf] = makeConditionalJump(std::visit(*this, *_if.condition));
	m_currentBlock = ifBranch;
	(*this)(_if.body);
	jump(*afterIf);
}

void DataFlowGraphBuilder::operator()(Switch const& _switch)
{
	yulAssert(m_currentBlock, "");
	size_t ghostVariableId = m_graph.ghostVariables.size();
	DFG::Variable ghostVariable{_switch.debugData, &m_graph.ghostVariables.emplace_back(
		Scope::Variable{""_yulstring, YulString("GHOST[" + to_string(ghostVariableId) + "]")}
	)};
	m_currentBlock->statements.emplace_back(
		DFG::Declaration{
			_switch.debugData,
			{ghostVariable},
			std::visit(*this, *_switch.expression)
		}
	);
	auto makeValueCompare = [&](Literal const& _value) {
		yul::FunctionCall const& ghostCall = m_graph.ghostCalls.emplace_back(yul::FunctionCall{
			_value.debugData,
			yul::Identifier{_value.debugData, "eq"_yulstring},
			{Identifier{_value.debugData, YulString("GHOST[" + to_string(ghostVariableId) + "]")}, _value}
		});
		return DFG::BuiltinCall{
			_value.debugData, m_dialect.equalityFunction({}),
			&ghostCall,
			{ghostVariable, (*this)(_value)},
			1
		};
	};
	DFG::BasicBlock& afterSwitch = m_graph.makeBlock();
	for(auto const& switchCase: _switch.cases | ranges::views::drop_last(1))
	{
		yulAssert(switchCase.value, "");
		auto&& [caseBranch, elseBranch] = makeConditionalJump(makeValueCompare(*switchCase.value));
		m_currentBlock = caseBranch;
		(*this)(switchCase.body);
		jump(afterSwitch);
		m_currentBlock = elseBranch;
	}
	Case const& switchCase = _switch.cases.back();
	if (switchCase.value)
	{
		DFG::BasicBlock& caseBranch = m_graph.makeBlock();
		makeConditionalJump(makeValueCompare(*switchCase.value), caseBranch, afterSwitch);
		m_currentBlock = &caseBranch;
		(*this)(switchCase.body);
	}
	else
		(*this)(switchCase.body);
	jump(afterSwitch);
}

void DataFlowGraphBuilder::operator()(ForLoop const& _loop)
{
	(*this)(_loop.pre);

	DFG::BasicBlock& loopCondition = m_graph.makeBlock();
	DFG::BasicBlock& loopBody = m_graph.makeBlock();
	DFG::BasicBlock& post = m_graph.makeBlock();
	DFG::BasicBlock& afterLoop = m_graph.makeBlock();

	ScopedSaveAndRestore scopedSaveAndRestore(m_forLoopInfo, ForLoopInfo{&afterLoop, &post});

	jump(loopCondition);
	makeConditionalJump(std::visit(*this, *_loop.condition), loopBody, afterLoop);
	m_currentBlock = &loopBody;
	(*this)(_loop.body);
	jump(post);
	(*this)(_loop.post);
	jump(loopCondition);

	m_currentBlock = &afterLoop;
}

void DataFlowGraphBuilder::operator()(Break const&)
{
	yulAssert(m_forLoopInfo.has_value(), "");
	jump(*m_forLoopInfo->afterLoop);
	m_currentBlock = &m_graph.makeBlock();
}

void DataFlowGraphBuilder::operator()(Continue const&)
{
	yulAssert(m_forLoopInfo.has_value(), "");
	jump(*m_forLoopInfo->post);
	m_currentBlock = &m_graph.makeBlock();
}


void DataFlowGraphBuilder::operator()(Leave const&)
{
	yulAssert(m_currentFunctionExit, "");
	jump(*m_currentFunctionExit);
	m_currentBlock = &m_graph.makeBlock();
}

void DataFlowGraphBuilder::operator()(FunctionDefinition const& _function)
{
	yulAssert(m_scope, "");
	yulAssert(m_scope->identifiers.count(_function.name), "");
	Scope::Function& function = std::get<Scope::Function>(m_scope->identifiers.at(_function.name));

	DFG::FunctionInfo& info = m_graph.functions[&function] = DFG::FunctionInfo{
		_function.debugData,
		&m_graph.makeBlock(),
		{},
		{},
		{}
	};

	yulAssert(m_info.scopes.at(&_function.body), "");
	Scope* virtualFunctionScope = m_info.scopes.at(m_info.virtualBlocks.at(&_function).get()).get();
	yulAssert(virtualFunctionScope, "");
	for (auto const& v: _function.parameters)
		info.parameters.emplace_back(DFG::Variable{
			v.debugData,
			&std::get<Scope::Variable>(virtualFunctionScope->identifiers.at(v.name))
		});
	for (auto const& v: _function.returnVariables)
		info.returnVariables.emplace_back(DFG::Variable{
			v.debugData,
			&std::get<Scope::Variable>(virtualFunctionScope->identifiers.at(v.name))
		});

	DataFlowGraphBuilder builder{m_graph, m_info, m_dialect};
	builder.m_currentFunctionExit = &m_graph.makeBlock();
	builder.m_currentBlock = info.entry;
	builder(_function.body);
	builder.jump(*builder.m_currentFunctionExit);
	info.exits = builder.m_exits;
	info.exits.insert(builder.m_currentFunctionExit);
}

Scope::Variable const& DataFlowGraphBuilder::lookupVariable(YulString _name) const
{
	yulAssert(m_scope, "");
	Scope::Variable *var = nullptr;
	if (m_scope->lookup(_name, util::GenericVisitor{
		[&](Scope::Variable& _var)
		{
			var = &_var;
		},
		[](Scope::Function&)
		{
			yulAssert(false, "Function not removed during desugaring.");
		}
	}))
	{
		yulAssert(var, "");
		return *var;
	};
	yulAssert(false, "External identifier access unimplemented.");

}
