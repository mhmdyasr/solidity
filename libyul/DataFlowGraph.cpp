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
#include <libsolutil/Permutations.h>
#include <libsolutil/Visitor.h>

#include <range/v3/range/conversion.hpp>
#include <range/v3/view/enumerate.hpp>
#include <range/v3/view/drop_last.hpp>
#include <range/v3/view/filter.hpp>
#include <range/v3/view/iota.hpp>
#include <range/v3/view/map.hpp>
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

	StackLayoutGenerator::generate(*result->entry);
	for (auto& funInfo: result->functions | ranges::views::values)
		StackLayoutGenerator::generate(funInfo);

	return result;
}

void StackLayoutGenerator::generate(DFG::FunctionInfo& _info)
{
	StackLayoutGenerator generator{};
	generator(*_info.entry);
}

Stack StackLayoutGenerator::combineStacks(Stack const&, Stack const&)
{
	yulAssert(false, "");
	return {};
}

void StackLayoutGenerator::operator()(DFG::BasicBlock& _block)
{
	if (_block.stackLayout.has_value())
		return;
	DFG::BasicBlock::StackLayout& layout = _block.stackLayout.emplace();

	std::visit(util::GenericVisitor{
		[](std::monostate) {},
		[&](DFG::BasicBlock::Jump& _jump)
		{
			StackLayoutGenerator{}(*_jump.target);
			yulAssert(_jump.target->stackLayout.has_value(), "");
			layout.exit = _jump.target->stackLayout->entry;
		},
		[&](DFG::BasicBlock::ConditionalJump const& _conditonalJump)
		{
			StackLayoutGenerator{}(*_conditonalJump.zero);
			yulAssert(_conditonalJump.zero->stackLayout.has_value(), "");
			StackLayoutGenerator{}(*_conditonalJump.nonZero);
			yulAssert(_conditonalJump.nonZero->stackLayout.has_value(), "");
			layout.exit = combineStacks(_conditonalJump.zero->stackLayout->entry, _conditonalJump.nonZero->stackLayout->entry);
		},
		[&](DFG::BasicBlock::FunctionReturn const& _functionReturn)
		{
			yulAssert(_functionReturn.info, "");
			layout.exit = _functionReturn.info->returnVariables | ranges::views::transform([](DFG::Variable const& _var){
				return VariableSlot{&_var};
			}) | ranges::to<Stack>;
			layout.exit.emplace_back(ReturnLabelSlot{});
		},
		[](DFG::BasicBlock::Stop const&) { },
		[](DFG::BasicBlock::Revert const&) { }
	}, _block.exit);

	layout.entry = layout.exit;
	m_currentStack = &layout.entry;

	for(auto& statement: _block.statements | ranges::views::reverse)
	{
		std::visit(*this, statement);
	}

}

void StackLayoutGenerator::operator()(DFG::Assignment& _assignment)
{
	std::visit(*this, _assignment.value);
	while(!m_currentStack->empty() && std::holds_alternative<LiteralSlot>(m_currentStack->back()))
		m_currentStack->pop_back();
	// TODO: Variables that are assigned don't need to be kept on stack previously.
}

void StackLayoutGenerator::operator()(DFG::Declaration& _declaration)
{
	yulAssert(m_currentStack, "");
	yulAssert(!_declaration.targetLayout.has_value(), "");
	_declaration.targetLayout = *m_currentStack;
	// Determine the target positions of each declared variable, if any, and count the number of variables to be kept.
	//auto& targetPositions = _declaration.targetPositions.emplace(vector<int>(_declaration.variables.size(), -1));
	vector<int> targetPositions(_declaration.variables.size(), -1);
	set<unsigned> targetPositionSet;
	size_t numToKeep = 0;
	for (auto&& [position, slot]: *m_currentStack | ranges::views::enumerate)
		if (VariableSlot const* varSlot = get_if<VariableSlot>(&slot))
			for (auto&& [declaredVar, declaredVarPosition]: ranges::zip_view(_declaration.variables, targetPositions))
				if (declaredVar.variable == varSlot->variable->variable)
				{
					numToKeep++;
					targetPositionSet.insert(static_cast<unsigned>(position));
					declaredVarPosition = static_cast<int>(position);
				}

	struct PreviousSlot { size_t x; };
	vector<std::variant<PreviousSlot, int>> layout;
	// Before the declaration the stack has size m_currentStack->size() - numToKeep
	for(size_t i = 0; i < m_currentStack->size() - numToKeep; ++i)
		layout.emplace_back(PreviousSlot{i});
	// Evaluating the declaration value adds variables with known target positions.
	layout += targetPositions;

	// Simulate a permutation of the declared variables to their target positions.
	// Previous variables are assumed to be already in place, except they are in a slot for one of the declared variables.
	util::permute(static_cast<unsigned>(layout.size()), [&](unsigned _i) {
		if (int* x = get_if<int>(&layout.at(_i)))
			return *x;
		return static_cast<int>(_i);
	}, [&](unsigned _x) {
		std::swap(layout.back(), layout[layout.size() - _x - 1]);
	}, [&]() { layout.pop_back(); });

	// Now we can construct the ideal layout before the declaration.
	// "layout" has the declared variables in the desired position and
	// for any PreviousSlot{x}, x yields the ideal place of the slot before the declaration.
	vector<optional<StackSlot>> idealLayout(m_currentStack->size() - numToKeep, nullopt);
	for (auto const& [slot, idealPosition]: ranges::zip_view(*m_currentStack, layout))
		if (PreviousSlot* previousSlot = std::get_if<PreviousSlot>(&idealPosition))
			idealLayout.at(previousSlot->x) = slot;

	m_currentStack->resize(idealLayout.size());
	for (auto&& [slot, idealSlot]: ranges::zip_view(*m_currentStack, idealLayout))
	{
		yulAssert(idealSlot.has_value(), "");
		slot = *idealSlot;
	}
}

void StackLayoutGenerator::operator()(DFG::Variable& _variable)
{
	yulAssert(m_currentStack, "");
	for(auto const& slot: *m_currentStack)
		if (VariableSlot const* stackVar = get_if<VariableSlot>(&slot))
			if (stackVar->variable->variable == _variable.variable)
				return;
	m_currentStack->emplace_back(VariableSlot{&_variable});
}
void StackLayoutGenerator::operator()(DFG::Literal& _literal)
{
	yulAssert(m_currentStack, "");
	for(auto const& slot: *m_currentStack)
		if (LiteralSlot const* stackLiteral = get_if<LiteralSlot>(&slot))
			if (stackLiteral->value == _literal.value)
				return;
	m_currentStack->emplace_back(LiteralSlot{_literal.value});
}
void StackLayoutGenerator::operator()(DFG::FunctionCall& _call)
{
	for(auto& argument: _call.arguments)
		std::visit(*this, argument);
	m_currentStack->emplace_back(ReturnLabelSlot{&_call});
}
void StackLayoutGenerator::operator()(DFG::BuiltinCall& _call)
{
	for(auto& argument: _call.arguments)
		std::visit(*this, argument);
	while(!m_currentStack->empty() && std::holds_alternative<LiteralSlot>(m_currentStack->back()))
		m_currentStack->pop_back();
}

void StackLayoutGenerator::operator()(DFG::ExpressionStatement& _expressionStatement)
{
	std::visit(*this, _expressionStatement.expression);
	while(!m_currentStack->empty() && std::holds_alternative<LiteralSlot>(m_currentStack->back()))
		m_currentStack->pop_back();
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
		_varDecl.value ? optional(std::visit(*this, *_varDecl.value)) : std::nullopt,
		std::nullopt
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
			std::visit(*this, *_switch.expression),
			std::nullopt
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
		&function,
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
	builder.m_currentFunctionExit->exit = DFG::BasicBlock::FunctionReturn{&info};
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
