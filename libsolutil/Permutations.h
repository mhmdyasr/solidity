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
#pragma once
#include <libsolutil/Visitor.h>
#include <liblangutil/Exceptions.h>
#include <range/v3/view/enumerate.hpp>

namespace solidity::util
{

template<typename GetTargetPosition, typename Swap, typename Pop>
void permute(unsigned _n, GetTargetPosition _getTargetPosition, Swap _swap, Pop _pop)
{
	static_assert(
		std::is_same_v<std::invoke_result_t<GetTargetPosition, unsigned>, int>,
		"_getTargetPosition needs to have the signature int(unsigned)"
	);
	static_assert(
		std::is_same_v<std::invoke_result_t<Swap, unsigned>, void>,
		"_swap needs to have the signature void(unsigned)"
	);
	static_assert(
		std::is_same_v<std::invoke_result_t<Pop>, void>,
		"_pop needs to have the signature void()"
	);
	if (_n == 0) return;
	int targetPositionTop = _getTargetPosition(_n - 1);

	if (targetPositionTop < 0)
	{
		// The last element should not be kept.
		// Pop it and recurse.
		_pop();
		permute(_n - 1, _getTargetPosition, _swap, _pop);
		return;
	}
	// TODO: exception?
	//	assertThrow(static_cast<unsigned>(targetPositionTop) < _n, langutil::InternalCompilerError, "Invalid permutation.");
	if (static_cast<unsigned>(targetPositionTop) == _n - 1)
	{
		// The last element is in position.
		// Seach for the deepest element that is not in position.
		// If there is none, we are done. Otherwise swap it up and recurse.
		for (int i = 0; i < static_cast<int>(_n - 1); ++i)
			if (_getTargetPosition(static_cast<unsigned>(i)) != i)
			{
				_swap(_n - static_cast<unsigned>(i) - 1);
				permute(_n, _getTargetPosition, _swap, _pop);
				return;
			}
	}
	else
	{
		// The last element is not in position.
		// Move it to its position and recurse.
		_swap(_n - static_cast<unsigned>(targetPositionTop) - 1);
		permute(_n, _getTargetPosition, _swap, _pop);
	}
}

}