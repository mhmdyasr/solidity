contract C {
	function f(uint x) public pure returns(uint y) {
		unchecked {
		 y = x * 2;
		if (x == 2)
			y = x;
		}
	}
}
// ====
// compileViaYul: true
// ----
// f(uint256): 100 ->
// f(uint256): 2 ->
