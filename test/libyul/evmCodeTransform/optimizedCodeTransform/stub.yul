{
    {
	let a := 42
        sstore(0,mload(calldataload(add(a, 43))))
    }
}
// ====
// stackOptimization: true
// ----
// PUSH1 0x1
// PUSH1 0x0
// DUP1
// SSTORE
