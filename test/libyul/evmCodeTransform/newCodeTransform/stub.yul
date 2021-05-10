{
	let x := 0x42
	let y := 0x21
	mstore(y,add(x,y))
	mstore(x,1)
	mstore(x,x)
}
// ====
// stackOptimization: true
// ----
