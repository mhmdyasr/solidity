{
    let _i := 0
    for { let x := 0 } iszero(lt(x, _i)) { x := add(x, 1) } {
        if eq(calldataload(0), _i) {
            mstore(_i, _i)
        }
    }
}
// ====
// stackOptimization: true
// ----
