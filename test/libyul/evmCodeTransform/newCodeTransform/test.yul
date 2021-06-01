{
            function array_allocation_size_array_uint256_dyn(length) -> size
            {
                if gt(length, 0xffffffffffffffff) { panic_error_0x41() }
                size := add(shl(5, length), 0x20)
            }
            function panic_error_0x41()
            {
                mstore(0, shl(224, 0x4e487b71))
                mstore(4, 0x41)
                revert(0, 0x24)
            }
            let x := array_allocation_size_array_uint256_dyn(0x42)
            return(x,x)
}

/*
object "C_23" {
    code {
        {
            mstore(64, 128)
            if callvalue() { revert(0, 0) }
            let _1 := datasize("C_23_deployed")
            codecopy(128, dataoffset("C_23_deployed"), _1)
            return(128, _1)
        }
    }
    object "C_23_deployed" {
        code {
            {
                mstore(64, 128)
                if iszero(lt(calldatasize(), 4))
                {
                    if eq(0x780900dc, shr(224, calldataload(0)))
                    {
                        if callvalue() { revert(0, 0) }
                        if slt(add(calldatasize(), not(3)), 32) { revert(0, 0) }
                        let value := calldataload(4)
                        let _1 := array_allocation_size_array_uint256_dyn(value)
                        let memPtr := mload(64)
                        let _2 := not(31)
                        let newFreePtr := add(memPtr, and(add(_1, 31), _2))
                        if or(gt(newFreePtr, 0xffffffffffffffff), lt(newFreePtr, memPtr)) { panic_error_0x41() }
                        mstore(64, newFreePtr)
                        mstore(memPtr, value)
                        calldatacopy(add(memPtr, 32), calldatasize(), add(array_allocation_size_array_uint256_dyn(value), _2))
                        let length := mload(memPtr)
                        let memPos := mload(64)
                        return(memPos, sub(abi_encode_uint256(memPos, length), memPos))
                    }
                }
                revert(0, 0)
            }
            function abi_encode_uint256(headStart, value0) -> tail
            {
                tail := add(headStart, 32)
                mstore(headStart, value0)
            }
            function array_allocation_size_array_uint256_dyn(length) -> size
            {
                if gt(length, 0xffffffffffffffff) { panic_error_0x41() }
                size := add(shl(5, length), 0x20)
            }
            function panic_error_0x41()
            {
                mstore(0, shl(224, 0x4e487b71))
                mstore(4, 0x41)
                revert(0, 0x24)
            }
        }
    }
}
*/
// ====
// stackOptimization: true
// ----