{
    function f(a, b) -> r { r := mul(a, b) }

    mstore(0x40, f(12, 13))
}
// ====
// stackOptimization: true
// ----
// PUSH1 0x11
// JUMP
// JUMPDEST
// PUSH1 0x0
// DUP3
// DUP3
// MUL
// SWAP1
// POP
// JUMPDEST
// SWAP3
// SWAP2
// POP
// POP
// JUMP
// JUMPDEST
// PUSH1 0x1B
// PUSH1 0xD
// PUSH1 0xC
// PUSH1 0x3
// JUMP
// JUMPDEST
// PUSH1 0x40
// MSTORE
