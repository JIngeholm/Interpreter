module Interpreter.Memory

    type memory

    val empty: int -> memory

    val alloc: int -> memory -> option<memory * int>

    val free: int -> int -> memory -> option<memory>

    val setMem: int -> int -> memory -> option<memory>

    val getMem: int -> memory -> option<int>