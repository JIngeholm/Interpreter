module Interpreter.State
    
    type state

    val mkState : int -> option<int> -> state

    val random : state -> int
 
    val declare : string -> state -> option<state> 

    val setVar : string -> int -> state -> option<state> 

    val getVar : string -> state -> option<int> 

    val alloc : string -> int -> state -> option<state> 

    val free : int -> int -> state -> option<state> 

    val setMem : int -> int -> state -> option<state> 

    val getMEm : int -> state -> option<int> 