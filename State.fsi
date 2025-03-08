module Interpreter.State
    
    type state

    val mkState : int -> state
 
    val declare : string -> state -> state option 

    val setVar : string -> int -> state -> state option

    val getVar : string -> state -> int option