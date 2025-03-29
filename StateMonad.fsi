module Interpreter.StateMonad

    open Language
    
    // Green and Yellow exercises (remove up until Red exercises if doing Red, but start with Green or Yellow regardless)

    type 'a stateMonad
    
    val ret  : 'a -> 'a stateMonad    
    
    val (>>=) : 'a stateMonad -> ('a -> 'b stateMonad) -> 'b stateMonad
    val (>>>=) : 'a stateMonad -> 'b stateMonad -> 'b stateMonad
    
    // Green
    val fail : 'a stateMonad
    
    // Yellow
    //val fail : error -> 'a stateMonad

    val (>>=) : 'a stateMonad -> ('a -> 'b stateMonad) -> 'b stateMonad
    val (>>>=) : 'a stateMonad -> 'b stateMonad -> 'b stateMonad


    
    
    // Red Exercises
    (*
    
    type stateContMonad<'a, 'r>
    
    val ret  : 'a -> stateContMonad<'a, 'r>
    
    val fret : int -> stateContMonad<unit, 'r>
    val fcall : string -> int list -> (stmnt -> stateContMonad<unit, 'r>) -> stateContMonad<int, 'r>
    
    // Green
    // val fail : stateContMonad<'a, 'r>
    
    // Yellow
    val fail : error -> stateContMonad<'a, 'r>
 
    val (>>=) : stateContMonad<'a, 'r> -> ('a -> stateContMonad<'b, 'r>) -> stateContMonad<'b, 'r>
    val (>>>=) : stateContMonad<'a, 'r> -> stateContMonad<'b, 'r> -> stateContMonad<'b, 'r>


 *)
    
    
    
    