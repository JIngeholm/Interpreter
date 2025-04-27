module Interpreter.Parser

    open Interpreter.Language

    open JParsec.TextParser             // Example parser combinator library.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use if performance gets bad
    

    let pif       : Parser<string> = pstring "if" <?> "if"
    let pelse     : Parser<string> = pstring "else" <?> "else"
    let palloc    : Parser<string> = pstring "alloc" <?> "alloc"
    let pfree     : Parser<string> = pstring "free" <?> "free"
    let pwhile    : Parser<string> = pstring "while" <?> "while"
    let pdo       : Parser<string> = pstring "do" <?> "do"
    let pdeclare  : Parser<string> = pstring "declare" <?> "declare"
    let ptrue     : Parser<string> = pstring "true" <?> "true"
    let pfalse    : Parser<string> = pstring "false" <?> "false"
    let pprint    : Parser<string> = pstring "print" <?> "print"
    let prandom   : Parser<string> = pstring "random" <?> "random"
    let pread     : Parser<string> = pstring "read" <?> "read"
    let pfunction : Parser<string> = pstring "function" <?> "function"
    let pret      : Parser<string> = pstring "ret" <?> "ret"
    
    let pwhitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespaceChar"
    let pletter         = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric   = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many pwhitespaceChar <?> "spaces"
    let spaces1        = many1 pwhitespaceChar <?> "spaces1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>> p2
    let (.>*>) p1 p2  = p1 .>> spaces >>. p2
    let (>*>.) p1 p2  = p1 >>. spaces .>> p2

    let parenthesise p = pchar '(' .>> spaces >>. p .>> spaces .>> pchar ')' <?> "Parenthesised"
    let curlyParenthesise p = pchar '{' .>*>. spaces .>> p .>*>. spaces .>> pchar '}'
    
    let listToString lst =
        System.String(List.toArray lst)
    
    let pid = (pletter <|> pchar '_')
                          .>>. many (palphanumeric <|> pchar '_')
                          |>> ( fun ( x, xs ) -> listToString ( x :: xs )) 
    
    let parseString=
        pchar '"' >>.
        many (satisfy ( fun c -> c <> '"'))
        .>> pchar '"'
        |>> (fun lst ->
            let s = listToString lst
            s.Replace("\\n", "\n").Replace("\\t", "\t")
            )
    
    let unop op a = op .>> spaces .>>. a |>> snd
    
    let binop op a b = a .>> spaces .>> op .>> spaces .>>. b
  
    let TermParse, tref = createParserForwardedToRef<aexpr>()
    let ProdParse, pref = createParserForwardedToRef<aexpr>()
    let AtomParse, aref = createParserForwardedToRef<aexpr>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Divide"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Modulo"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse] 
  
    let NParse   = pint32 |>> Num <?> "Int"
    let VarParse = pid |>> Var <?> "Var"
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul (Num -1, x)) <?> "Negation"
    let ParParse = parenthesise TermParse <?> "Parens"
    do aref := choice [NParse; VarParse; NegParse; ParParse]

    
    let CondParse =
        TermParse .>> pchar '?' .>>. TermParse .>> pchar ':' .>>. TermParse
        |>> (fun ((cond, t), f) -> Cond (cond, t, f)) <?> "Conditional"

    let paexpr = choice [CondParse; TermParse] <?> "Expression"
    

    
    
    let pbexpr = pstring "not implemented" |>> (fun _ -> TT)

    
    
    
    
    let pstmnt = pstring "not implemented" |>> (fun _ -> Skip)
    
    let pprogram = pstmnt |>> (fun s -> (Map.empty : program), s)
    
    let run = run
       
    let runProgramParser = run (pprogram .>> eof)  
