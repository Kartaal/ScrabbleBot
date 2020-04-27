module Ass7

open System
open JParsec
open TextParser
open Parser
open Eval

module ImpParser =
    
    //7.2
    let (.>*>.) p1 p2 = 
        p1 >>= (fun r1 -> spaces >>= fun r2 -> p2 >>= fun r3 -> returnP (r1, r3)) <?>
        sprintf "(%s >*> %s)" (getLabel p1) (getLabel p2)
            
    let (.>*>) p1 p2 = p1 .>*>. p2 |>> fst <?> (sprintf "(%s .>*> %s)" (getLabel p1) (getLabel p2))
    let (>*>.) p1 p2 = p1 .>*>. p2 |>> snd <?> (sprintf "(%s >*>. %s)" (getLabel p1) (getLabel p2))

    //7.3
    let parenthesise p =
        (pchar '(')  >*>. spaces >*>. p .>*> spaces .>*> (pchar ')')

        
    let bracketerise p =
        (pchar '{')  >*>. spaces >*>. p .>*> spaces .>*> (pchar '}')

    //7.6
    let binop op a b : Parser<TextInputState, 'a * 'b> = 
        a .>*> op .>*>. b 
    
    //7.5
    let unop op a = 
        op >*>. a

    
    //7.1
    let pIntToChar = pstring "intToChar"
    let pPointValue = pstring "pointValue"
    let pCharToInt = pstring "charToInt"
    let pToUpper = pstring "toUpper"
    let pToLower = pstring "toLower"
    let pCharValue = pstring "charValue"
    let pTrue = pstring "true"
    let pFalse = pstring "false"
    let pif = pstring "if"
    let pthen = pstring "then"
    let pelse = pstring "else"
    let pwhile = pstring "while"
    let pdo = pstring "do"

    let pdeclare = pstring "declare"


    //7.4
    let pid = letterChar .>>. many alphaNumeric |>> fun (ch,chlst) -> charListToStr (ch::chlst)

    //aExp
    let TermParse, tref = createParserForwardedToRef<TextInputState, aExp>()
    let ProdParse, pref = createParserForwardedToRef<TextInputState, aExp>()
    let AtomParse, aref = createParserForwardedToRef<TextInputState, aExp>()

    //cExp
    let CEParse, cref = createParserForwardedToRef<TextInputState, cExp>()

    //bExp
    let ANDORParse, b1ref = createParserForwardedToRef<TextInputState, bExp>()
    let EqParse, b2ref = createParserForwardedToRef<TextInputState, bExp>()
    let BParse, b3ref = createParserForwardedToRef<TextInputState, bExp>()

    //stmnt
    let SeqParse, seqref = createParserForwardedToRef<TextInputState, stm>()
    let StmParse, sref = createParserForwardedToRef<TextInputState, stm>()

    
    let CParParse = parenthesise CEParse <?> "char parenthesis"

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
                   
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse] <?> "Term"

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse] <?> "Prod"
    
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> (N -1, x)) |>> Mul <?> "-aExp"
    let NParse   = pint |>> N <?> "Int"
    let ParParse = parenthesise TermParse <?> "parenthesis"
    let VParse   = pid |>> V <?> "Var"
    let PointValueParse = pPointValue >*>. ParParse |>> PV <?> "PV"
    let CharToIntParse = pCharToInt >*>. CParParse |>> CharToInt <?> "charToInt"
    do aref := choice [NegParse; CharToIntParse; PointValueParse; NParse; VParse; ParParse] <?> "Atom"

    let AexpParse = TermParse <?> "arithmetic expression"

    let apoParse = pchar '\''

    let CParse = between apoParse pAnyChar apoParse |>> C <?> "Char"
    let CVParse = pCharValue >*>. ParParse |>> CV <?> "CharValue"
    
    let ToLowerParse = pToLower >*>. CParParse |>> ToLower <?> "ToLower"
    let ToUpperParse = pToUpper >*>. CParParse |>> ToUpper <?> "ToUpper"
    let IToCParse = pIntToChar >*>. ParParse |>> IntToChar <?> "IntToChar"
    do cref := choice [CParse; CVParse; ToLowerParse; ToUpperParse; IToCParse]

    let CexpParse = CEParse <?> "character expression"

    let BEParse = ANDORParse
    
    let NotParse = unop (pchar '~') ANDORParse |>> (fun x -> ~~x) <?> "Not"
    
    let ConjParse = binop (pstring "/\\") EqParse ANDORParse |>> (fun (a,b) -> a .&&. b) <?> "/\\" 
    let DisjParse = binop (pstring "\\/") EqParse ANDORParse |>> (fun (a,b) -> a .||. b) <?> "\\/"

    do b1ref := choice [ConjParse; DisjParse; EqParse] <?> "boolean conjunction or disjoinction"

    let EqualParse = binop (pchar '=') AtomParse AtomParse |>> (fun (a,b) -> a .=. b) <?> "="
    let LTParse = binop (pchar '<') AtomParse AtomParse |>> (fun (a,b) -> a .<. b) <?> "<"
    let NotEqualParse = binop (pstring "<>") AtomParse AtomParse |>> (fun (a,b) -> a .<>. b) <?> "<>"
    let LTEParse = binop (pstring "<=") AtomParse AtomParse |>> (fun (a,b) -> a .<=. b) <?> "<="
    let GTEParse = binop (pstring ">=") AtomParse AtomParse |>> (fun (a,b) -> a .>=. b) <?> ">="
    let GTParse = binop (pchar '>') AtomParse AtomParse |>> (fun (a,b) -> a .>. b) <?> ">"

    do b2ref := choice [EqualParse; NotEqualParse; LTParse; LTEParse; GTParse; GTEParse; BParse] <?> "bool comparison"

    let TrueParse = pstring "true" |>> (fun _ -> TT) <?> "true"
    let FalseParse = pstring "false" |>> (fun _ -> FF) <?> "false"
    let BParParse = parenthesise BEParse <?> "bool parenthesis"
    do b3ref := choice [NotParse; TrueParse; FalseParse; BParParse] <?> "boolean negation, true, false or parenthesis expression"

    let BexpParse = BEParse <?> "boolean expression"
    
    let BracParse = bracketerise StmParse <?> "statement brackets"

    let AssParser = binop (pstring ":=") pid AtomParse |>> Ass <?> ":="
    let DeclareParser = pdeclare .>>. whitespaceChar >>. pid |>> Declare <?> "declare"
    let SeqParser = binop (pchar ';') StmParse SeqParse |>> Seq <?> ";" //Stack overflow or bypassing seq entirely, GREAT FUN
    let ITEParser = pif .>>. whitespaceChar >>. BParParse .>> whitespaceChar .>>
                    pthen .>> whitespaceChar .>>. BracParse .>> whitespaceChar .>> 
                    pelse .>> whitespaceChar .>>. BracParse |>> 
                    (fun ((b,s1),s2) -> (b,s1,s2)) |>> ITE <?> "If-Then-Else"
    let ITParser = pif .>>. whitespaceChar >>. BParParse .>> whitespaceChar .>>
                    pthen .>> whitespaceChar .>>. BracParse |>> 
                    (fun (b,s) -> (b,s,Skip) ) |>> ITE <?> "If-Then"
    let WhileParser = pwhile .>>. whitespaceChar >>. BParParse .>> whitespaceChar .>>
                      pdo .>> whitespaceChar .>>. BracParse |>> While <?> "While"

    do seqref := SeqParser <|> StmParse
    do sref := choice [AssParser; DeclareParser; ITEParser; ITParser; WhileParser] <?> "statement expression"

    let stmParse = SeqParse <?> "statement expression"
