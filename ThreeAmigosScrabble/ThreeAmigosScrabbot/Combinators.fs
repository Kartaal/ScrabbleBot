module JParsec

open System

type Position = {
    line : int
    column : int
}

module Position =
    (* Module and datatype inspired by Scott Wlaschin's web-site
       F# for fun and profit *)

    let mkPos l c   = {line = l; column = c}
    let getLine p   = p.line
    let getColumn p = p.column

    let setLine l p  = {p with line = l}
    let setColumn l p = {p with column = l}

    let initialPos = mkPos 0 0

    let incrCol p = setColumn (getColumn p + 1) p
    let incrLine p = (setLine (getLine p + 1) >> setColumn 0) p



type TextInputState = {
    lines : string[]
    position : Position 
}

module TextInputState =

    let initialInputState s =
        if String.IsNullOrEmpty(s) then
            {lines=[||]; position=Position.initialPos}
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = s.Split(separators, StringSplitOptions.None)
            {lines=lines; position=Position.initialPos} 

    let getLines    is = is.lines
    let getPosition is = is.position

    let setPosition is p = {is with position = p}

    let numLines = getLines >> Array.length

    let columnNumber = getPosition >> Position.getColumn
    let lineNumber   = getPosition >> Position.getLine

    let getLine is = 
        function
        | i when i < numLines is -> Some (getLines is).[i]
        | _ -> None

    let currentLine is = 
        getPosition is |> 
        Position.getLine |> 
        getLine is

    let getError is =
        let failureCaret = sprintf "%*s^" (columnNumber is) ""
        sprintf "Line: %i\tColumn: %i\n%s\n%s" 
                (lineNumber is) (columnNumber is) 
                (currentLine is |> Option.get) failureCaret

    let incrCol  is = getPosition is |> Position.incrCol |> setPosition is
    let incrLine is = getPosition is |> Position.incrLine |> setPosition is

    let nextChar is =
        let cPos = columnNumber is

        match currentLine is with
        | Some l when cPos < l.Length -> incrCol is, Some l.[cPos]
        | Some l                      -> incrLine is, Some '\n'
        | None                        -> is, None
                

type Result<'a, 'b> = 
   | Success of 'a
   | Failure of 'b

type ParserLabel = string
type ParseError = string

type ParseError<'a> = {
    label : ParserLabel
    input : 'a
    error : ParseError
}

module ParseError =
    let mkParseError l i err = {label = l; input = i; error = err}

    let setLabel pe l = {pe with label = l}

    let getLabel pe = pe.label
    let getInput pe = pe.input
    let getError pe = pe.error

    let print pe = 
        printf "Error parsing %s\n%s%s" 
                (getLabel pe) (TextInputState.getError (getInput pe)) (getError pe)

type Parser<'a, 'b> = {
   pfun  : 'a -> Result<'b * 'a, ParseError<'a>>
   label : ParserLabel
}

module Parser =

    let printResult =
        function
        | Success (v, _) -> printfn "Success: %A" v
        | Failure pe -> ParseError.print pe

    let getSuccess =
        function
        | Success (v, _) -> Some v
        | _              -> None

    let mkParser f l = {pfun = f; label = l}

    let getLabel p = p.label

    let run parser input =
        parser.pfun input
        
    let satisfy nextSymbol predicate label =
        let parseFun input =
            let remainingInput, symOpt = nextSymbol input
            match symOpt with
            | None     -> Failure (ParseError.mkParseError label input "no more input")
            | Some sym when predicate sym -> 
                  Success (sym, remainingInput)
            | Some sym -> 
                let err = sprintf "unexpected %A\n" sym
                Failure (ParseError.mkParseError label input err)

        mkParser parseFun label

    let returnP x = mkParser (fun input -> Success(x, input)) "unknown"

    let bindP f p = 
        let parseFun input =
            match run p input with
            | Failure pe -> Failure pe
            | Success (v, remainingInput) ->
                run (f v) remainingInput
        mkParser parseFun "unknown"


    let (>>=) p f = bindP f p

    let setLabel p l =
        let parseFun input =
            match run p input with
            | Success s  -> Success s
            | Failure pe -> Failure (ParseError.setLabel pe l)

        mkParser parseFun l

    let (<?>) = setLabel

    let andThen p1 p2 = 
        p1 >>= (fun r1 -> p2 >>= fun r2 -> returnP (r1, r2)) <?>
        sprintf "(%s .>>. %s)" (getLabel p1) (getLabel p2)

    let (.>>.) = andThen
        
    let orElse p1 p2 =
        let label = sprintf "(%s <|> %s)" (getLabel p1) (getLabel p2)
        let parserFun input =
            match run p1 input with
            | Success s  -> Success s
            | Failure pe -> run p2 input

        mkParser parserFun label

    let (<|>) = orElse

    let applyP fP xP =
        fP >>= (fun f -> xP >>= (fun x -> returnP (f x)))

    let ( <*> ) = applyP

    let choice ps = List.reduce orElse ps

    let mapP f = bindP (f >> returnP)

    let (|>>) p f = mapP f p

    let parseZeroOrMore p input = 
        let rec aux input acc =
            match run p input with
            | Failure pe -> Success (List.rev acc, input)
            | Success (v, remainingInput) ->
                aux remainingInput (v::acc)

        aux input []

    let many p = mkParser (parseZeroOrMore p) "unknown"

    let many1 p = 
        p >>= fun x -> many p >>= fun xs -> returnP (x :: xs)

    let rec sequence =
        function
        | []      -> returnP []
        | x :: xs -> returnP (fun h tl -> h :: tl) <*> x <*> (sequence xs)
        
    let opt p = p |>> Some <|> returnP None

    let (.>>) p1 p2 = p1 .>>. p2 |>> fst <?> (sprintf "(%s .>> %s)" (getLabel p1) (getLabel p2))
    let (>>.) p1 p2 = p1 .>>. p2 |>> snd <?> (sprintf "(%s >>. %s)" (getLabel p1) (getLabel p2))
    let between p1 p2 p3 = p1 >>. p2 .>> p3

    let sepBy1 p sep =
        p .>>. many (sep >>. p)
        |>> fun (x,xs) -> x::xs

    let sepBy p sep =
        sepBy1 p sep <|> returnP []

    let createParserForwardedToRef<'a, 'b> () =

        let dummyParser= 
            let innerFn input : Result<'b * 'a, ParseError<'a>> = failwith "unfixed forwarded parser"
            mkParser innerFn "unknown"

        let parserRef = ref dummyParser 

        let innerFn input = run !parserRef input 

        mkParser innerFn "unknown", parserRef
        
module TextParser =

    open Parser

    let charListToStr charList = String(List.toArray charList) |> string

    let pAnyChar = satisfy TextInputState.nextChar (fun _ -> true) "anyChar"

    let pchar c =
        satisfy TextInputState.nextChar (fun x -> x = c) (sprintf "%c" c)

    let letterChar =
        satisfy TextInputState.nextChar Char.IsLetter "letter"

    let anyOf ss =
        ss |> List.map pchar |> Parser.choice <?> 
        sprintf "anyOf %A" ss

    let manyChars p  = many p  |>> charListToStr
    let manyChars1 p = many1 p |>> charListToStr

    let pstring str =
        str |> List.ofSeq |> List.map pchar |> sequence |>> charListToStr <?> str



    let whitespaceChar =
        satisfy TextInputState.nextChar (Char.IsWhiteSpace) "whitespace" <?> "white space"

    let spaces = many whitespaceChar <?> "spaces"
    let space1 = many1 whitespaceChar <?> "space1"

    let digitChar =
        satisfy TextInputState.nextChar Char.IsDigit "digit" <?> "digit"

    let digits = manyChars1 digitChar <?> "digits"

    let alphaNumeric = letterChar <|> digitChar <?> "alphanumeric"

    let pint =
        opt (pchar '-') .>>. digits |>>
        function
        | (Some _, ds) -> -(int ds)
        | (None, ds)   -> int ds
        <?> "int"

    
    let parseUnwrap res = match res with
                          | Success (x,_) -> x
                          | Failure err -> failwith (sprintf "%A" err)

    let runTextParser parser inputStr =
        run parser (TextInputState.initialInputState inputStr)


    let runParserFromFile parser path =
        runTextParser parser (System.IO.File.ReadAllText path)

    