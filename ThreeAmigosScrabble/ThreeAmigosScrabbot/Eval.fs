module Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H',4);('E',1);('L',1);('L',1);('O',2)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add (a:SM<int>) (b:SM<int>) : SM<int> = 
        a >>= fun x -> 
            b >>= fun y -> 
                ret (x + y)

                
    let sub (a:SM<int>) (b:SM<int>) : SM<int> = 
        a >>= fun x -> 
            b >>= fun y -> 
                ret (x - y)

    
    let mul (a:SM<int>) (b:SM<int>) : SM<int> = 
        a >>= fun x -> 
            b >>= fun y -> 
                ret (x * y)

    let div a b = 
        a >>= fun x -> 
            b >>= fun y -> 
                if(y <> 0)
                then ret (x / y)
                else fail DivisionByZero 

    
    let modulo (a:SM<int>) (b:SM<int>) : SM<int> = 
        a >>= fun x -> 
            b >>= fun y -> 
                if(y <> 0)
                then ret (x % y)
                else fail DivisionByZero 

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsConsonant of cExp  (* check for constant *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    
    
    let toUpper c : SM<char> =
        c >>= fun x ->
            ret (System.Char.ToUpper x)
            
    let toLower c : SM<char> =
        c >>= fun x ->
            ret (System.Char.ToLower x)
            
    let equal (a:SM<'a>) (b:SM<'a>) : SM<bool> = 
        a >>= fun x -> 
            b >>= fun y -> 
                ret (x = y)
                
    let lessThan (a:SM<'a>) (b:SM<'a>) : SM<bool> = 
        a >>= fun x -> 
            b >>= fun y -> 
                ret (x < y)

    let Not (a:SM<bool>) : SM<bool> =
        a >>= fun x ->
            ret (not x)
            
    let Conjunction (a:SM<bool>) (b:SM<bool>) : SM<bool> = 
        a >>= fun x -> 
            b >>= fun y -> 
            ret (x && y)


    let isVowel (c:char) = 
        match System.Char.ToLower(c) with
        | 'a'|'e'|'i'|'o'|'u' -> true 
        | _ -> false

    let isConsonant (c:char) = 
        match System.Char.IsLetter(c) with
        | true -> if(isVowel c) then false else true
        | false -> false

    let isVowelSM (a:SM<char>) : SM<bool> =
        a >>= fun x ->
            ret (isVowel x)

    let isConsonantSM (a:SM<char>) : SM<bool> =
        a >>= fun x ->
            ret (isConsonant x)
            
    let rec arithEval a : SM<int> = 
        match a with
        | N i -> ret i
        | V s -> lookup s
        | WL -> wordLength
        | PV ae -> 
            arithEval ae >>= pointValue 
        | Add (a,b) -> add (arithEval a) (arithEval b)
        | Sub (a,b) -> sub (arithEval a) (arithEval b)
        | Mul (a,b) -> mul (arithEval a) (arithEval b)
        | Div (a,b) -> div (arithEval a) (arithEval b)
        | Mod (a,b) -> modulo (arithEval a) (arithEval b)
        | CharToInt ce -> 
            charEval ce >>= fun x -> ret ((int) x)


    and charEval c : SM<char> = 
        match c with
        | C ch -> ret ch
        | CV ae -> 
            arithEval ae >>= characterValue
        | ToUpper ce -> toUpper (charEval ce)
        | ToLower ce -> toLower (charEval ce)
        | IntToChar ae -> 
            arithEval ae >>= fun x -> ret ((char) x)

    and boolEval b : SM<bool> = 
        match b with
        | TT                   -> ret true
        | FF                   -> ret false
                                //Super inconsistent naming of helper functions, wheee
        | AEq (ae1,ae2)        -> equal (arithEval ae1) (arithEval ae2)
        | ALt (ae1,ae2)        -> lessThan (arithEval ae1) (arithEval ae2)

        | Not be               -> Not (boolEval be)
        | Conj (be1,be2)       -> Conjunction (boolEval be1) (boolEval be2)

        | IsVowel ce           -> isVowelSM (charEval ce)
        | IsConsonant ce       -> isConsonantSM (charEval ce)


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)
    
    let rec stmntEval stmnt : SM<unit> = 
        match stmnt with
        | Declare s               -> declare s
        | Ass (s,ae)              -> arithEval ae >>= (update s)
        | Skip                    -> ret ()
        | Seq (se1,se2)           -> stmntEval se1 >>= fun () -> stmntEval se2
        | ITE(bExp, stm1, stm2)   -> 
            boolEval bExp >>= (fun b -> if b then push >>>= (stmntEval stm1) else push >>>= (stmntEval stm2)) >>>= pop
        | While (be,se)           -> 
            let loop = Seq(se,While(be,se))
            stmntEval (ITE(be,loop,Skip))
    
(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    (*let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"*)

(* Part 4 (Optional) *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> int

    let unwrapRes res = match res with
                        | Success x -> x
                        | Failure err -> failwith (sprintf "%A" err)

    let stmntToSquareFun stm : squareFun = 
        fun (w:word) (pos:int) (acc:int) ->
            let state = mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"]

            stmntEval stm >>>= lookup "_result_" 
            |> evalSM state |> unwrapRes

    type coord = int * int

    type boardFun = coord -> bool

    let stmntToBoardFun stm squares : boardFun = 
        fun (x,y) ->
            let state = mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_";"_y_";"_result_"]
    
            evalSM state (stmntEval stm >>>= 
                lookup "_result_" >>= 
                    (fun i -> 
                        match Map.tryFind i squares with
                        | Some x -> ret true
                        | None -> ret false
                    )
                ) |> unwrapRes
            
        
    type board = {
        center        : coord
        squares       : boardFun
    }

    let mkBoard (coord:coord) defaultSq (boardStmnt:stm) ids = 
        
        let defSquare = stmntToSquareFun defaultSq
        let idsMap = Map.ofList ids
        let idsFuns = Map.map (fun _ stm -> (stmntToSquareFun stm)) idsMap
        let squares = stmntToBoardFun boardStmnt idsFuns

        { 
            center = coord; 
            squares = squares 
        }