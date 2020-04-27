module StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = 
        S (fun s -> Success ((), {s with vars = s.vars.Tail}))

    let wordLength : SM<int> = 
        S (fun s -> Success ((s.word.Length, s)))

    let characterValue (pos : int) : SM<char> = 
        S (fun s ->
            if(s.word.Length-1 >= pos && pos > -1)
            then Success (fst (s.word.Item pos), s)
            else Failure (IndexOutOfBounds pos)
        )

    let pointValue (pos : int) : SM<int> = 
        S (fun s ->
            if(s.word.Length-1 >= pos && pos > -1)
            then Success (snd (s.word.Item pos), s)
            else Failure (IndexOutOfBounds pos)
        )

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = 
        S (fun s ->
            let head = s.vars.Head
            if(s.reserved.Contains var)
            then Failure (ReservedName var)
            else if(head.ContainsKey var)
                 then Failure (VarExists var)
                 else Success ((), {s with vars = (Map.add var 0 head)::s.vars.Tail})
        )

    let update (var : string) (value : int) : SM<unit> = 
        //This does not work and I will not fix it!
        let aux2 s = List.fold (fun acc map -> 
                        if((not (snd acc)) && Map.containsKey var map)
                        then ((map.Add (var, value))::(fst acc), true)
                        else (map::(fst acc), snd acc)) ([], false) s.vars

        let rec aux (stack:Map<string, int> list) =
            match stack with 
            | [] -> None
            | map::maps -> 
                match map.TryFind var with
                | Some x -> Some ((map.Add (var, value))::maps)
                | None -> 
                    match aux maps with
                    | Some x -> Some (map::x)
                    | None -> None
        
        (*
        S (fun s -> 
            match aux2 s with
            | (lst, true) -> Success ((), {s with vars = lst})
            | _ -> Failure (VarNotFound var)
            )
        *)

        S (fun s -> 
            match aux s.vars with
            | Some x -> Success ((), {s with vars = x})
            | None -> Failure (VarNotFound var)
        )
