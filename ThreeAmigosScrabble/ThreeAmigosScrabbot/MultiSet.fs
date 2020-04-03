module MultiSet 
    type MultiSet<'a when 'a : comparison> = 
        | MS of Map<'a, uint32>
        override q.ToString() =
            match q with 
            | MS map -> let elems = Map.fold (fun str key value -> sprintf "%s(%A, #%d), " str key value ) "" map
                        let str = elems.Substring(0, elems.Length-2)
                        sprintf "{%s}" str

    let empty = MS(Map.empty)
    
    let isEmpty s = match s with
                    | MS map -> map.IsEmpty
    
    let size s = match s with
                 | MS map -> Map.fold (fun sum key value -> sum + value) 0u map
        
    let contains a s = match s with
                       | MS map -> map.ContainsKey a
    
    let numItems a s = match s with
                       | MS map -> let opt = map.TryFind a 
                                   match opt with
                                   | None -> 0u
                                   | Some n -> n

    let add a n s = match s with
                    | MS map -> let count = numItems a s
                                MS (map.Add (a, (n+count)))
    
    let addSingle a s = add a 1u s
    
    let remove a n s = match s with
                       | MS map -> let count = numItems a s
                                   let reduce = if(count < n)
                                                then 0u
                                                else count-n
                                   if(reduce = 0u)
                                   then MS (map.Remove a)
                                   else MS (map.Add (a, reduce))
    
    let removeSingle a s = remove a 1u s
    
    let fold f acc (MS(set)) = Map.fold f acc set
    
    let foldBack f (MS(set)) acc = Map.foldBack f set acc
    
    let map (f:'a->'b) (MS(set)) : MultiSet<'b> = 
        Map.toList set |> List.map (fun (k,v) -> (f k,v)) |> List.fold (fun acc (k,v) -> add k v acc) empty
    
    let ofList lst = List.fold (fun ms e -> addSingle e ms) empty lst
    
    let toList (MS(set)) = 
        let rec addKVToList list key value = 
            match value with
            | n when n < 1u -> list
            | n -> addKVToList (list@[key]) key (n-1u)

        Map.fold (fun lst key value -> addKVToList lst key value) [] set
    
    let union s1 s2 = 
        fold (fun s1 key value -> 
            if(not (contains key s1))
            then add key value s1
            else if(numItems key s1 >= value)
                 then s1
                 else add key value s1
        ) s1 s2

        //function that fold takes
        //('a -> 'b -> uint32 -> 'a) 
        //'a = acc
        //'b = key in MS
        // uint32 = value in MS

    let sum s1 s2 = 
        let sumset = fold (fun acc key value -> add key value acc) empty s1
        fold (fun sumset key value -> 
            add key (value) sumset
        ) sumset s2
    
    
    let subtract s1 s2 =
        fold (fun s1 key value -> 
            remove key value s1
        ) s1 s2 
    
    let intersection s1 s2 =
        fold (fun acc key value -> 
            if(contains key s1)
            then 
                let s1value = numItems key s1
                if(s1value < value)
                then add key s1value acc
                else add key value acc
            else acc
        ) empty s2
    //MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>