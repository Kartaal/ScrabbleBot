module Dictionary
    type Dictionary =
        | Dict of bool * Map<char,Dictionary>
    
    let empty (s:string) = Dict (false, Map.empty)

    let rec insert s dict = 
        let (b, map) =
            match dict with
            |Dict (b,m) -> (b,m)

        match s with
        | "" -> Dict(true, map)
        | s -> 
            let letter = s.[0]

            let lettersDict = 
                let hasLetterOpt = map.TryFind letter
                if(hasLetterOpt.IsSome)
                then hasLetterOpt.Value
                else empty ""

            //make the dict for current letter in word, depends upon later recursive calls returning
            let returnDict = insert (s.Substring(1, s.Length-1)) lettersDict

            //update the dict associated with current letter of word
                                //char   dict    map
            let newmap = Map.add letter returnDict map

            //return dict for previous letter in word
            Dict(b, newmap)
            
    let rec lookup (s:string) (Dict(b,map)) = 
        match s with
        | "" -> b
        | s -> 
            let letter = s.[0]
            let newString = s.Substring(1, s.Length-1)
            let hasLetterOpt = map.TryFind letter
            if(hasLetterOpt.IsSome)
            then lookup newString (hasLetterOpt.Value)
            else false

    let stepLookup (c:char) (Dict(_,map)) =
        match map.TryFind c with
        | Some (Dict(newB,newDict)) -> Some (newB,newDict)
        | None -> None