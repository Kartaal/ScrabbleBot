module Dictionary
    type Dictionary =
        | Dict of bool * Map<char,Dictionary>
    
    val empty : string -> Dictionary

    val insert : string -> Dictionary -> Dictionary

    val lookup : string -> Dictionary -> bool