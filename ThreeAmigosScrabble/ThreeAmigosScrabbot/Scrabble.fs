namespace TAScrabbot

open ScrabbleLib

open ScrabbleServer
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.Net.Sockets
open System.IO
open DebugPrint

open Dictionary

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, and your player numer but it could, potentially, 
    // keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        dictionary    : Dictionary
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        points        : int                         // represents the amount of points local player has
        board         : coord -> bool               // 
        playedTiles   : Map<coord,char*int>         //Represents the coords and tiles played in the game
        //turn          : bool                        //represents if it is the local player's turn????
    }

    let mkState d pn h p b pt = { dictionary = d; playerNumber = pn; hand = h; points = p; board = b; playedTiles = pt }
    
    let dictionary st    = st.dictionary
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let points st        = st.points
    let board st         = st.board
    let playedTiles st   = st.playedTiles

module Scrabble =
    open System.Threading
    open Ass7.ImpParser
    open JParsec.TextParser
    open Eval

    let playGame cstream pieces (st : State.state) =
        
        let newTilesPlayed tiles = List.fold (fun map (coord,(_,(char,value))) -> Map.add coord (char,value) map ) st.playedTiles tiles

        let rec aux (st : State.state) =
        (*  MANUAL PLAY LINES START HERE   *)
            Thread.Sleep(5000) // only here to not confuse the pretty-printer. Remove later.
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of state between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move) //sends a play move to the server
            //send cstream (SMPass) //sends a pass move to the server
            //send cstream (SMForfeit) //sends a forfeit move to the server
            //send cstream (SMChange pieceIdList) //sends a change pieces move to the server (I am swapping these pieces for new ones)
            
        (*  MANUAL PLAY LINES END HERE   *)

            // TODO move finding algorithm

            (* Bot figuring out moves here *)
            //Run through playedTiles, check if character at that tile (n,m) matches first letter of legal word
            //If it matches, check if tile (n,m+1) is useable, loop until word finished or cannot build word
            //If (n,m+1) not useable, check (n+1,m) instead, loop until word finished or cannot build word
            //If no match, check next playedTiles tile

            (*Jesper comment
            Have a function that returns (coord, (uint32, (char, int))) list option (that is the type the server expects) 
                    that gradually builds all of the information the server needs. 
            It's very difficult to do this in several cycles. 
            It sort of needs to be done at once - traverse the board, alternate by what is on your hand 
                and what is on your board, traverse your dictionary and 
                backtrack as soon as something fails (cannot progress word, or building illegal word with crossing word). 

            Your hand is a multiset.
            Fold over that (MultiSet.fold)
            Take the individual letters and check if they will continue the word.
            If they do, remove a single element (MultiSet.removeSingle) from your hand and 
                recurse making sure to save the coordinate, the id and so on that you used.
            So your recursive function takes a hand, you fold over that and remove successful letter placements, 
                and when you recurse you send your new hand. 
            *)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) -> // newPieces = (id,num)
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let pieceIds = List.fold (fun (acc:uint32 list) (_,(id,_)) -> id::acc) [] move
                let cleanedHand = List.foldBack MultiSet.removeSingle pieceIds st.hand // Removes pieces we have already placed
                let refilledHand = List.foldBack (fun newPiece acc -> MultiSet.add (fst newPiece) (snd newPiece) acc) newPieces cleanedHand
                
                let playedTiles' = newTilesPlayed ms //adding new tiles to the map of already placed tiles
                
                let st' = State.mkState st.dictionary st.playerNumber refilledHand (st.points + points) st.board playedTiles' // This state needs to be updated, missing new state things
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let playedTiles' = newTilesPlayed ms //adding new tiles to the map of already placed tiles

                //Not keeping track of other players' points
                let st' = State.mkState st.dictionary st.playerNumber st.hand st.points st.board playedTiles'
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let playedTiles' = newTilesPlayed ms //adding new tiles to the map of already placed tiles

                let st' = State.mkState st.dictionary st.playerNumber st.hand st.points st.board playedTiles'
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (alphabet : string) 
            (words : string list)
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)
        
        let emptyDictionary = Dictionary.empty alphabet
        let dictionary = List.fold (fun dict word -> Dictionary.insert word dict) emptyDictionary words //Should put alla the words from the words list into our dictionary

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        let playedTiles : Map<coord,char*int> = Map.empty
        
        let mapSquareStringToStmnt = Map.map (fun _ -> runTextParser stmParse)

        let squareStmts = Map.map (fun _ -> mapSquareStringToStmnt) boardP.squares //parses the strings for the squares to statements, keeping the data structure intact
        
        let mapSquareStmntToFun = Map.map (fun _ -> stmntToSquareFun)
        let squareFuns = Map.map (fun _  -> mapSquareStmntToFun) squareStmts //Now has squareFuns!

        let boardStmnt = runTextParser stmParse boardP.prog
        let boardFun = stmntToBoardFun boardStmnt squareFuns //Now only has coord -> does square exist

        fun () -> playGame cstream tiles (State.mkState dictionary playerNumber handSet 0 boardFun playedTiles) //probably needs updating... something something words/alphabet ensure bot only plays valid words FUUUUUUUUUUU
        