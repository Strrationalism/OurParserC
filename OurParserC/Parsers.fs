module OurParserC.Parsers

open Parser

exception SourceEnded
let anyChar : char parser = fun input ->
    match Input.peek input with
    | Some (c,input) -> Ok (c,input)
    | None -> Error (SourceEnded,input)

exception NotMatched

let character ch : unit parser = 
    pred anyChar ((=) ch) 
    >> Parsed.ignore 
    >> Parsed.mapError (function
    | :? ConditionTestFailed -> NotMatched
    | e -> e)

let charInSeq range : char parser = 
    pred anyChar (fun x -> Seq.exists ((=) x) range) 
    >> Parsed.mapError (fun _ -> NotMatched)
    >> Parsed.mapError (function
    | :? ConditionTestFailed -> NotMatched
    | e -> e)

let rec literal (excepted:char seq) : unit parser = fun input ->
    match excepted with
    | x when Seq.isEmpty x -> Ok ((),input)
    | x -> 
        match character (Seq.head x) input with
        | Error (e,input) -> Error (e,input)
        | Ok ((),input) -> 
            literal (Seq.tail x) input
