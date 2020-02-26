namespace OurParserC

type binary<'left,'right> = 
| Left of 'left
| Right of 'right

type parsed<'a> = Result<'a * input,exn * input>

module Parsed =
    exception ParsedException of exn*input
    let map (f:'a->'b) : 'a parsed -> 'b parsed = Result.map (fun (p,i) ->f p,i)
    let mapError (f:exn->exn) : 'a parsed -> 'a parsed = Result.mapError (fun (e,i) -> f e,i)
    let replaceError (e:exn) = mapError (fun _ -> e)
    let replace (t:'target) : 'a parsed -> 'target parsed = map (fun _ -> t)
    let get : 'a parsed -> 'a = function
    | Error (e,i) -> raise (ParsedException(e,i))
    | Ok (x,_) -> x
    let raise : 'a parsed -> 'a parsed = function
    | Error (e,i) -> raise (ParsedException(e,i))
    | x -> x
    let ignore<'a,'b> : 'a parsed -> unit parsed = map global.ignore
    let fst<'a,'b> : ('a * 'b) parsed -> 'a parsed = map global.fst
    let snd<'a,'b> : ('a * 'b) parsed -> 'b parsed = map global.snd
    let flatBinary<'a> : binary<'a,'a> parsed -> 'a parsed = 
        map (function
        | Left a -> a
        | Right a -> a)
    let bind<'a,'b> (f:'a -> Result<'b,exn>) : 'a parsed -> 'b parsed = 
        Result.bind (fun (a,input) ->
            match f a with
            | Ok b -> Ok (b,input)
            | Error ex -> Error (ex,input))



