module OurParserC.ExprChain

open OurParserC
open Parser
open Parsers

type Expr<'atom,'operator> =
| Atom of 'atom
| SubExpr of 'operator * Expr<'atom,'operator> * Expr<'atom,'operator>

let chainl<'atom,'operator> (atomParser:Expr<'atom,'operator> parser) (opParser:'operator parser) input =
    let rec rest x rem : Expr<'atom,'operator> parsed =
        match opParser rem with
        | Error _ -> Ok (x,rem)
        | Ok (f,rem2) ->
            match atomParser rem2 with
            | Error _ -> Error (NotMatched,rem2)
            | Ok (b,rem3) ->
                match rest (SubExpr (f,x,b)) rem3 with
                | Ok x -> Ok x
                | Error _ -> Ok (SubExpr (f,x,b), rem3)
    match atomParser input with
    | Error x -> Error x
    | Ok (a,rem) -> rest a rem

