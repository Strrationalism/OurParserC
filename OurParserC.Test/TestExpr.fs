module OurParserC.Tests.TestExpr

open NUnit.Framework
open OurParserC
open Parser
open Parsers
open Utils

type atom = int
type operator = char

type Expr = ExprChain.Expr<atom,operator>

let atomParser0 : Expr parser =
    let numbers = charInSeq (seq{'0'..'9'})
    whitespace0 <+@> (character '-' <|> numbers) <+..> zeroOrMore numbers
    >> Parsed.map (List.toArray >> System.String >> int >> Expr.Atom)
    
let operatorParser0 : operator parser = 
    whitespace0 <+@> charInSeq ['*';'/']

let operatorParser1 : operator parser = 
    whitespace0 <+@> charInSeq ['+';'-']


let chainl = ExprChain.chainl<atom,operator>


let rec atomParser input : Expr parsed =
    atomParser0 <|> (whitespace0 <+@> character '(' <+@> adds <@+> character ')' <@+> whitespace0)
    <| input
and muls input = chainl atomParser operatorParser0 <| input
and adds input = chainl muls operatorParser1 <| input

let rec eval (expr:Expr) =
    match expr with
    | Expr.Atom i -> i
    | Expr.SubExpr (op,o1,o2) ->
        let o1,o2 = eval o1,eval o2
        match op with
        | '+' -> o1 + o2
        | '-' -> o1 - o2
        | '*' -> o1 * o2
        | '/' -> o1 / o2
        | _ -> failwith "Unknown Operator"

[<Test>]
let testExpr () =
    let assertExpr str result =
        adds (Input.create str)
        |> Parsed.map (eval >> (fun x -> Assert.AreEqual (x,result)))
        |> ignore

    assertExpr "(1 + 1) * 1 / 1 + 1 - 1" 2
    assertExpr "1 + 2" 3
    assertExpr "1  + (1*2+3+8/4*6-5+(1*2/2*2) )/5" 3