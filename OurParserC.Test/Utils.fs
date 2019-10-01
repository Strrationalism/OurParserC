module OurParserC.Tests.Utils

open NUnit.Framework
open OurParserC
open Parsers
open Parser

let assertParsed parsed target =
    match parsed with
    | Ok (t,_) -> if t <> target then Assert.Fail ()
    | _ -> Assert.Fail ()

let assertParser parser src target =
    assertParsed (parser (OurParserC.Input.create src)) target

let assertParsedError parsed error =
    match parsed with
    | Error (t,_) -> Assert.AreEqual (t,error)
    | _ -> Assert.Fail ()

let assertParserError parser src target =
    assertParsedError (parser (OurParserC.Input.create src)) target


let whitespace = charInSeq [' ';'\t';'\n';'\r'] >> Parsed.ignore
let whitespace0 = zeroOrMore whitespace >> Parsed.ignore
let whitespace1 = oneOrMore whitespace >> Parsed.ignore
let whitespaceWrapper parser = whitespace0 <+@> parser <@+> whitespace0
