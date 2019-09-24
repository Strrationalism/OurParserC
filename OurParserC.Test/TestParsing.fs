module OutParserC.Tests.TestParsing

open NUnit.Framework
open OurParserC
open Parsers
open Parser
open Utils

[<Test>]
let testPair () =
    let input = Input.create "12"
    let parserl = (anyChar <@+> anyChar)
    let parserr = (anyChar <+@> anyChar)
    assertParsed (parserl input) '1'
    assertParsed (parserr input) '2'

    let parsere1 = character '1' <@+> character '3'
    let parsere2 = character '2' <+@> character '2'
    let parsere3 = character '3' <+> character '4'
    assertParsedError (parsere1 input) NotMatched
    assertParsedError (parsere2 input) NotMatched
    assertParsedError (parsere3 input) NotMatched

[<Test>]
let testOr () =
    let parser = character '1' <||||> anyChar
    assertParsed (parser (Input.create "12")) (Left ())
    assertParsed (parser (Input.create "29")) (Right '2')
    let parsere = character 'a' <||||> charInSeq (Seq.singleton '1')
    assertParsedError (parsere (Input.create "25")) (BinaryException (NotMatched,NotMatched))

[<Test>]
let testFlattedOr () =
    let parser = character '1' <|> character '2'
    assertParsed (parser (Input.create "1")) ()
    assertParsed (parser (Input.create "2")) ()
    assertParsedError (parser (Input.create "3")) (BinaryException (NotMatched,NotMatched))

[<Test>]
let testMore () =
    let input = Input.create "12345ab"
    let parsed = ['1';'2';'3';'4';'5']
    let basicParser = charInSeq (seq { '1' .. '9' })
    assertParsed (oneOrMore basicParser input) parsed
    assertParsed (zeroOrMore basicParser input) parsed
    let basicParsere = charInSeq (seq { 'a'..'z' })
    assertParsedError (oneOrMore basicParsere input) NotMatched
    assertParsed (zeroOrMore basicParsere input) []

[<Test>]
let testAlways () =
    let input = Input.create "p"
    assertParsed (always 12345 input) 12345

[<Test>]
let testLiteral () =
    let input = Input.create "Super one two three"
    assertParsed (literal "Super" input) ()


