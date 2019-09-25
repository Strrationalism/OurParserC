module OutParserC.Tests.TestParsing

open NUnit.Framework
open OurParserC
open Parsers
open Parser
open Utils

[<Test>]
let testPair () =
    let parserl = (anyChar <@+> anyChar)
    let parserr = (anyChar <+@> anyChar)
    assertParser parserl "12" '1'
    assertParser parserr "12" '2'

    let parsere1 = character '1' <@+> character '3'
    let parsere2 = character '2' <+@> character '2'
    let parsere3 = character '3' <+> character '4'
    assertParserError parsere1 "12" NotMatched
    assertParserError parsere2 "12" NotMatched
    assertParserError parsere3 "12" NotMatched

[<Test>]
let testOr () =
    let parser = character '1' <||||> anyChar
    assertParser parser "12" (Left '1')
    assertParser parser "29" (Right '2')
    let parsere = character 'a' <||||> charInSeq (Seq.singleton '1')
    assertParserError parsere "25" (BinaryException (NotMatched,NotMatched))

[<Test>]
let testFlattedOr () =
    let parser = character '1' <|> character '2'
    assertParser parser "12" '1'
    assertParser parser "21" '2'
    assertParserError parser "3" (BinaryException (NotMatched,NotMatched))

[<Test>]
let testDick () =
    let parser = anyChar @-> character
    assertParser parser "11" '1'
    assertParserError parser "12" NotMatched

[<Test>]
let testMore () =
    let input = "12345ab"
    let parsed = ['1';'2';'3';'4';'5']
    let basicParser = charInSeq (seq { '1' .. '9' })
    assertParser (oneOrMore basicParser) input parsed
    assertParser (zeroOrMore basicParser) input parsed
    let basicParsere = charInSeq (seq { 'a'..'z' })
    assertParserError (oneOrMore basicParsere) input NotMatched
    assertParser (zeroOrMore basicParsere) input []

[<Test>]
let testAlways () =
    assertParser (always 12345) "p" 12345

[<Test>]
let testLiteral () =
    let input = "Super one two three"
    assertParser (literal "Super") input ()


