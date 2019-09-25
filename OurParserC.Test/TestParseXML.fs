module OurParserC.Tests.TestParseXML

open NUnit.Framework
open OurParserC
open Parser
open Parsers
open Utils

let quotedString : string parser =
    character '\"' <+@> zeroOrMore (pred anyChar ((<>) '\"')) <@+> character '\"'
    >> Parsed.map (List.toArray >> System.String)

[<Test>]
let testQuotedString () =
    assertParser quotedString "\"super parser\"  " "super parser"
    assertParserError quotedString "nonono" (NotMatched)

let lowerCase = charInSeq (seq{'a'..'z'})
let upperCase = charInSeq (seq{'A'..'Z'})
let numbers = charInSeq (seq{'0'..'9'})

let idenifier : string parser =
    (lowerCase <|> upperCase) 
    <+..> zeroOrMore (lowerCase <|> upperCase <|> character '-' <|> numbers)
    >> Parsed.map (List.toArray >> System.String)

type XMLAttribute = {
    key : string
    value : string
}

let attribute =
    idenifier <@+> character '=' <+> quotedString
    >> Parsed.map (fun (a,b) -> { key = a; value = b })

[<Test>]
let testAttribute () =
    assertParser attribute "superAttribute=\"super\"" {key = "superAttribute";value = "super"}
    assertParserError attribute "super\"stup\"" NotMatched

let elementHead = character '<' <+@> whitespace0 <+@> idenifier <+> zeroOrMore (whitespace0 <+@> attribute)

type XMLElement = {
    tag : string
    attributes : XMLAttribute list
    elements : XMLElement list
}

let singleElement =
    elementHead <@+> (character '/' <+> whitespace0 <+> character '>')
    >> Parsed.map (fun x -> {tag = fst x; attributes = snd x;elements = []})

let testSingleElement =
    assertParser 
        singleElement 
        "< superTag   a1=\"1\"  a2=\"2\"/>" 
        {
            tag = "superTag" 
            attributes = [
                {key="a1";value="1"}
                {key="a2";value="2"}
            ]
            elements = []
        }

let closeElement openElementTag =
    character '<' 
    <+> whitespace0 
    <+> character '/' 
    <+> whitespace0
    <+> pred idenifier ((=) openElementTag) 
    <+> whitespace0
    <+> character '>'
    >> Parsed.ignore

let openElement =
    elementHead <@+> whitespace0 <@+> character '>'

let parentElement (childrenParser:XMLElement list parser) =
    openElement @-> (fun (tag,attributes) ->
        childrenParser <@+> closeElement tag
        >> Parsed.map (fun elements ->
        {
            tag = tag
            attributes = attributes
            elements = elements
        }))

let rec parseXML input : XMLElement list parsed =
    zeroOrMore (whitespaceWrapper (singleElement <|> parentElement parseXML))
    <| input

[<Test>]
let testParseXML () =
    let xml = """
    <top label="Top">
        <semi-bottom label="Bottom"/>
        <middle>
            <bottom label="Another bottom"/>
        </middle>
            </top>"""

    let parsed =
        [{
            tag = "top"
            attributes = [ {key = "label"; value = "Top"} ]
            elements = [
                { tag = "semi-bottom"; attributes = [{ key = "label"; value = "Bottom" }]; elements = []}
                { tag = "middle"; attributes = []; elements = [
                    { tag = "bottom"; attributes = [{ key = "label"; value = "Another bottom"}] ; elements = [] }] } ] }]

    assertParser parseXML xml parsed