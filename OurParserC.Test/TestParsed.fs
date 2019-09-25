module OurParserC.Tests.TestParsed

open NUnit.Framework
open OurParserC
open Utils

[<Test>]
let testMap () =
    let a = Ok (1,Input.create "")
    let b = Parsed.map uint32 a
    assertParsed b 1u

[<Test>]
let testIgnore () =
    let a = Ok (1,Input.create "") |> Parsed.ignore
    assertParsed a ()

[<Test>]
let testFstSnd () =
    let a = Ok ((1,2),Input.create "")
    assertParsed (Parsed.fst a) 1
    assertParsed (Parsed.snd a) 2
