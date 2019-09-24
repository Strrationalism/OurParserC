module OutParserC.Tests.TestInputTestParsed

open NUnit.Framework
open OurParserC

[<Test>]
let testMap () =
    let a = Ok (1,Input.create "")
    let b = Parsed.map float a
    match b with
    | Ok (1.0,_) -> ()
    | _ -> Assert.Fail ()

[<Test>]
let testIgnore () =
    let a = Ok (1,Input.create "")
    match Parsed.ignore a with
    | Ok ((),_) -> ()
    | _ -> Assert.Fail ()

[<Test>]
let testFstSnd () =
    let a = Ok ((1,2),Input.create "")
    match Parsed.fst a with
    | Ok (1,_) -> ()
    | _ -> Assert.Fail ()

    match Parsed.snd a with
    | Ok (2,_) -> ()
    | _ -> Assert.Fail ()