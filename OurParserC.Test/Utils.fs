module OutParserC.Tests.Utils

open NUnit.Framework

let assertParsed parsed target =
    match parsed with
    | Ok (t,_) -> if t <> target then Assert.Fail ()
    | _ -> Assert.Fail ()

let assertParsedError parsed error =
    match parsed with
    | Error (t,_) -> Assert.AreEqual (t,error)
    | _ -> Assert.Fail ()
