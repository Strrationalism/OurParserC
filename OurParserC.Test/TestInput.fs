module OutParserC.Tests.TestInput

open NUnit.Framework
open OurParserC

[<Test>]
let createAndPeek () =
    let src ="""1
2"""

    let i1 = Input.create src
    match Input.peek i1 with
    | Some ('1',i2) ->
        Assert.AreEqual (i2.position,1)
        Assert.AreEqual (i2.row,0u)
        Assert.AreEqual (i2.col,1u)

        match Input.peek i2 with
        | Some ('\n',i3) ->
            Assert.AreEqual (i3.position,2)
            Assert.AreEqual (i3.row,1u)
            Assert.AreEqual (i3.col,0u)

            match Input.peek i3 with
            | Some ('2',i4) -> 
                Assert.AreEqual (i4.position,3)
                Assert.AreEqual (i4.row,1u)
                Assert.AreEqual (i4.col,1u)

                match Input.peek i4 with
                | None -> ()
                | _ -> Assert.Fail ()

            | _ -> Assert.Fail ()
        | _ -> Assert.Fail ()


    | _ -> Assert.Fail ()