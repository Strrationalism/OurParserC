namespace OurParserC

type parser<'target> = input -> 'target parsed

module Parser =

    let (<+>) (a:'a parser) (b:'b parser) : ('a * 'b) parser = fun input ->
        match a input with
        | Error e -> Error e
        | Ok (first,input) ->
            match b input with
            | Error e -> Error e
            | Ok (second,input) -> Ok ((first,second),input)

    let (<+..>) (head:'a parser) (tail:'a list parser) : 'a list parser =
        head <+> tail
        >> Parsed.map (fun (a,b) -> a::b)

    let (<@+>) (a:'a parser) (b:'b parser) : 'a parser = a <+> b >> Parsed.fst
    let (<+@>) (a:'a parser) (b:'b parser) : 'b parser = a <+> b >> Parsed.snd

    exception BinaryException of exn * exn
    let (<||||>) (a:'a parser) (b:'b parser) : binary<'a,'b> parser = fun input ->
        match a input with
        | Ok (a,input) -> Ok (Left a,input)
        | Error (e1,_) ->
            match b input with
            | Ok (b,input) -> Ok (Right b,input)
            | Error (e2,_) -> Error (BinaryException (e1,e2),input)

    let (<|>) (a:'a parser) (b:'a parser) : 'a parser = a <||||> b >> Parsed.flatBinary

    let rec zeroOrMore (p:'a parser) : 'a list parser = fun input ->
        match p input with
        | Ok (a,input) ->
            match zeroOrMore p input with
            | Ok (tail,input) -> Ok (a::tail,input)
            | Error _ -> Ok ([a],input)
        | Error _ -> Ok ([],input)

    let oneOrMore p =
        p <+..> zeroOrMore p

    let zeroOrOne (p:'a parser) : 'a option parser = 
        p
        >> function
        | Ok (a,b) -> Ok ((Some a),b)
        | Error (_,b) -> Ok (None,b)
        

    exception ConditionTestFailed
    let pred (p:'a parser) (condition:'a->bool) : 'a parser = fun input ->
        input
        |> p
        |> function
        | Error a -> Error a
        | Ok (a,inputOk) ->
            if condition a then Ok (a,inputOk)
            else Error (ConditionTestFailed,input)

    let (@->) (a:'a parser) (b:'a->'b parser) : 'b parser = fun input ->
        match a input with
        | Error e -> Error e
        | Ok (a,input) -> b a input

