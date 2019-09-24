namespace OurParserC

type parser<'target> = input -> 'target parsed

module Parser =

    let ignore (a:'a parser) : unit parser = a >> Parsed.ignore
    let fst (a:('a * 'b) parser) : 'a parser = a >> Parsed.fst
    let snd (a:('a * 'b) parser) : 'b parser = a >> Parsed.snd
    let map (f:'a->'b) (a:'a parser) : 'b parser = a >> Parsed.map f
    let mapError (f:exn->exn) (a:'a parser) : 'a parser = a >> Parsed.mapError f
    let flatBinary (a:binary<'a,'a> parser) : 'a parser = a >> Parsed.flatBinary

    let (<+>) (a:'a parser) (b:'b parser) : ('a * 'b) parser = fun input ->
        match a input with
        | Error e -> Error e
        | Ok (first,input) ->
            match b input with
            | Error e -> Error e
            | Ok (second,input) -> Ok ((first,second),input)

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

    let oneOrMore (p:'a parser) : 'a list parser =
        p <+> zeroOrMore p
        >> Parsed.map (fun (head,tail) -> head::tail)

    exception ConditionTestFailed
    let pred (p:'a parser) (condition:'a->bool) : 'a parser =
        p
        >> function
        | Error a -> Error a
        | Ok (a,input) ->
            if condition a then Ok (a,input)
            else Error (ConditionTestFailed,input)

    let (@->) (a:'a parser) (b:'a->'b parser) : 'b parser = fun input ->
        match a input with
        | Error e -> Error e
        | Ok (a,input) -> b a input

