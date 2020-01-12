namespace OurParserC

type input = {
    src : char seq

    col : uint32
    row : uint32
}

module Input =
    let create str = {
        src = str
        col = 0u
        row = 0u
    }

    let peek input =
        match Seq.tryHead input.src with
        | None -> None
        | Some x when x = '\n' -> 
            Some(
                x,
                {
                    src = Seq.tail input.src
                    row = input.row + 1u
                    col = 0u
                })
        | Some x -> 
            Some(
                x,
                {
                    input with
                        src = Seq.tail input.src
                        col = input.col + 1u
                }
            )

    
