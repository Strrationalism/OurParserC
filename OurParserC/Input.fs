namespace OurParserC

type input = {
    src : char seq
    position : int

    col : uint32
    row : uint32
}

module Input =
    let create str = {
        src = str
        position = 0
        col = 0u
        row = 0u
    }

    let peek input =
        match Seq.tryItem input.position input.src with
        | None -> None
        | Some x when x = '\n' -> 
            Some(
                x,
                {
                    input with
                        position = input.position + 1
                        row = input.row + 1u
                        col = 0u
                })
        | Some x -> 
            Some(
                x,
                {
                    input with
                        position = input.position + 1
                        col = input.col + 1u
                }
            )

    
