namespace OurParserC

type input = {
    src : char array
    pos : int

    col : uint32
    row : uint32
}

module Input =
    let create (str:string) = {
        src = str.ToCharArray()
        pos = 0
        col = 0u
        row = 0u
    }

    let peek input =
        match Array.tryItem input.pos input.src with
        | None -> None
        | Some x when x = '\n' -> 
            Some(
                x,
                {
                    src = input.src
                    row = input.row + 1u
                    col = 0u
                    pos = input.pos + 1
                })
        | Some x -> 
            Some(
                x,
                {
                    input with
                        src = input.src
                        col = input.col + 1u
                        pos = input.pos + 1
                }
            )

    
