namespace ComradeVanti.FDijkstra

type Cell = CellValue of int

type Graph = Cells of Cell list list


[<RequireQualifiedAccess>]
module Cell =

    let make = CellValue

    let value (CellValue value) = value


[<RequireQualifiedAccess>]
module Graph =

    let make = Cells

    let cells (Cells cells) = cells

    let width graph =
        graph
        |> cells
        |> List.tryHead
        |> function
            | Some list -> list |> List.length
            | None -> 0

    let height graph = graph |> cells |> List.length

    let positions graph =
        let h = graph |> height
        let w = graph |> width

        seq {
            for y in [ 0 .. h ] do
                for x in [ 0 .. w ] do
                    yield (x, y)
        }

    let containsPos (x, y) graph =
        x >= 0
        && y >= 0
        && x < (graph |> width)
        && y < (graph |> height)

    let tryGet (x, y) graph =
        graph
        |> cells
        |> List.tryItem y
        |> Option.bind (List.tryItem x)
