namespace ComradeVanti.FDijkstra

type Cell = int

type Graph = Cells of Cell list list


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
        let h = (graph |> height) - 1
        let w = (graph |> width) - 1

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
