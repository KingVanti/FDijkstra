namespace ComradeVanti.FDijkstra

open ComradeVanti.FDijkstra.GraphGen
open FsCheck.Xunit
open Microsoft.FSharp.Core


[<Properties(Arbitrary = [| typeof<ArbGraphs> |])>]
module DijkstraTests =

    let private start = (0, 0)

    let private goal graph =
        ((graph |> Graph.width) - 1, (graph |> Graph.height) - 1)

    let private neighborsIn graph (x, y) =
        [ (x, y + 1); (x + 1, y); (x, y - 1); (x - 1, y) ]
        |> List.filter (fun pos -> graph |> Graph.containsPos pos)

    let private distanceIn graph _ pos2 =
        graph
        |> Graph.tryGet pos2
        |> Option.map Cell.value
        |> Option.defaultValue 1000

    let private solve graph =
        Dijkstra.solve
            start
            (graph |> goal)
            (graph |> Graph.positions)
            (neighborsIn graph)
            (distanceIn graph)
