namespace ComradeVanti.FDijkstra

open ComradeVanti.FDijkstra.GraphGen
open FsCheck
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

    [<Property>]
    let ``All paths start with the start-vertex`` (ValidGraph graph) =
        (graph |> solve |> List.head) = start

    [<Property>]
    let ``All paths end with the goal-vertex`` (ValidGraph graph) =
        (graph |> solve |> List.last) = (graph |> goal)

    [<Property>]
    let ``All positions in the path are adjacent`` (ValidGraph graph) =

        let isAligned ((x1, y1), (x2, y2)) =
            let xDiff = abs (x1 - x2)
            let yDiff = abs (y1 - y2)

            (xDiff = 1 && yDiff = 0) || xDiff = 0 && yDiff = 1

        let path = graph |> solve

        path
        |> List.pairwise
        |> List.forall isAligned
        |> Prop.label $"Not all positions were adjacent.\nPath: %A{path}"
