module ComradeVanti.FDijkstra.GraphGen

open FsCheck



let genCell = Gen.choose (1, 10) |> Gen.map Cell.make

let genGraphOfSize s =
    genCell
    |> Gen.listOfLength s
    |> Gen.listOfLength s
    |> Gen.map Graph.make

let genGraph = Gen.sized <| genGraphOfSize


type ValidGraph = ValidGraph of Graph

let rec shrink (ValidGraph graph) =
    let width = (graph |> Graph.width) - 1
    let height = (graph |> Graph.height) - 1

    if width > 0 && height > 0 then
        seq {
            let shrunk =
                graph
                |> Graph.cells
                |> List.take height
                |> List.map (List.take width)
                |> Graph.make
                |> ValidGraph

            yield shrunk
            yield! shrink shrunk
        }
    else
        Seq.empty



type ArbGraphs =
    static member Valid =
        Arb.fromGenShrink (genGraph |> Gen.map ValidGraph, shrink)
