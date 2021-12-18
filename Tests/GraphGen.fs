module ComradeVanti.FDijkstra.GraphGen

open FsCheck



let genCell: Gen<Cell> = Gen.choose (1, 10)

let genGraphOfSize s =
    genCell
    |> Gen.listOfLength s
    |> Gen.listOfLength s
    |> Gen.map Graph.make

type LargeGraph = LargeGraph of Graph

type SmallGraph = SmallGraph of Graph

let rec shrink (LargeGraph graph) =
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
                |> LargeGraph

            yield shrunk
            yield! shrink shrunk
        }
    else
        Seq.empty



type ArbGraphs =
    static member Large =
        Arb.fromGenShrink (genGraphOfSize 100 |> Gen.map LargeGraph, shrink)

    static member Small = genGraphOfSize 4 |> Gen.map SmallGraph |> Arb.fromGen
