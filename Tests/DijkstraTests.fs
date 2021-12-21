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

    let private weightIn graph pos =
        graph |> Graph.tryGet pos |> Option.defaultValue 1000

    let private distanceIn graph _ pos2 = pos2 |> weightIn graph

    let private solve graph =
        Dijkstra.solve
            start
            (graph |> goal)
            (graph |> Graph.positions |> Seq.toList)
            (neighborsIn graph)
            (distanceIn graph)

    let private allPathsFor graph =

        let goal = graph |> goal

        let rec findPaths current =

            let unvisited pos = not <| (current |> List.contains pos)

            let head = current |> List.head

            if head = goal then
                [ current |> List.rev ]
            else
                head
                |> neighborsIn graph
                |> List.filter unvisited
                |> function
                    | [] -> []
                    | next ->
                        next
                        |> List.collect (fun pos -> findPaths (pos :: current))

        [ start ] |> findPaths

    let private lengthIn graph path =
        path |> List.skip 1 |> List.map (weightIn graph) |> List.sum

    [<Property>]
    let ``All paths start with the start-vertex`` (SmallGraph graph) =
        (graph |> solve |> List.head) = start

    [<Property>]
    let ``All paths end with the goal-vertex`` (SmallGraph graph) =
        (graph |> solve |> List.last) = (graph |> goal)

    [<Property>]
    let ``All positions in the path are adjacent`` (SmallGraph graph) =

        let isAligned ((x1, y1), (x2, y2)) =
            let xDiff = abs (x1 - x2)
            let yDiff = abs (y1 - y2)

            (xDiff = 1 && yDiff = 0) || xDiff = 0 && yDiff = 1

        let path = graph |> solve

        path
        |> List.pairwise
        |> List.forall isAligned
        |> Prop.label $"Not all positions were adjacent.\nPath: %A{path}"

    [<Property>]
    let ``The path is the shortest possible path`` (SmallGraph graph) =
        let path = graph |> solve
        let length = path |> lengthIn graph

        let shortest =
            allPathsFor graph
            |> List.sortBy (lengthIn graph)
            |> List.head

        let shortestLength = shortest |> lengthIn graph

        shortestLength = length
        |> Prop.label
            $"\nFound:\t\t%A{path} (%d{length})\nShortest:\t%A{shortest} (%d{shortestLength})"

    [<Property(MaxTest = 1)>]
    let ``Works for large graphs`` (LargeGraph graph) =
        graph |> solve |> ignore
        true
