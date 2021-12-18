[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module ComradeVanti.FDijkstra.Dijkstra

open System
open Microsoft.FSharp.Collections

let solveAll start vertices neighbors distanceBetween =

    let distances =
        vertices
        |> List.toMapWithValue Int32.MaxValue
        |> Map.add start 0

    let prev = vertices |> List.toMapWithValue None

    let rec searchUntilDone unvisited (distances, prevs) =

        let distance v = distances |> Map.find v

        let findMinDistVertex = Set.toList >> List.minBy distance

        let isUnvisited v = unvisited |> Seq.contains v

        let check v =
            let checkNeighbor (distances, prev) n =
                let alt = (distance v) + distanceBetween v n

                if alt < (distance n) then
                    distances |> Map.add n alt, prev |> Map.add n (Some v)
                else
                    distances, prev

            v
            |> neighbors
            |> List.filter isUnvisited
            |> List.fold checkNeighbor (distances, prevs)
            |> (searchUntilDone (unvisited |> Set.remove v))

        if unvisited |> Set.isEmpty then
            prevs
        else
            unvisited |> findMinDistVertex |> check

    searchUntilDone (vertices |> Set.ofList) (distances, prev)

let solve start goal vertices neighbors distance =

    let prevs = solveAll start vertices neighbors distance

    let rec makePath current =
        match prevs.[current] with
        | Some prev -> (makePath prev) @ [ current ]
        | None -> [ current ]

    makePath goal
