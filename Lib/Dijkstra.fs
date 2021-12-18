[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module ComradeVanti.FDijkstra.Dijkstra

open System
open Microsoft.FSharp.Collections

let solveAll start vertices neighbors distanceBetween =

    let mutable distances =
        vertices
        |> List.toMapWithValue Int32.MaxValue
        |> Map.add start 0

    let mutable prevs = vertices |> List.toMapWithValue None
    let mutable unvisited = vertices |> Set.ofList

    let distance v = distances |> Map.find v

    let findMinDistVertex = Set.toList >> List.minBy distance

    let isUnvisited v = unvisited |> Seq.contains v

    let check v =

        unvisited <- unvisited |> Set.remove v

        let checkNeighbor n =
            let alt = (distance v) + distanceBetween v n

            if alt < (distance n) then
                distances <- distances |> Map.add n alt
                prevs <- prevs |> Map.add n (Some v)

        v
        |> neighbors
        |> List.filter isUnvisited
        |> List.iter checkNeighbor
    
    let rec searchUntilDone () =
        if unvisited |> Set.isEmpty then
            ()
        else
            unvisited |> findMinDistVertex |> check
            searchUntilDone ()

    searchUntilDone ()
    prevs

let solve start goal vertices neighbors distance =

    let prevs = solveAll start vertices neighbors distance

    let rec makePath current =
        match prevs.[current] with
        | Some prev -> (makePath prev) @ [ current ]
        | None -> [ current ]

    makePath goal
