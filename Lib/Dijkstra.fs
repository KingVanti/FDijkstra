[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module ComradeVanti.FDijkstra.Dijkstra

open System
open Microsoft.FSharp.Collections

let solveAll start vertices neighbors distance =

    let distances =
        vertices
        |> List.map (fun v -> v, Int32.MaxValue)
        |> Map.ofList
        |> Map.add start 0

    let prev = vertices |> List.map (fun v -> v, None) |> Map.ofList

    let findMinDistVertex =
        Set.toList
        >> List.sortBy (fun u -> distances.[u])
        >> List.head

    let rec searchUntilDone unvisited (distances, prev) =
        if unvisited |> Set.isEmpty then
            prev
        else
            let u = unvisited |> findMinDistVertex

            let checkNeighbor (distances, prev) v =
                let alt = (distances |> Map.find u) + distance u v

                if alt < (distances |> Map.find v) then
                    distances |> Map.add v alt, prev |> Map.add v (Some u)
                else
                    distances, prev

            u
            |> neighbors
            |> List.filter (fun v -> unvisited |> Seq.contains v)
            |> List.fold checkNeighbor (distances, prev)
            |> (searchUntilDone (unvisited |> Set.remove u))

    searchUntilDone (vertices |> Set.ofList) (distances, prev)

let solve start goal vertices neighbors distance =

    let prev = solveAll start vertices neighbors distance

    let rec makePath current =
        match prev.[current] with
        | Some prev -> (makePath prev) @ [ current ]
        | None -> [ current ]

    makePath goal
