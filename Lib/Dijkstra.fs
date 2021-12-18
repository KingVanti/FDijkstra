[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module ComradeVanti.FDijkstra.Dijkstra

open System
open Microsoft.FSharp.Collections

let solve start goal vertices neighbors distance =

    let mutable distances =
        vertices
        |> List.map (fun v -> v, Int32.MaxValue)
        |> Map.ofList
        |> Map.add start 0

    let mutable prev = vertices |> List.map (fun v -> v, None) |> Map.ofList

    let findMinDistVertex =
        Set.toList
        >> List.sortBy (fun u -> distances.[u])
        >> List.head

    let rec searchUntilDone unvisited =
        if unvisited |> Set.isEmpty then
            ()
        else
            let u = unvisited |> findMinDistVertex

            u
            |> neighbors
            |> List.filter (fun v -> unvisited |> Seq.contains v)
            |> List.iter
                (fun v ->
                    let alt = distances.[u] + distance u v

                    if alt < distances.[v] then
                        distances <- distances |> Map.add v alt
                        prev <- prev |> Map.add v (Some u))

            searchUntilDone (unvisited |> Set.remove u)

    let rec makePath current =
        match prev.[current] with
        | Some prev -> (makePath prev) @ [ current ]
        | None -> [ current ]

    searchUntilDone (vertices |> Set.ofList)
    makePath goal
