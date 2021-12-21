[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module ComradeVanti.FDijkstra.Dijkstra

open System
open Microsoft.FSharp.Collections
open ComradeVanti.FDijkstra.PrioQueue
open Microsoft.FSharp.Core

let solveAll start vertices neighbors distanceBetween =

    let mutable queue = PrioQueue.empty
    let mutable dists = Map.empty
    let mutable prevs = Map.empty

    dists <- dists |> Map.add start 0

    vertices
    |> List.iter
        (fun v ->
            if v <> start then
                dists <- dists |> Map.add v Int32.MaxValue

            prevs <- prevs |> Map.add v None
            queue <- queue |> PrioQueue.add v dists.[v])

    while not <| (queue |> PrioQueue.isEmpty) do
        let u, newQueue = queue |> PrioQueue.tryPop
        queue <- newQueue
        let u = Option.get u

        u
        |> neighbors
        |> List.filter (fun v -> queue |> PrioQueue.contains v)
        |> List.iter
            (fun v ->
                let alt = dists.[u] + distanceBetween u v

                if alt < dists.[v] then
                    dists <- dists |> Map.add v alt
                    prevs <- prevs |> Map.add v (Some u)
                    queue <- queue |> PrioQueue.changePrio v alt)

    prevs

let solve start goal vertices neighbors distance =

    let prevs = solveAll start vertices neighbors distance

    let rec makePath current =
        match prevs.[current] with
        | Some prev -> (makePath prev) @ [ current ]
        | None -> [ current ]

    makePath goal
