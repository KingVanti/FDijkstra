[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module internal ComradeVanti.FDijkstra.List

open ComradeVanti.FDijkstra.TupleUtil

let toMapWithValue v list = list |> List.map (withSnd v) |> Map.ofList

let splitAt index list = (list |> List.take index, list |> List.skip index)

let insertAt index item list =
    let before, after = list |> splitAt index
    List.concat [ before; [ item ]; after ]

let appendItem item list = List.append list [ item ]
