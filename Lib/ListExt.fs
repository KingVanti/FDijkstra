[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module internal ComradeVanti.FDijkstra.List

open ComradeVanti.FDijkstra.TupleUtil

let toMapWithValue v list = list |> List.map (withSnd v) |> Map.ofList
