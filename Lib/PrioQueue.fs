module ComradeVanti.FDijkstra.PrioQueue

open ComradeVanti.FDijkstra


type PrioQueue<'a> = PrioQueue of ('a * int) list


[<RequireQualifiedAccess>]
module PrioQueue =

    let empty = PrioQueue []

    let count (PrioQueue queue) = queue |> List.length

    let isEmpty queue = queue |> count = 0

    let itemsWithPrio (PrioQueue queue) = queue

    let items queue = queue |> itemsWithPrio |> List.map fst

    let contains item queue = queue |> items |> List.contains item

    let add item prio (PrioQueue queue) =
        queue
        |> List.tryFindIndex (fun (_, p) -> p > prio)
        |> function
            | Some index -> queue |> List.insertAt index (item, prio)
            | None -> queue |> List.appendItem (item, prio)
        |> PrioQueue

    let tryPop (PrioQueue queue) =
        match queue with
        | head :: tail -> (Some(fst head), PrioQueue tail)
        | _ -> None, PrioQueue queue

    let changePrio item newPrio (PrioQueue queue) =
        queue
        |> List.filter (fst >> (<>) item)
        |> PrioQueue
        |> add item newPrio
