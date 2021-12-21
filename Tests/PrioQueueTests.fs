namespace ComradeVanti.FDijkstra

open ComradeVanti.FDijkstra.PrioQueue
open ComradeVanti.FDijkstra.PrioQueueGen
open FsCheck.Xunit
open global.Xunit

[<Properties(Arbitrary = [| typeof<ArbPrioQueues> |])>]
module PrioQueueTests =

    [<Fact>]
    let ``The empty queue has no items`` () =
        Assert.Equal(PrioQueue.empty |> PrioQueue.count, 0)

    [<Property>]
    let ``Adding an item, increases the count by one`` (IntQueue queue) item =
        queue
        |> PrioQueue.add item (item * 2)
        |> PrioQueue.count
        |> (=) ((queue |> PrioQueue.count) + 1)

    [<Property>]
    let ``Adding an item, does not change any existing items``
        (IntQueue queue)
        item
        =
        let added = queue |> PrioQueue.add item (item * 2)

        queue
        |> PrioQueue.items
        |> List.forall (fun i -> added |> PrioQueue.contains i)

    [<Property>]
    let ``Adding an item, keeps the correct prio-order`` (IntQueue queue) item =

        let isInCorrectOrder ((_, p1), (_, p2)) = p1 <= p2

        queue
        |> PrioQueue.add item (item * 2)
        |> PrioQueue.itemsWithPrio
        |> List.pairwise
        |> List.forall isInCorrectOrder

    [<Property>]
    let ``Popping an item, reduces the count by one`` (IntQueue queue) =
        queue
        |> PrioQueue.tryPop
        |> snd
        |> PrioQueue.count
        |> (=) ((queue |> PrioQueue.count) - 1)

    [<Property>]
    let ``Popping an item, returns the first item`` (IntQueue queue) =
        queue
        |> PrioQueue.tryPop
        |> fst
        |> function
            | Some item -> item = (queue |> PrioQueue.items |> List.head)
            | None -> true

    [<Property>]
    let ``Changing priority, does not change count`` (IntQueue queue) =
        let item, prio = queue |> PrioQueue.itemsWithPrio |> List.head

        queue
        |> PrioQueue.changePrio item (prio * 2)
        |> PrioQueue.count
        |> (=) (queue |> PrioQueue.count)

    [<Property>]
    let ``Changing priority, removes it with the old priority``
        (IntQueue queue)
        =
        let item, prio = queue |> PrioQueue.itemsWithPrio |> List.head

        queue
        |> PrioQueue.changePrio item (abs (prio * 2) + 1)
        |> PrioQueue.itemsWithPrio
        |> (not << List.contains (item, prio))
