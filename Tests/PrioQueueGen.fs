module ComradeVanti.FDijkstra.PrioQueueGen

open FsCheck
open ComradeVanti.FDijkstra.PrioQueue

let genLargerQueue queue =
    gen {
        let! item =
            Arb.generate
            |> Gen.filter (fun i -> not <| (queue |> PrioQueue.contains i))

        let! prio = Gen.choose (0, 100)

        return queue |> PrioQueue.add item prio
    }

let genQueueOfLength l =
    gen {

        let rec genUntilDone cl queue =
            gen {
                if cl = l then
                    return queue
                else
                    let! larger = queue |> genLargerQueue
                    return! larger |> genUntilDone (cl + 1)
            }

        return! genUntilDone 0 PrioQueue.empty
    }

let genQueue = Gen.sized genQueueOfLength


type IntQueue = IntQueue of PrioQueue<int>

type ArbPrioQueues =
    static member Int = genQueue |> Gen.map IntQueue |> Arb.fromGen
