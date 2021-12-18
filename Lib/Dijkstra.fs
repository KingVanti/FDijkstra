[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module ComradeVanti.FDijkstra.Dijkstra

let private moveTowards g n =
    if g < n then n - 1
    elif g > n then n + 1
    else n

let private x = fst

let private y = snd

let solve start goal vertices neighbors distance =

    let rec walkTowardsGoal current =
        seq {
            yield current

            if current <> goal then

                let moveX = (current |> x) <> (goal |> x)

                let next =
                    if moveX then
                        (current |> x |> moveTowards (goal |> x), current |> y)
                    else
                        (current |> x), current |> y |> moveTowards (goal |> y)

                yield! walkTowardsGoal next
        }

    walkTowardsGoal start |> Seq.toList
