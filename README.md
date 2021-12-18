# FDijkstra
An F# implementation of Dijkstras algorithm

The implementation works with all sorts of graphs as long as it fulfills the following requirements:
- It has vertices
- It can find vertices connected to a specific vertex
- It can measure the distance between two connected vertices

The general syntax for using the algorithm is

`ComradeVanti.FDijkstra.Dijkstra.solve start goal vertices neighbors distanceBetween`

with
- `start` is the vertex where from which to begin the search
- `goal` is the vertex where to which a path should be searched
- `vertices` are all the vertices in the graph
- `neighbors` is a function which finds adjacent vertices to a vertex
- `distanceBetween` is a function which measures distance between two connected vertices

An example with actual values could be:

```
let vertices = [ 0; 1; 2; 3 ]

// Tuples are of the form ((vertex1, vertex2), distance)
let edges =
    [ ((0, 1), 2)
      ((1, 2), 4)
      ((2, 3), 3)
      ((1, 3), 7)
      ((0, 2), 4) ]

let neighbors v =
    [ edges
      |> List.filter (fst >> fst >> (=) v)
      |> List.map (fst >> snd)
      edges
      |> List.filter (fst >> snd >> (=) v)
      |> List.map (fst >> fst) ]
    |> List.concat

let distance v1 v2 =
    edges
    |> List.tryFind (fst >> (fun e -> e = (v1, v2) || e = (v2, v1)))
    |> Option.map snd
    |> Option.defaultValue 1000

solve 0 3 vertices neighbors distance
```