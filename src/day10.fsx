let testInput =
    [ 28
      33
      18
      42
      31
      14
      46
      20
      48
      47
      24
      23
      49
      45
      19
      38
      39
      11
      1
      32
      25
      35
      8
      17
      7
      9
      4
      2
      34
      10
      3 ]

let testInput2 =
    [ 16
      10
      15
      5
      1
      11
      7
      19
      6
      12
      4 ]

let problemData =
    [ 99
      128
      154
      160
      61
      107
      75
      38
      15
      11
      129
      94
      157
      84
      121
      14
      119
      48
      30
      10
      55
      108
      74
      104
      91
      45
      134
      109
      164
      66
      146
      44
      116
      89
      79
      32
      149
      1
      136
      58
      96
      7
      60
      23
      31
      3
      65
      110
      90
      37
      43
      115
      122
      52
      113
      123
      161
      50
      95
      150
      120
      101
      126
      151
      114
      127
      73
      82
      162
      140
      51
      144
      36
      4
      163
      85
      42
      59
      67
      64
      86
      49
      2
      145
      135
      22
      24
      33
      137
      16
      27
      70
      133
      130
      20
      21
      83
      143
      100
      41
      76
      17 ]

let maxPower = problemData |> List.max

let diffs =
    [ 0; maxPower + 3 ]
    @ problemData
    |> List.sort
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)

let joltsDiff1 =
    diffs |> List.filter ((=) 1) |> List.length

let joltsDiff3 =
    diffs |> List.filter ((=) 3) |> List.length

let result = joltsDiff1 * joltsDiff3

let getNextAdapters adapters joltage =
    adapters
    |> List.filter (fun a ->
        (a = joltage
         + 1
         || a = joltage + 2
         || a = joltage + 3))



let rec solve availableAdapters chain =
    match availableAdapters with
    | [] -> chain
    | lst ->
        match (getNextAdapters availableAdapters (chain |> List.last)) with
        | [] -> chain
        | arr -> solve (chain @ arr) (availableAdapters |> List.except arr)

let getValidChain chain max = 
    match (chain |> List.contains max) with
    | true -> chain |> List.distinct
    | _ -> []


let rec mutateChain adaptersChain acc max: int list list =
    match adaptersChain with
    | [] -> [ getValidChain acc max ]
    | head :: t ->
        match (head < (acc |> List.max)) with
        | true -> mutateChain t acc max
        | false ->
            match (getNextAdapters adaptersChain head) with
            | [] -> [getValidChain (acc@[head]) max]
            | [ a ] ->
                [ getValidChain (acc @ [ head; a ]) max ]
                @ mutateChain t ((acc @ [ head; a ])|> List.distinct) max
            | [ a; b ] ->
                [ getValidChain (acc @ [ head; a ]) max
                  getValidChain (acc @ [ head; b ]) max
                  getValidChain (acc @ [ head; a; b ]) max ]
                @ mutateChain t ((acc @ [ head; a ])|> List.distinct) max
                @ mutateChain t ((acc @ [ head; b ])|> List.distinct) max
                @ mutateChain t ((acc @ [ head; a; b ])|> List.distinct) max
            | [ a; b; c ] ->
                [ getValidChain (acc @ [ head; a ]) max
                  getValidChain (acc @ [ head; b ]) max
                  getValidChain (acc @ [ head; c ]) max
                  getValidChain (acc @ [ head; a; b ]) max
                  getValidChain (acc @ [ head; a; c ]) max
                  getValidChain (acc @ [ head; b; c ]) max
                  getValidChain (acc @ [ head; a; b; c ]) max ]
                @ mutateChain t ((acc @ [ head; a ])|> List.distinct) max
                @ mutateChain t ((acc @ [ head; b ])|> List.distinct) max
                @ mutateChain t ((acc @ [ head; c ])|> List.distinct) max
                @ mutateChain t ((acc @ [ head; a; b ])|> List.distinct) max
                @ mutateChain t ((acc @ [ head; a; c ])|> List.distinct) max
                @ mutateChain t ((acc @ [ head; b; c ])|> List.distinct) max
                @ mutateChain t ((acc @ [ head; a; b; c ])|> List.distinct) max
            | lst -> failwith ("not possible")

let sortedTestInput = testInput2 |> List.sort
let max = (sortedTestInput |> List.max)
let muts =
    (mutateChain sortedTestInput [ 0 ] max )
    |> Set.ofList
    |> Seq.toList
// sortedTestInput2
// |> List.map (fun a ->
//     [ [ a ]
//       getNextAdapters sortedTestInput2 a ])
