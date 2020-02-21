module Tests

// finished @ 28 Minutes
// cleaned up @ 49 Minutes

open Xunit

module Calc =

  let private negativesNotAllowed ns =
    ns
    |> List.map string
    |> String.concat ", "
    |> failwithf "negatives not allowed: %s"

  let private checkNegatives xs = 
    match xs |> List.filter ((>) 0) with
    | []   -> xs
    | ns -> negativesNotAllowed ns
    
  let convert (x:string) =
    let [|a; b|] = x.Split('\n', 2)
    a.Trim([|'[';']'|]).Split("][")
    |> Array.fold (fun (x:string) s ->
      x.Replace(s, ",")) b

  let (|Prefixed|_|) (prefix:string) (input:string) =
    if input.StartsWith(prefix)
    then Some input.[prefix.Length..]
    else None

  let cleanup = function
    | Prefixed "//" rest -> convert rest
    | ""                 -> "0"
    | x                  -> x

  let split (x:string) =
    x.Split(',', '\n')
    |> Array.toList

  let add =
    cleanup
    >> split
    >> List.map int
    >> checkNegatives
    >> List.filter ((>=) 1000)
    >> List.sum

[<Fact>]
let testBigNumbers () =
  Assert.Equal(2, Calc.add "2,1001")

[<Fact>]
let testNegativesNumbers () =
  let ex = Assert.Throws(fun () -> Calc.add "-1,3,-2" |> ignore)
  Assert.Equal("negatives not allowed: -1, -2", ex.Message)

[<Fact>]
let testEmptyString () =
  Assert.Equal(0, Calc.add "")

[<Fact>]
let testSeparatedNumbers () =
  Assert.Equal(1, Calc.add "1")
  Assert.Equal(3, Calc.add "2,1")
  Assert.Equal(6, Calc.add "2\n1,3")

[<Fact>]
let testCustomCharDelimiter () =
  Assert.Equal(3, Calc.add "//;\n1;2")

[<Fact>]
let testCustomStringDelimiter () =
  Assert.Equal(6, Calc.add "//[***]\n1***2***3")
  Assert.Equal(6, Calc.add "//[*][%%]\n1%%2*3")
