// Adapted from
// https://blog.svpino.com/2015/05/08/solution-to-problem-5-and-some-other-thoughts-about-this-type-of-questions

[<EntryPoint>]
let main argv = 

    let GetValidExpressions (numbers : int list) total = 

        let rec getValidExpressions head tail total' = 
            seq {
                match tail with 
                |   []  ->  

                    if head = total' then 
                        yield (abs head).ToString()

                |   head' :: tail'  ->  

                    yield! 
                        getValidExpressions head' tail' (total' - head)
                        |>  Seq.map (sprintf "%d + %s" <| abs head)

                    yield!
                        getValidExpressions -head' tail' (total' - head)
                        |>  Seq.map (sprintf "%d - %s" <| abs head)

                    let concat = (head * 10) + (if head > 0 then head' else -head')

                    yield! 
                        getValidExpressions concat tail' total'
                        |>  Seq.map (sprintf "%s")
            }

        getValidExpressions  numbers.Head numbers.Tail total



    let solutions = 
        GetValidExpressions [1..9] 100 
    
    solutions
    |> Seq.iter (printfn "%s") 

    System.Console.ReadLine () |> ignore
    0 // return an integer exit code
