module Models
open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

type AnswerState = | NeedsReview | Good | NoAnswer

type MathProblems(size: int) =
    let nextProblem() =
        let nextNumber() = (JS.Math.random() * (float size) |> int) + 1
        let j, k = nextNumber(), nextNumber()
        (j, k, (j*k).ToString())
    let mutable problem = nextProblem()
    let mutable score = 0
    let mutable currentAnswer = "";
    let mutable reviewList = []
    let mutable cells = [1..size] |> List.map (fun x -> [1..size] |> List.map (fun y -> x * y, ref NoAnswer))
    member this.Score = score
    member this.CurrentProblem =
        let j, k, _ = problem
        sprintf "%d x %d = %s" j k (if currentAnswer.Length > 0 then currentAnswer else "??")
    member this.Advance() =
        let x, y, ans = problem
        let answerCell = cells.[x-1].[y-1] |> snd
        if ans = currentAnswer then
            score <- score + 100
            answerCell := Good
        else
            score <- score - 100
            answerCell := NeedsReview
            reviewList <- ((x, y, ans, currentAnswer) :: reviewList)
        currentAnswer <- "";
        problem <- nextProblem()
    member this.HintCells =
        cells
    member this.ReviewList =
        reviewList
    member this.KeyPress (n: int) =
        currentAnswer <- currentAnswer + n.ToString()
    member this.Backspace() =
        if(currentAnswer.Length > 0) then
            currentAnswer <- currentAnswer.Substring(0, currentAnswer.Length - 1)
    member this.Reset() =
        score <- 0
        currentAnswer <- ""
        reviewList <- []
        cells <- [1..size] |> List.map (fun x -> [1..size] |> List.map (fun y -> x * y, ref NoAnswer))
        problem <- nextProblem()