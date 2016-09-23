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
        (j, k, j*k)
    let mutable problem = nextProblem()
    let mutable score = 0
    let cells = [1..size] |> List.map (fun x -> [1..size] |> List.map (fun y -> x * y, ref NoAnswer))
    member x.Score = score
    member x.CurrentProblem = 
        let j, k, _ = problem
        sprintf "%d x %d = ??" j k
    member x.Advance() =
        score <- score + 1
        let x, y, ans = problem
        let answerCell = cells.[x-1].[y-1] |> snd
        answerCell := if x % 2 = 0 then NeedsReview else Good
        problem <- nextProblem()
    member this.HintCells =
        cells