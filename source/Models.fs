module Models
open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

type MathProblems() =
    let nextProblem() =
        let nextNumber() = JS.Math.random() * 12. |> int
        let j, k = nextNumber(), nextNumber()
        (j, k, j*k)
    let mutable problem = nextProblem()
    let mutable score = 0
    member x.Score = score
    member x.CurrentProblem = 
        let j, k, _ = problem
        sprintf "%d x %d = ??" j k
    member x.Advance() =
        score <- score + 1
        problem <- nextProblem()