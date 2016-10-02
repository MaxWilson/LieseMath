module Models
open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

type AnswerState = | NeedsReview | Good | NoAnswer
module Enums =
    type MathKey = | Number of int * string | Enter | Backspace | HintKey
    type MathType = | Plus | Minus | Times | Divide
    type MathBase = Binary | Decimal | Hex
    let numberKey x = Number(x, x.ToString())
    let DecimalKeys = [1..9] |> List.map numberKey|> (fun x -> List.append x [Backspace; numberKey 0; Enter; HintKey])
    let HexKeys = [1..9] |> List.map numberKey |> (fun x -> List.append x [Number(10, "A"); Number(11, "B"); Number(12, "C"); Number(13, "D"); Number(14, "E"); Number(15, "F"); Backspace; numberKey 0; Enter; HintKey])
    let BinaryKeys = [Backspace; numberKey 1; numberKey 0; Enter; HintKey]

open Enums

let FormatByBase mathBase n =
    match mathBase with
    | Decimal -> n.ToString()
    | Binary ->
        let rec binPrint n =
            if n > 1 then
                let next = (binPrint (n / 2))
                next + ((n % 2).ToString())
            else
                n.ToString()
        binPrint n
    | Hex ->
        let hexDigit n =
            match n % 16 with
                | x when x >= 10 -> (65 + (x - 10)) |> char |> string
                | x -> x.ToString()
        let rec hexPrint n =
            if n > 15 then
                let next = hexPrint (n/16)
                let thisDigit = hexDigit n
                next + thisDigit
            else
                hexDigit n
        hexPrint n

let ComputeAnswer mathType mathBase lhs rhs =
    let ans = (match mathType with | Plus -> (+) | Minus -> (-) | Times -> (*) | Divide -> (/)) lhs rhs
    FormatByBase mathBase ans

let FormatProblem mathType mathBase lhs rhs =
    sprintf "%s x %s" (FormatByBase mathBase lhs) (FormatByBase mathBase rhs)
type MathProblems(onCorrect: _ -> _, onIncorrect: _ -> _) =
    let mutable size = 12;
    let mutable mathBase = Enums.Decimal;
    let mutable mathType = Enums.Times
    let nextProblem() =
        let nextNumber() = (JS.Math.random() * (float size) |> int) + 1
        let j, k = nextNumber(), nextNumber()
        (j, k, ComputeAnswer mathType mathBase j k)
    let mutable problem = nextProblem()
    let mutable score = 0
    let mutable currentAnswer = "";
    let mutable reviewList = []
    let mutable cells = [1..size] |> List.map (fun x -> [1..size] |> List.map (fun y -> ComputeAnswer mathType mathBase x y, ref NoAnswer))
    member this.MathBase
        with get() = mathBase
        and set(v) =
            mathBase <- v
    member this.MaxNum
        with get() = size
        and set(v) =
            size <- v
    member this.MathType = mathType
    member this.Score = score
    member this.CurrentProblem =
        let j, k, _ = problem
        sprintf "%s = %s" (FormatProblem mathType mathBase j k) (if currentAnswer.Length > 0 then currentAnswer else "??")
    member this.Advance() =
        if currentAnswer.Length > 0 then
            let x, y, ans = problem
            let answerCell = cells.[x-1].[y-1] |> snd
            if ans = currentAnswer then
                score <- score + 100
                answerCell := Good
                onCorrect()
            else
                score <- score - 100
                answerCell := NeedsReview
                reviewList <- ((x, y, ans, currentAnswer) :: reviewList)
                onIncorrect()
            currentAnswer <- "";
            problem <- nextProblem()
    member this.HintCells =
        cells
    member this.ReviewList =
        reviewList
    member this.KeyPress (n: int) =
        // ignore keys that don't apply to this base
        if n < (match mathBase with Decimal -> 10 | Hex -> 16 | Binary -> 2) then
            currentAnswer <- currentAnswer + (if n < 10 then n.ToString() else (65 + (n - 10)) |> char |> string)
    member this.Backspace() =
        if(currentAnswer.Length > 0) then
            currentAnswer <- currentAnswer.Substring(0, currentAnswer.Length - 1)
    member this.Reset() =
        score <- 0
        currentAnswer <- ""
        reviewList <- []
        cells <- [1..size] |> List.map (fun x -> [1..size] |> List.map (fun y -> ComputeAnswer mathType mathBase x y, ref NoAnswer))
        problem <- nextProblem()
    member this.Keys = match mathBase with | Decimal -> DecimalKeys | Hex -> HexKeys | Binary -> BinaryKeys