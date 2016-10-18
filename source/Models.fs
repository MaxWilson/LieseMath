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

/// PersistentSetting is a setting which is cached between sessions in local storage
type PersistentSetting<'a when 'a: equality>(name: string, defaultValue: 'a) =
    let key = "Setting." + name
    let mutable storedValue =
        let stringVal = Fable.Import.Browser.localStorage.[key]
        if stringVal = null then
            defaultValue
        else
            Fable.Core.JsInterop.ofJson<'a>(unbox stringVal)
    member this.Value
        with get() = storedValue
        and set(v) =
            if storedValue <> v then
                Fable.Import.Browser.localStorage.[key] <- Fable.Core.JsInterop.toJson(v)
                storedValue <- v

type MathProblems(onCorrect: _ -> _, onIncorrect: _ -> _) =
    let size = PersistentSetting("size", 12);
    let mathBase = PersistentSetting("mathBase", Enums.Decimal)
    let mathType = PersistentSetting("mathType", Enums.Times)
    let autoEnter = PersistentSetting("autoEnter", false)
    let mutable reviewList = []
    let nextProblem() =
        // 30% of the time it will backtrack to one you got wrong before
        if(JS.Math.random() < 0.30 && reviewList.Length > 0) then
            let (j, k, correctAnswer, _) = reviewList.[(JS.Math.random() * 1000. |> int) % reviewList.Length]
            (j, k, correctAnswer)
        else
            let nextNumber() = (JS.Math.random() * (float size.Value) |> int) + 1
            let j, k = nextNumber(), nextNumber()
            (j, k, ComputeAnswer mathType.Value mathBase.Value j k)
    let mutable problem = nextProblem()
    let mutable score = 0
    let mutable currentAnswer = "";
    let mutable cells = [1..size.Value] |> List.map (fun x -> [1..size.Value] |> List.map (fun y -> ComputeAnswer mathType.Value mathBase.Value x y, ref NoAnswer))
    member this.MathBase
        with get() = mathBase.Value
        and set(v) =
            mathBase.Value <- v
    member this.MaxNum
        with get() = size.Value
        and set(v) =
            size.Value <- v
    member this.AutoEnter
        with get() = autoEnter.Value
        and set(v) = autoEnter.Value <- v
    member this.MathType = mathType
    member this.Score = score
    member this.CurrentProblem =
        let j, k, _ = problem
        sprintf "%s = %s" (FormatProblem mathType mathBase.Value j k) (if currentAnswer.Length > 0 then currentAnswer else "??")
    member this.Advance() =
        if currentAnswer.Length > 0 then
            let x, y, ans = problem
            let answerCell = cells.[x-1].[y-1] |> snd
            if ans = currentAnswer then
                score <- score + 100
                answerCell := Good
                if reviewList |> Seq.exists (fun (j, k, _, _) -> j = x && k = y) then
                    // now that they've got it correct, eliminate it from the review list
                    reviewList <- reviewList |> List.filter (fun (j, k, _, _) -> not (j = x && k = y))
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
        if n < (match mathBase.Value with Decimal -> 10 | Hex -> 16 | Binary -> 2) then
            currentAnswer <- currentAnswer + (if n < 10 then n.ToString() else (65 + (n - 10)) |> char |> string)
        if this.AutoEnter then
            let _, _, ans = problem
            if ans.Length = currentAnswer.Length then
                this.Advance()
    member this.Backspace() =
        if(currentAnswer.Length > 0) then
            currentAnswer <- currentAnswer.Substring(0, currentAnswer.Length - 1)
    member this.Reset() =
        score <- 0
        currentAnswer <- ""
        reviewList <- []
        cells <- [1..size.Value] |> List.map (fun x -> [1..size.Value] |> List.map (fun y -> ComputeAnswer mathType.Value mathBase.Value x y, ref NoAnswer))
        problem <- nextProblem()
    member this.Keys = match mathBase.Value with | Decimal -> DecimalKeys | Hex -> HexKeys | Binary -> BinaryKeys