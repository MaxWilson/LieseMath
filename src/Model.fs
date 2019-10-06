module Model
open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

type AnswerState = | NeedsReview | Good | NoAnswer | ChromeOnly
module Enums =
    type MathKey = | Number of int * string | Enter | Backspace | HintKey
    type MathType = | Plus | Minus | Times | Divide
    type MathBase = Binary | Decimal | Hex
    let numberKey x = Number(x, x.ToString())
    let DecimalKeys = [1..9] |> List.map numberKey|> (fun x -> List.append x [Backspace; numberKey 0; Enter; HintKey])
    let HexKeys = [1..9] |> List.map numberKey |> (fun x -> List.append x [Number(10, "A"); Number(11, "B"); Number(12, "C"); Number(13, "D"); Number(14, "E"); Number(15, "F"); Backspace; numberKey 0; Enter; HintKey])
    let BinaryKeys = [Backspace; numberKey 1; numberKey 0; Enter; HintKey]
    let mathTypeMappings = [Plus, "+"; Minus, "−"; Times, "×"; Divide, "÷"]

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

let ComputeHints size mathBase mathType =
    match mathType with
    | Enums.Plus | Enums.Minus ->
        [for x in 0..size ->
            [for y in 0..size ->
                FormatByBase mathBase (x+y), ref (if x = 0 || y = 0 then ChromeOnly else NoAnswer)
                ]
            ]
    | Enums.Times | Enums.Divide ->
        [for x in 1..size ->
            [for y in 1..size ->
                FormatByBase mathBase (x*y), ref NoAnswer
                ]
            ]

let ComputeProblem opType j k mathBase =
    let makeProb lhs rhs =
        let symbol = Enums.mathTypeMappings |> Seq.find (fun (k,v) -> k = opType) |> snd
        sprintf "%s %s %s" (FormatByBase mathBase lhs) symbol (FormatByBase mathBase rhs)
    let prob, ans =
        match opType with
        | Enums.Plus -> makeProb j k, FormatByBase mathBase (j + k)
        | Enums.Minus ->
            makeProb (j+k) j, FormatByBase mathBase k
        | Enums.Times -> makeProb j k, FormatByBase mathBase (j * k)
        | Enums.Divide ->
            makeProb (j*k) j, FormatByBase mathBase k
    (j, k, prob, ans)

let FormatProblem mathType mathBase lhs rhs =
    let symbol = Enums.mathTypeMappings |> Seq.find (fun (k,v) -> k = mathType) |> snd
    sprintf "%s %s %s" (FormatByBase mathBase lhs) symbol (FormatByBase mathBase rhs)

/// PersistentSetting is a setting which is cached between sessions in local storage
type PersistentSetting<'a when 'a: equality>(name: string, defaultValue: 'a) =
    let key = "Setting." + name
    let mutable storedValue =
        let stringVal = Fable.Import.Browser.localStorage.[key]
        if stringVal = null then
            defaultValue
        else
            match Thoth.Json.Decode.Auto.fromString<'a>(unbox<string> stringVal) with
            | Ok v -> v
            | Error e -> failwithf "Could not decode %A" stringVal
    member this.Value
        with get() = storedValue
        and set(v) =
            if storedValue <> v then
                Fable.Import.Browser.localStorage.[key] <- Thoth.Json.Encode.Auto.toString(1, v)
                storedValue <- v

// I don't trust JS.Math.random() (samples don't seem very independent) so instead of using it directly via Math.random() < prob-as-decimal I transform it a bit
let prob percentage =
    JS.Math.random() < (float percentage)/100.

type MathProblems(onCorrect: _ -> _, onIncorrect: _ -> _) =
    let size = PersistentSetting("size", 12);
    let mathBase = PersistentSetting("mathBase", Enums.Decimal)
    let mathType = PersistentSetting("mathType", Enums.Times)
    let autoEnter = PersistentSetting("autoEnter", false)
    let progressiveDifficulty = PersistentSetting("progressiveDifficulty", false)
    let mutable reviewList = []
    let mutable cells : (string * AnswerState ref) list list = ComputeHints size.Value mathBase.Value mathType.Value
    let cellFor mathType x y =
        match mathType with
            | Enums.Plus | Enums.Minus -> cells.[x].[y] |> snd
            | Enums.Times | Enums.Divide -> cells.[x-1].[y-1] |> snd
    let nextProblem() =
        // 30% of the time it will backtrack to one you got wrong before
        if(prob 30 && reviewList.Length > 0) then
            let (j, k, prob, correctAnswer, _) = reviewList.[(JS.Math.random() * 1000. |> int) % reviewList.Length]
            (j, k, prob, correctAnswer)
        else
            let nextNumber() = (JS.Math.random() * (float size.Value) |> int) + 1
            // in progressive difficulty mode, 70% of the time, it will pick a problem you haven't answered correctly yet
            if (progressiveDifficulty.Value && prob 70) then
                // if all already answered, grow to next level
                let flattenedCells = Seq.concat cells
                // note that <> does not work right with union types currently so we have to use match instead
                if not (flattenedCells |> Seq.exists (fun (v, c) -> match !c with | Good | ChromeOnly -> false | _ -> true)) then
                    let bigger = size.Value + 1
                    size.Value <- bigger
                    let newHints = ComputeHints bigger mathBase.Value mathType.Value
                    for oldrow, newrow in newHints |> Seq.zip cells do
                        for oldcell, newcell in newrow |> Seq.zip oldrow do
                            // note that <> does not work right with union types currently so we have to use match instead
                            match !(snd oldcell) with
                            | Good | NeedsReview as v -> (snd newcell) := v
                            | _ -> ()
                    cells <- newHints
                let mutable j, k = nextNumber(), nextNumber()
                // note that <> does not work right with union types currently so we have to use match instead
                while(match !(cellFor mathType.Value j k) with
                        | Good -> true
                        | _ -> false) do
                    j <- nextNumber()
                    k <- nextNumber()
                ComputeProblem mathType.Value j k mathBase.Value
            else
                let j, k = nextNumber(), nextNumber()
                ComputeProblem mathType.Value j k mathBase.Value
    let mutable problem = nextProblem()
    let mutable score = 0
    let mutable currentAnswer = "";
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
    member this.ProgressiveDifficulty
        with get() = progressiveDifficulty.Value
        and set(v) = progressiveDifficulty.Value <- v
    member this.MathType
        with get() = mathType.Value
        and set(v) = mathType.Value <- v
    member this.Score = score
    member this.CurrentProblem =
        let _, _, prob, _ = problem
        sprintf "%s = %s" prob (if currentAnswer.Length > 0 then currentAnswer else "??")
    member this.Advance() =
        if currentAnswer.Length > 0 then
            let x, y, prob, ans = problem
            let answerCell = cellFor mathType.Value x y
            if ans = currentAnswer then
                score <- score + 100
                answerCell := Good
                if reviewList |> Seq.exists (fun (j, k, _, _, _) -> j = x && k = y) then
                    // now that they've got it correct, eliminate it from the review list
                    reviewList <- reviewList |> List.filter (fun (j, k, _, _, _) -> not (j = x && k = y))
                onCorrect()
            else
                score <- score - 100
                answerCell := NeedsReview
                reviewList <- ((x, y, prob, ans, currentAnswer) :: reviewList)
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
            let _, _, _, ans = problem
            if ans.Length = currentAnswer.Length then
                this.Advance()
    member this.Backspace() =
        if(currentAnswer.Length > 0) then
            currentAnswer <- currentAnswer.Substring(0, currentAnswer.Length - 1)
    member this.Reset() =
        score <- 0
        currentAnswer <- ""
        reviewList <- []
        cells <- ComputeHints size.Value mathBase.Value mathType.Value
        problem <- nextProblem()
    member this.Keys = match mathBase.Value with | Decimal -> DecimalKeys | Hex -> HexKeys | Binary -> BinaryKeys
