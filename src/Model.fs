module Model
open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Browser.WebStorage
open Common

type AnswerState = | NeedsReview | Good | NoAnswer | ChromeOnly
type Correctness = NotReady | Correct | Incorrect
module Seq =
    let every pred = not << Seq.exists (not << pred)

module Enums =
    type MathKey = | Number of string | Enter | Backspace | HintKey
    type MathType = | Plus | Minus | Times | Divide
    type MathBase = Binary | Decimal | Hex
    let numberKey x = Number(x.ToString())
    let DecimalKeys = [1..9] |> List.map numberKey|> (fun x -> List.append x [Backspace; numberKey 0; Enter; HintKey]) |> Array.ofList
    let HexKeys = [1..9] |> List.map numberKey |> (fun x -> List.append x [Number "A"; Number "B"; Number "C"; Number "D"; Number "E"; Number "F"; Backspace; numberKey 0; Enter; HintKey]) |> Array.ofList
    let BinaryKeys = [|Backspace; numberKey 1; numberKey 0; Enter; HintKey|]
    let mathTypeMappings = [Plus, "+"; Minus, "−"; Times, "×"; Divide, "÷"]
    let keysOf = function Binary -> BinaryKeys | Decimal -> DecimalKeys | Hex -> HexKeys

open Enums
type SoundState = On | Off | CheerOnly | BombOnly
type Settings = {
    size: int
    mathBase: Enums.MathBase
    mathType: Enums.MathType
    autoEnter: bool
    progressiveDifficulty: bool
    sound: SoundState
    feedbackDuration: int
    } with
    static member Default = {
        size = 12
        mathBase = Decimal
        mathType = Enums.Times
        autoEnter = false
        progressiveDifficulty = true
        sound = On
        feedbackDuration = 1000
    }

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

let ComputeHints settings =
    let size, mathBase, mathType = settings.size, settings.mathBase, settings.mathType
    match mathType with
    | Enums.Plus | Enums.Minus ->
        [for x in 0..size ->
            [for y in 0..size ->
                FormatByBase mathBase (x+y), (if x = 0 || y = 0 then ChromeOnly else NoAnswer)
                ]
            ]
    | Enums.Times | Enums.Divide ->
        [for x in 1..size ->
            [for y in 1..size ->
                FormatByBase mathBase (x*y), NoAnswer
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

// I don't trust JS.Math.random() (samples don't seem very independent) so instead of using it directly via Math.random() < prob-as-decimal I transform it a bit
let prob percentage =
    JS.Math.random() < (float percentage)/100.

let inline persist key value =
    localStorage.[key:string] <- Thoth.Json.Encode.Auto.toString(1, value)

let inline retrievePersisted key defaultValue =
    match localStorage.[key:string] with
    | null -> defaultValue
    | rawValue ->
        match Thoth.Json.Decode.Auto.fromString(unbox<string> rawValue) with
        | Ok v -> v
        | Error _ -> defaultValue

type Review = { lhs: int; rhs: int; problem: string; guess: string; correctAnswer: string }

let coordsFor mathType x y =
    match mathType with
        | Enums.Plus | Enums.Minus -> x,y
        | Enums.Times | Enums.Divide -> x-1,y-1
let cellFor (cells: _ list list) mathType x y =
    let i,j = coordsFor mathType x y
    cells.[i].[j] |> snd

type Game = {
    settings: Settings
    reviewList: Review list
    cells: (string * AnswerState) list list
    problem: {| lhs: int; rhs: int; question: string; answer: string |}
    score: int
    currentAnswer: string
    messageToUser: {| color: string; msg: string |} option
    showOptions: bool
    showHints: bool
    } with
    static member Fresh(?settings) =
        let settings = match settings with | Some v -> v | None -> retrievePersisted "settings" Settings.Default
        {
            settings = settings
            reviewList = []
            cells = ComputeHints settings
            problem = Unchecked.defaultof<_>
            score = 0
            currentAnswer = ""
            messageToUser = None
            showOptions = false
            showHints = false
        } |> Game.nextProblem
    static member nextProblem (g: Game) =
        // 30% of the time it will backtrack to one you got wrong before
        if(prob 30 && g.reviewList.Length > 0) then
            let review = g.reviewList.[(JS.Math.random() * 1000. |> int) % g.reviewList.Length]
            { g with problem = {| lhs = review.lhs; rhs = review.rhs; question = review.problem; answer = review.correctAnswer |} }
        else
            let settings = g.settings
            // in progressive difficulty mode, 70% of the time, it will pick a problem you haven't answered correctly yet until you answer all of them, then it will grow (with 100% probability)
            let flattenedCells = Seq.concat g.cells
            let allCorrect = flattenedCells |> Seq.every (fun (_, c) -> match c with | Good | ChromeOnly -> true | _ -> false)
            if (settings.progressiveDifficulty && (prob 70 || allCorrect)) then
                // if all already answered, grow to next level
                let cells, settings =
                    // note that <> does not work right with union types currently so we have to use match instead
                    if allCorrect then
                        let settings = { settings with size = settings.size + 1 }
                        let newHints =
                            ComputeHints settings
                            |> List.mapi(fun i row ->
                                row |> List.mapi(fun j cell ->
                                        if i < g.cells.Length && j < g.cells.[i].Length then
                                            g.cells.[i].[j]
                                        else
                                            cell
                                    )
                                )
                        newHints, settings
                    else g.cells, settings
                let nextNumber() = (JS.Math.random() * (float settings.size) |> int) + 1
                let mutable j, k = nextNumber(), nextNumber()
                // note that <> does not work right with union types currently so we have to use match instead
                while(match (cellFor cells settings.mathType j k) with
                        | Good -> true
                        | _ -> false) do
                    j <- nextNumber()
                    k <- nextNumber()
                let lhs, rhs, problem, answer = ComputeProblem settings.mathType j k settings.mathBase
                { g with settings = settings; cells = cells; problem = {|lhs = lhs; rhs = rhs; question = problem; answer = answer |}}
            else
                let nextNumber() = (JS.Math.random() * (float settings.size) |> int) + 1
                let j, k = nextNumber(), nextNumber()
                let lhs, rhs, problem, answer = ComputeProblem settings.mathType j k settings.mathBase
                { g with problem = {|lhs = lhs; rhs = rhs; question = problem; answer = answer |}}
    static member CurrentProblem (this: Game) =
        sprintf "%s = %s" this.problem.question (if this.currentAnswer.Length > 0 then this.currentAnswer else "??")
    static member Evaluate (this: Game) =
        let currentAnswer = this.currentAnswer
        if this.currentAnswer.Length > 0 then
            let problem = this.problem
            let (x,y) = coordsFor this.settings.mathType problem.lhs problem.rhs
            let updateCells newValue =
                this.cells |> List.mapi(fun i row ->
                        if x <> i then row else
                            row |> List.mapi(fun j cell ->
                                if y <> j then cell else (fst cell, newValue))
                    )
            if problem.answer = currentAnswer then
                let reviewList' =
                    if this.reviewList |> Seq.exists (fun review -> (review.lhs, review.rhs) = (problem.lhs, problem.rhs)) then
                    // now that they've got it correct, eliminate it from the review list
                        this.reviewList |> List.filter (fun review -> (review.lhs, review.rhs) <> (problem.lhs, problem.rhs))
                    else this.reviewList
                Correct, { this with currentAnswer = ""; score = this.score + 100; cells = updateCells Good; reviewList = reviewList' }
            else
                let reviewList' =
                    { Review.lhs = problem.lhs; rhs = problem.rhs; problem = problem.question; correctAnswer = problem.answer; guess = this.currentAnswer } :: this.reviewList
                Incorrect, { this with currentAnswer = ""; score = this.score - 100; cells = updateCells NeedsReview; reviewList = reviewList' }
        else
            NotReady, this
