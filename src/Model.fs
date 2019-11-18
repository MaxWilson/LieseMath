module Model
open Fable.Import
open Domain.Equation

type Mode = Homework | Game
type Activity = | EquationEntry | DataEntry
type VariableName = string
type Status = Correct | Incorrect | Pending

type Entry = {
    answers: Map<VariableName, string>
    leftOutput: string option
    rightOutput: string option
    status: Status
    }

type Model = {
    mode: Mode
    activity: Activity
    rawFormula: string
    formula: (string[] * Equation) option
    userEnteredEquation: Equation option
    entries: Entry list
    error: string option
}

let fresh = {
    mode = Homework
    activity = EquationEntry
    rawFormula = ""
    formula = None
    entries = []
    error = None
    userEnteredEquation = None
    }

let freshEntry variable value = {
    answers = Map.empty |> Map.add variable value
    leftOutput = None
    rightOutput = None
    status = Pending
    }

let checkStatus (variables: string[], formula: Equation) (e: Entry) =
    if e.answers.Count = (variables |> Seq.length) then
        let answers = e.answers |> Map.map (fun variable txt -> Domain.Parse.tryParseNumber txt)
        if answers |> Map.exists (fun _ v -> Option.isNone v) then
            // not ready yet
            e
        else
            let (Equation(lhs, rhs)) = formula
            let left = evaluateElements (fun v -> answers.[v].Value) lhs |> renderNumber |> Some
            let right = evaluateElements (fun v -> answers.[v].Value) rhs |> renderNumber |> Some
            { e with leftOutput = left; rightOutput = right; status = if left = right then Correct else Incorrect }
    else e
