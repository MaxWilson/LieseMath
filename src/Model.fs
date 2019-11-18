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
    }

let freshEntry variable value = {
    answers = Map.empty |> Map.add variable value
    leftOutput = None
    rightOutput = None
    status = Pending
    }
