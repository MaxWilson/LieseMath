module Model
open Fable.Import
open Domain.Equation

type Mode = Homework | Game
type Activity = Help | EquationEntry | DataEntry
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
    formula: (string * Equation) option
    entries: Entry []
}

let fresh = {
    mode = Homework
    activity = EquationEntry
    rawFormula = ""
    formula = None
    entries = Array.empty
    }
