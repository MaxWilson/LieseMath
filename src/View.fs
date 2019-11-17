module View

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Fable
open Fable.React
open Fable.React.Props

open Common
open ViewElement
open Model

[<Emit("parseInt($0, $1)")>]
let parseInt v radix = jsNative

type Cmd =
    | SwitchMode of Mode
    | SwitchActivity of Activity
    | RawFormula of string
    | ChangeFormula of string
    | Error of string option

let view (m:Model.Model) dispatch =
    div [ClassName "ui"][
        div[ClassName "modeSelection"] [
            label[][
                input[Type "radio"; Name "mode"]
                str "Homework"
                ]
            label[][
                input[Type "radio"; Name "mode"]
                str "Game mode"
                ]
            ]
        div[ClassName "help"][
            a[OnClick ignore][str "Help"]
            ]
        div[ClassName "equationEntry"][
            match m.error with
            | None ->
                yield form[OnSubmit (fun e -> e.preventDefault(); m.rawFormula |> ChangeFormula |> dispatch)] [
                    input[Placeholder "Enter an equation, e.g. 2y = -x + 15"; Value m.rawFormula; OnChange(fun e -> e.Value |> RawFormula |> dispatch)]
                    button[Type "submit"][str "OK"]
                    ]
            | Some err ->
                yield span[ClassName "error"][str err]
            ]
        div[ClassName "tableEntry"][
            match m.formula with
            | None -> ()
            | Some(variables, Domain.Equation.Equation(lhs, rhs)) ->
                let redundant = function
                    | Domain.Equation.Variable(_, Domain.Equation.Number(1, (None | Some 1)))::[] -> true
                    | _ -> false
                yield table[][
                    tr[][
                        for v in variables do
                            yield th[][str v]
                        if not (redundant lhs) then
                            yield th[][str (Domain.Equation.renderElements true lhs)]
                        if not (redundant rhs) then
                            yield th[][str (Domain.Equation.renderElements true rhs)]
                        ]
                    tr[][
                        yield td[][input [Value "3"]]
                        yield td[][]
                        if not (redundant lhs) then
                            yield td[][]
                        if not (redundant rhs) then
                            yield td[][]
                        ]
                    tr[][
                        yield td[][input [Value "3"]]
                        yield td[][input [Value "4"]]
                        if not (redundant lhs) then
                            yield td[][str "8"]
                        if not (redundant rhs) then
                            yield td[][str "12"]
                        ]
                    ]
            ]
        div[ClassName "showAnswers"][
            div[][
                button[][str "Show answers"]
                ]
            ]
        ]
