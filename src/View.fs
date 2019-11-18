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
    | Mode of Mode
    | Activity of Activity
    | RawFormula of string
    | Formula of string
    | Error of string option
    | EntryValue of row: int * variable: string * value: string
    | SolveFor of string option
    | ShowAnswers

// an input-like component which stores state locally until blur
let localInput =
    let component' =
        FunctionComponent.Of(
            fun (value, props, onChange) ->
                let v = Hooks.useState value
                let lst : IHTMLProp list = [
                    yield upcast Value v.current
                    yield upcast OnChange(fun e -> if e <> null then v.update(e.Value))
                    yield upcast OnKeyDown(fun e -> if e.keyCode = 13. then
                                                        e.preventDefault()
                                                        onChange v.current
                                                    )
                    yield upcast OnBlur(fun _ -> onChange v.current)
                    yield! props
                    ]
                input lst
            , memoizeWith = (fun (v1, p1, _) (v2, p2, _) -> v1 = v2))
    (fun value (props: seq<IHTMLProp>) onChange -> component'(value, props, onChange))

let view (m:Model.Model) dispatch =
    printfn "Rendering..."
    summarize m |> ignore
    printfn "Done rendering"
    div [ClassName "ui"][
        h1[ClassName "header"][str "Liese's Equation Checker"]
        //div[ClassName "modeSelection"] [
        //    label[][
        //        input[Type "radio"; Name "mode"]
        //        str "Homework"
        //        ]
        //    label[][
        //        input[Type "radio"; Name "mode"]
        //        str "Game mode"
        //        ]
        //    ]
        //div[ClassName "help"][
        //    a[OnClick ignore][str "Help"]
        //    ]
        div[ClassName "equationEntry"][
            match m.error with
            | None ->
                match m.activity with
                | Activity.EquationEntry ->
                    yield form[OnSubmit (fun e -> e.preventDefault(); m.rawFormula |> Formula |> dispatch)] [
                        FunctionComponent.Of(fun () ->
                            let ref: IRefHook<Browser.Types.Element option> = Hooks.useRef None
                            Hooks.useEffect(fun () ->
                                match ref.current with
                                | Some r ->
                                    (r |> unbox<Browser.Types.HTMLInputElement>).blur()
                                | None -> ()
                                )
                            input[AutoFocus true; Placeholder "Enter an equation, e.g. 2y = -x + 15"; Value m.rawFormula; OnChange(fun e -> e.Value |> RawFormula |> dispatch)]
                            )()
                        button[Type "submit"; Disabled (m.rawFormula.Length = 0)][str "OK"]
                        ]
                | DataEntry ->
                    yield form[OnSubmit (fun e -> e.preventDefault(); dispatch(RawFormula ""); dispatch (Activity EquationEntry))] [
                        input[Disabled true; Value m.rawFormula]
                        button[Type "submit"][str "New equation"]
                        ]
            | Some err ->
                yield span[ClassName "error"][str err]
            ]
        div[ClassName "tableEntry"][
            match m.formula with
            | Some(variables, Domain.Equation.Equation(lhs, rhs)) when m.activity = DataEntry ->
                let redundant = function
                    | Domain.Equation.Variable(_, Domain.Equation.Number(1, (None | Some 1)))::[] -> true
                    | _ -> false
                yield table[][
                    yield tr[][
                        yield th[][a [OnClick(fun _ -> SolveFor None |> dispatch)][str (Domain.Equation.renderEquation m.userEnteredEquation.Value)]]
                        for v in variables do
                            yield th[][a [OnClick(fun _ -> SolveFor(Some v) |> dispatch)][str v]]
                        if not (redundant lhs) then
                            yield th[][str (Domain.Equation.renderElements true lhs)]
                        if not (redundant rhs) then
                            yield th[][str (Domain.Equation.renderElements true rhs)]
                        ]
                    yield! m.entries |> Seq.mapi (fun i entry ->
                        tr[][
                            yield match entry.status with | Correct -> td[ClassName "correct"][str "Correct!"] | Incorrect -> td[ClassName "incorrect"][str "Incorrect"] | Pending -> td[][]
                            for variable in variables do
                                yield td[][
                                            let value = match entry.answers |> Map.tryFind variable with Some v -> v | _ -> ""
                                            yield input [OnChange(fun e -> EntryValue(i, variable, e.Value) |> dispatch); Value value]
                                            yield localInput value [] (fun newValue -> EntryValue(i, variable, newValue) |> dispatch)
                                            ]
                            if not (redundant lhs) then
                                yield td[] (entry.leftOutput |> Option.toList |> List.map str)
                            if not (redundant rhs) then
                                yield td[] (entry.rightOutput |> Option.toList |> List.map str)
                            ]) |> List.ofSeq
                    // If there are no blank rows, also yield some blank rows for more data entry
                    let every f x = x |> Seq.exists f |> not
                    if m.entries |> every (fun e -> e.answers |> every (function KeyValue(_, v) -> not <| System.String.IsNullOrWhiteSpace v)) then
                        yield tr[][
                            yield td[][]
                            for v in variables do
                                yield td[][yield localInput "" [] (fun newValue -> if not (System.String.IsNullOrWhiteSpace newValue) then (EntryValue(m.entries.Length, v, newValue) |> dispatch))]
                            if not (redundant lhs) then
                                yield td[][]
                            if not (redundant rhs) then
                                yield td[][]
                            ]
                    ]
            | _ -> ()
            ]
        div[ClassName "showAnswers"][
            div[][
                if m.activity = DataEntry then
                    yield button[OnClick (fun _ -> dispatch ShowAnswers)][str "Show answers"]
                ]
            ]
        ]
