module Update

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Common
open Domain
open Domain.Equation
open Model
open View

let init _ = Model.fresh, Cmd.none

[<Emit("setTimeout($1, $0)")>]
let setTimeout ms callback = jsNative

module Cmd =
    let delayMsg delayMs msg = Cmd.ofSub(fun d -> setTimeout delayMs (fun _ -> d msg))

let update msg model =
    match msg with
    | RawFormula txt -> { model with rawFormula = txt }, Cmd.Empty
    | Formula txt ->
        match Domain.Parse.tryParse(txt) with
        | Some(Equation(lhs, rhs) as e) ->
            let extractVariable set = function
                | Variable(v, _) -> set |> Set.add v
                | _ -> set
            let variables = (lhs@rhs) |> List.fold (fun st input -> extractVariable st input) Set.empty
            { model
                with
                    activity = Activity.DataEntry;
                    rawFormula = (renderEquation e);
                    formula = Some (Set.toArray variables, e)
                    entries = []
                    }, Cmd.Empty
        | _ -> { model with error = Some (sprintf "I don't understand '%s'" txt) }, Cmd.delayMsg 1000 (Error None)
    | Error err -> { model with error = err }, Cmd.Empty
    | Activity a -> { model with activity = a }, Cmd.Empty
    | Mode m -> { model with mode = m }, Cmd.Empty
    | EntryValue(i, variable, value) ->
        let model =
            if i >= model.entries.Length then
                { model with entries = model.entries @ [ freshEntry variable value ] }
            else
                let checkStatus (e: Entry) =
                    if e.answers.Count = (model.formula.Value |> fst |> Seq.length) then
                        let answers = e.answers |> Map.map (fun variable txt -> Domain.Parse.tryParseNumber txt)
                        if answers |> Map.exists (fun _ v -> Option.isNone v) then
                            // not ready yet
                            e
                        else
                            let (Equation(lhs, rhs)) = model.formula.Value |> snd
                            let left = evaluateElements (fun v -> answers.[v].Value) lhs |> renderNumber |> Some
                            let right = evaluateElements (fun v -> answers.[v].Value) rhs |> renderNumber |> Some
                            { e with leftOutput = left; rightOutput = right; status = if left = right then Correct else Incorrect }
                    else e
                { model with entries = model.entries |> List.mapi(fun j e -> if i <> j then e else { e with answers = e.answers |> Map.add variable value } |> checkStatus) }
        model, Cmd.Empty
