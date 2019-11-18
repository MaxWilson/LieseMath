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
                    userEnteredEquation = Some e
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
            elif model.entries.[i].answers |> Map.tryFind variable = Some value then // no change, don't do anything to avoid messing up focus
                model
            else
                { model with entries = model.entries |> List.mapi(fun j e -> if i <> j then e else { e with answers = e.answers |> Map.add variable value } |> checkStatus model.formula.Value) }
        model, Cmd.Empty
    | SolveFor variable ->
        match model.formula with
        | None -> model, Cmd.Empty // shouldn't happen
        | Some(variables, eq) ->
            match recheckEntries =
                { model with entries = model.entries |> List.map (fun e -> { e with status = Pending; leftOutput = None; rightOutput = None} |> checkStatus) }
            match variable with
            | None ->
                { model with formula = model.userEnteredEquation |> Option.map(fun e -> variables, e) } |> recheckEntries, Cmd.Empty
            | Some variable ->
                { model with formula = Some(variables, eq |> Domain.solveFor variable)} |> recheckEntries, Cmd.Empty

