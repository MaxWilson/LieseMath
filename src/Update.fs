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

let recheckEntries (model: Model) =
    match model.formula with
    | Some(formula) ->
        { model with entries = model.entries |> List.map (fun e -> { e with status = Pending; leftOutput = None; rightOutput = None } |> checkStatus formula) }
    | None -> model

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
            let equation =
                match variable with
                | None -> model.userEnteredEquation.Value
                | Some variable ->
                    eq |> Domain.solveFor variable
            { model with formula = Some(variables, equation); rawFormula = (renderEquation equation) } |> recheckEntries, Cmd.Empty
    | ShowAnswers ->
        match model.formula with
        | Some(variables, eq) ->
            let solveEntry (entry: Entry) =
                let fill (answers: Map<_, _>) =
                    variables |> Array.fold (fun acc variableName ->
                                                match acc |> Map.tryFind variableName with
                                                | Some txt when not (System.String.IsNullOrWhiteSpace txt) -> acc
                                                | _ -> acc |> Map.add variableName "0"
                                    ) answers
                let missing variableName =
                    match (entry.answers |> Map.tryFind variableName) with
                    | Some answer when System.String.IsNullOrWhiteSpace(answer) -> Some variableName
                    | None -> Some variableName
                    | Some answer -> None
                let solve missingVariable =
                    let answers = entry.answers |> fill
                    let eval v =
                        match answers |> Map.tryFind v with
                        | Some txt ->
                            match Parse.tryParseNumber txt with
                            | Some n -> n
                            | None -> Number(0, None)
                        | None -> Number(0, None)
                    let (Equation(_, solved) as e) = solveFor missingVariable eq
                    let v = evaluateElements eval solved |> renderNumber
                    let answers = answers |> Map.add missingVariable v
                    { entry with answers = answers }

                match variables |> Seq.tryPick missing with
                | Some missingVariable -> solve missingVariable
                | None when entry.status <> Correct && variables.Length > 0 -> solve (variables |> Array.last)
                | None -> entry
            { model with entries = model.entries |> List.map solveEntry } |> recheckEntries |> summarize, Cmd.Empty
        | None -> model, Cmd.Empty


