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

let update msg model =
    match msg with
    | RawFormula txt -> { model with rawFormula = txt }, Cmd.Empty
    | ChangeFormula txt ->
        match Domain.Parse.tryParse(txt) with
        | Some(Equation(lhs, rhs) as e) ->
            let extractVariable set = function
                | Variable(v, _) -> set |> Set.add v
                | _ -> set
            let variables = (lhs@rhs) |> List.fold (fun st input -> extractVariable st input) Set.empty
            { model with rawFormula = txt; formula = Some (Set.toArray variables, e) }, Cmd.Empty
        | _ -> model, Cmd.ofSub(fun d ->
                Error(Some (sprintf "I don't understand '%s'" txt)) |> d
                setTimeout 1000 (fun _ -> Error None |> d)
                )
    | Error err -> { model with error = err }, Cmd.Empty
    | _ -> model, Cmd.Empty
