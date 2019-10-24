module Update

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Model
open View

let init _ = Game.Fresh(), Cmd.none
let update msg model =
  match msg with
  | ToggleOptions -> { model with showOptions = not model.showOptions }, Cmd.none
  | Reset -> Game.Fresh(), Cmd.none
  | _ -> model, Cmd.none
