module Update

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

open Model
open View

let init _ = Game.Fresh(), Cmd.none
let update msg model =
  match msg with
  | NoOp -> model, Cmd.none
