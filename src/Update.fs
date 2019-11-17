module Update

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Common
open Model
open View

let init _ = Model.fresh, Cmd.none

[<Emit("setTimeout($1, $0)")>]
let setTimeout ms callback = jsNative

let update msg model = model, Cmd.Empty
