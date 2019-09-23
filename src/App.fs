module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Types
open App.State
open Global

open Fable.Helpers.React
open Fable.Helpers.React.Props

let menuItem label page currentPage =
    li
      [ ]
      [ a
          [ classList [ "is-active", page = currentPage ]
            Href (toHash page) ]
          [ str label ] ]

let menu currentPage =
  aside
    [ ClassName "menu" ]
    [ p
        [ ClassName "menu-label" ]
        [ str "General" ]
      ul
        [ ClassName "menu-list" ]
        [ menuItem "Home" Home currentPage
          menuItem "Counter sample" Counter currentPage
          menuItem "About" Page.About currentPage ] ]

let root1 model dispatch =

  let pageHtml =
    function
    | Page.About -> Info.View.root
    | Counter -> Counter.View.root model.counter (CounterMsg >> dispatch)
    | Home -> Home.View.root model.home (HomeMsg >> dispatch)

  div
    []
    [ div
        [ ClassName "navbar-bg" ]
        [ div
            [ ClassName "container" ]
            [ Navbar.View.root ] ]
      div
        [ ClassName "section" ]
        [ div
            [ ClassName "container" ]
            [ div
                [ ClassName "columns" ]
                [ div
                    [ ClassName "column is-3" ]
                    [ menu model.currentPage ]
                  div
                    [ ClassName "column" ]
                    [ pageHtml model.currentPage ] ] ] ] ]

type Message = Toggle
let init _ = true, Cmd.none
let update msg model =
  match msg with
  | Toggle -> not model, Cmd.none

let root model dispatch =
  div [ClassName "app"] [
    div [ClassName "ui"] [
      div[ClassName "score"] [str "Score: 0"]
      div[ClassName "header"][
        button [][str "Reset"]
        button [OnClick <| fun _ -> dispatch Toggle][str "Options"]
      ]
      div[ClassName "display"][str "3 x 5 = ??"]
      (if model then
        div[ClassName "keypad-decimal"] [
          for x in 1..9 do
            yield button[][str <| x.ToString()]
          yield button[][str "Backspace"]
          yield button[][str "0"]
          yield button[][str "ENTER"]
          yield button[ClassName "hintButton"][str "Show hints"]
        ]
      else
        div[ClassName "keypad-hex"] [
          for x in 1..9 do
            yield button[][str <| x.ToString()]
          for x in 'A'..'F' do
            yield button[][str <| x.ToString()]
          yield button[][str "0"]
          yield button[][str "Backspace"]
          yield button[][str "ENTER"]
          yield button[ClassName "hintButton"][str "Show hints"]
        ])
      ]
    div[ClassName "hints"][
      yield table [ClassName "hintTable"] [
        tbody [] [
          for x in 1..12 do
            yield tr [] [for y in 1..12 -> td [ClassName "hintCell"] [str <| (x*y).ToString()]]
          ]
      ]
      yield ul [ClassName "reviewList"] [
        li [] [str <| sprintf "%s = %s (you guessed %s)" "5x5" "25" "7"]
      ]
    ]
  ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

module Rewrite =
    type Model = string
    type Message = Message of string
    let view model dispatch =
        div [] [
            str model
            ]
    let init _ = "Hello World", Cmd.Empty
    let update msg model =
        match msg with
        | Message m -> m, Cmd.Empty
let view = root
// App
Program.mkProgram init update view
//|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "main"
|> Program.run
