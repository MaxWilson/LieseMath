module View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

open Fable.Helpers.React
open Fable.Helpers.React.Props

type Message = Toggle

let view model dispatch =
  div [ClassName "app shell columnDisplay"] [
    div [ClassName "ui"] [
      div[ClassName "header"][
        button [][str "Reset"]
        button [OnClick <| fun _ -> dispatch Toggle][str "Options"]
      ]
      h3[ClassName "scoreDisplay"] [str "Score: 0"]
      div[ClassName "numDisplay"][str "3 x 5 = ??"]
      (if model then
        div[ClassName "keyList"] [
          for x in 1..9 do
            yield button[][str <| x.ToString()]
          yield button[][str "Backspace"]
          yield button[][str "0"]
          yield button[][str "ENTER"]
          yield button[ClassName "hintButton"][str "Show hints"]
        ]
      else
        div[ClassName "keyList"] [
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
    div[ClassName "hintDisplay"][
      yield table [ClassName "hintTable"] [
        tbody [] [
          for x in 1..4 do
            yield tr [] [for y in 1..4 -> td [ClassName "hintCell"] [str <| (x*y).ToString()]]
          ]
      ]
      yield ul [ClassName "reviewList"] [
        li [] [str <| sprintf "%s = %s (you guessed %s)" "5x5" "25" "7"]
      ]
    ]
  ]
