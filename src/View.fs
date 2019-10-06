module View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Model
open Model.Enums

type Message = NoOp
type Message2 =
    | Reset
    | ToggleOptions
    | ToggleHints
    | DataEntry of string
    | ENTER
    | Backspace
let onClick f x = OnClick <| fun _ -> f x
let btn label attrs = button attrs [str label]

let view2 (g:Game) dispatch =
    div [ClassName "ui"][
        div[ClassName "header"][
            btn "Reset" [onClick dispatch Reset]
            btn "Options" [onClick dispatch ToggleOptions]
            ]
        h3[ClassName "scoreDisplay"][str <| sprintf "Score: %d" g.score]
        div[ClassName "numDisplay"][str (defaultArg g.messageToUser g.problem.question)]
        div[ClassName "keyList"][
            let maybeDispatch = if g.messageToUser.IsSome then ignore else dispatch
            for k in keysOf g.settings.mathBase do
                match k with
                | Number(num, label) -> btn label [onClick maybeDispatch (DataEntry label)]
                | Enter -> btn "ENTER" [onClick maybeDispatch ENTER]
                | Enums.Backspace -> btn "Backspace" [onClick maybeDispatch Backspace]
                | HintKey -> btn "Show hints" [onClick maybeDispatch ToggleHints]
            ]
        div[ClassName "hintDisplay"][
            table [ClassName "hintTable"] [
                tbody [] [
                    for row in g.cells do
                        tr [] [for (v, status) in row -> td [ClassName "hintCell"] [str v]]
                    ]
                ]
            ul [ClassName "reviewList"] [
                for r in g.reviewList do
                    li [] [str <| sprintf "%s = %s (you guessed %s)" r.problem r.correctAnswer r.guess]
                ]
            ]
        ]

let view (model:Game) dispatch =
  div [ClassName "app shell columnDisplay"] [
    div [ClassName "ui"] [
      div[ClassName "header"][
        button [][str "Reset"]
        button [OnClick <| fun _ -> dispatch NoOp][str "Options"]
      ]
      h3[ClassName "scoreDisplay"] [str "Score: 0"]
      div[ClassName "numDisplay"][str "3 x 5 = ??"]
      (if model.settings.mathBase = Enums.Decimal then
          div[ClassName "keyList"] [
            for x in 1..9 do
              button[][str <| x.ToString()]
            button[][str "Backspace"]
            button[][str "0"]
            button[][str "ENTER"]
            button[ClassName "hintButton"][str "Show hints"]
          ]
      elif model.settings.mathBase = Enums.Binary then
        div[ClassName "keyList"] [
          for x in 1..1 do
            button[][str <| x.ToString()]
          button[][str "Backspace"]
          button[][str "0"]
          button[][str "ENTER"]
          button[ClassName "hintButton"][str "Show hints"]
        ]
      else
        div[ClassName "keyList"] [
          for x in 1..9 do
            button[][str <| x.ToString()]
          for x in 'A'..'F' do
            button[][str <| x.ToString()]
          button[][str "0"]
          button[][str "Backspace"]
          button[][str "ENTER"]
          button[ClassName "hintButton"][str "Show hints"]
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
