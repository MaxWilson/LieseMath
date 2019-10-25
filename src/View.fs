module View

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Model
open Model.Enums
open Fable
open Fable.React
open Fable.React.Props

type Message0 = NoOp
type SettingChange =
    | Sound of SoundState
    | AutoEnter of bool
    | ProgressiveDifficulty of bool
    | MathBase of MathBase
    | Operation of MathType
    | Maximum of int
type Message =
    | Reset
    | ToggleOptions
    | ToggleHints
    | DataEntry of string
    | ENTER
    | Backspace
    | Setting of SettingChange
let onClick f x = OnClick <| fun _ -> f x
let btn label attrs = button attrs [str label]

let viewOptions (settings:Settings) dispatch =
    let setting label currentValue msg options =
        div[][
            yield text[][str label]
            for (label, value) in options ->
                button[ClassName (if value = currentValue then "option selected" else "option");
                        OnClick (delay1 dispatch (Setting <| msg value))][str label]
            ]
    div [ClassName "optionsDisplay"] [
        setting "Sound" settings.sound Sound ["On", On; "Off", Off; "Bomb", BombOnly; "Cheers", CheerOnly]
        setting "Auto-ENTER" settings.autoEnter AutoEnter ["On", true; "Off", false]
        setting "Progressive difficulty" settings.progressiveDifficulty AutoEnter ["On", true; "Off", false]
        setting "Base" settings.mathBase MathBase ["Binary", Binary; "Decimal", Decimal; "Hexadecimal", Hex]
        setting "Operation" settings.mathType Operation ["+", Plus; "−", Minus; "×", Times; "÷", Divide]
        button [ClassName "optionDoneButton"; OnClick (delay1 dispatch ToggleOptions)][unbox "OK"]
        ]

let view (g:Game) dispatch =
    for row in g.cells do
        for (v, status) in row do
            Browser.Dom.console.log(v)
    div [ClassName "ui"](
        div[ClassName "header"][
            btn "Reset" [onClick dispatch Reset]
            btn "Options" [onClick dispatch ToggleOptions]
            ]
        ::
        if g.showOptions then
            [viewOptions g.settings dispatch]
        else [
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
                        for row in g.cells ->
                            tr [] [for (v, status) in row -> td [ClassName "hintCell"] [str v]]
                        ]
                    ]
                ul [ClassName "reviewList"] [
                    for r in g.reviewList do
                        li [] [str <| sprintf "%s = %s (you guessed %s)" r.problem r.correctAnswer r.guess]
                    ]
                ]
            ]
        )

let view0 (model:Game) dispatch =
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
          for row in model.cells do
            tr [] [for (label, answerState) in row -> td [ClassName "hintCell"] [str label]]
          ]
      ]
      yield ul [ClassName "reviewList"] [
        li [] [str <| sprintf "%s = %s (you guessed %s)" "5x5" "25" "7"]
      ]
    ]
  ]

