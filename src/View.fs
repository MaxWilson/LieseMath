module View

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Fable
open Fable.React
open Fable.React.Props

open Common
open Model
open Model.Enums

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
    | AnswerKey of MathKey
    | Setting of SettingChange
let onClick f x = OnClick <| fun _ -> f x
let btn label attrs = button attrs [str label]

[<Emit("parseInt($0, $1)")>]
let parseInt v radix = jsNative

let viewOptions (settings:Settings) dispatch =
    let setting label currentValue msg options =
        div[][
            text[ClassName "optionLabel"][str label]
            span[ClassName "optionSpan"][
                for (label, value) in options ->
                    button[ClassName (if value = currentValue then "option selected" else "option");
                            OnClick (delay1 dispatch (Setting <| msg value))][str label]
                ]
            ]
    let maxSlider =
        let MAXVAL = 25
        let btnStyle = ClassName "optionButton"
        div[ClassName "options"] [
            text[ClassName "optionLabel"][str "Up to"]
            span[ClassName "optionSpan"][
                button[btnStyle; OnClick (delay1 dispatch (Setting (Maximum <| max 1 (min MAXVAL <| settings.size - 1))))][str "-"]
                input[  ClassName "optionInput"
                        Style [MaxWidth "2em"]
                        OnChange (fun e ->
                                        let radix = (match settings.mathBase with Binary -> 2 | Decimal -> 10 | Hex -> 16)
                                        let v : string = (e.target?value) |> unbox
                                        match parseInt v radix with
                                        | n when n > 0 && n <> settings.size ->
                                                let n = max 1 (min MAXVAL (int n))
                                                dispatch (Setting (Maximum n))
                                                e.preventDefault()
                                        | _ -> ()
                                        )
                        Value (Model.FormatByBase settings.mathBase settings.size)]
                button[btnStyle; OnClick (delay1 dispatch (Setting (Maximum <| max 1 (min MAXVAL <| settings.size + 1))))][str "+"]
                ]
            ]
    div [ClassName "optionsDisplay"] [
        setting "Sound" settings.sound Sound ["On", On; "Off", Off; "Bomb", BombOnly; "Cheers", CheerOnly]
        setting "Auto-ENTER" settings.autoEnter AutoEnter ["On", true; "Off", false]
        setting "Progressive difficulty" settings.progressiveDifficulty ProgressiveDifficulty ["On", true; "Off", false]
        setting "Base" settings.mathBase MathBase ["Binary", Binary; "Decimal", Decimal; "Hexadecimal", Hex]
        setting "Operation" settings.mathType Operation ["+", Plus; "−", Minus; "×", Times; "÷", Divide]
        maxSlider
        button [ClassName "optionDoneButton"; OnClick (delay1 dispatch ToggleOptions)][unbox "OK"]
        ]

let view (g:Game) dispatch =
    let hintTable =
        div[ClassName "hintDisplay"][
            table [ClassName "hintTable"] [
                tbody [] [
                    for row in g.cells ->
                        let className = function
                            | ChromeOnly -> "hintCell chromeOnly"
                            | NeedsReview -> "hintCell needsReview"
                            | Good -> "hintCell correct"
                            | NoAnswer -> "hintCell"
                        tr [] [for (v, status) in row -> td [ClassName <| className status] [str v]]
                    ]
                ]
            ul [ClassName "reviewList"] [
                for r in g.reviewList do
                    yield li [] [str <| sprintf "%s = %s (you guessed %s)" r.problem r.correctAnswer r.guess]
                ]
            ]
    div [ClassName <| if g.showHints || g.showOptions then "ui withHints" else "ui"](
        div[ClassName "header"][
            btn "Reset" [onClick dispatch Reset]
            btn "Options" [onClick dispatch ToggleOptions]
            ]
        ::
        if g.showOptions then
            [viewOptions g.settings dispatch; hintTable]
        else [
            yield h3[ClassName "scoreDisplay"][str <| sprintf "Score: %d" g.score]
            yield div[ClassName "numDisplay"][str (defaultArg g.messageToUser (sprintf "%s = %s" g.problem.question (if g.currentAnswer = "" then String.replicate g.problem.answer.Length "?" else g.currentAnswer)))]
            yield div[ClassName "keyList"][
                let maybeDispatch = if g.messageToUser.IsSome then ignore else dispatch
                for k in keysOf g.settings.mathBase do
                    let keyButton label = btn label [onClick maybeDispatch (AnswerKey k)]
                    if not <| (k = Enter && g.settings.autoEnter) then
                        yield
                            match k with
                            | Number(label) -> keyButton label
                            | Enter -> keyButton "ENTER"
                            | Enums.Backspace -> keyButton (if g.showHints then "Back" else "Backspace")
                            | HintKey -> keyButton "Show hints"
                ]
            if g.showHints then yield hintTable
            ]
        )

#if LEGACY
type Message0 = NoOp
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

#endif
