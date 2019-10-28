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
    | FeedbackDuration of int
type Message =
    | Reset
    | ToggleOptions
    | AnswerKey of MathKey
    | Setting of SettingChange
    | UserMessage of {| color:string; msg: string |} option
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
        setting "Feedback duration" settings.feedbackDuration FeedbackDuration ["None", 0; "Short", 300; "Medium", 1000; "Long", 5000]
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
            yield
                if g.messageToUser.IsSome then
                    div[ClassName "numDisplay"; Style[Color g.messageToUser.Value.color]][str g.messageToUser.Value.msg]
                else
                    div[ClassName "numDisplay"][str (sprintf "%s = %s" g.problem.question (if g.currentAnswer = "" then String.replicate g.problem.answer.Length "?" else g.currentAnswer))]
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
