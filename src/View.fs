module View

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Fable
open Fable.React
open Fable.React.Props

open Common
open ViewElement
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

let ofEnum x = x |> List.map (fun v -> v, v.ToString())
let ofBool = [true, "On"; false, "Off"]

// minor perf opt: pre-curry all the settings. Not necessary, just seeing if it can be done elegantly.
let adapt (label, currentValueGetter, modelCommand, options) model d = ViewElement.setting label options (currentValueGetter model) (fun v -> d (modelCommand v |> Setting))
let changeTo wrap (ChangeValue v) = wrap v
let settingsControls = [
    // hey, this getter/setter pattern kind of reminds me of lenses!
    ("Auto-ENTER", (fun s -> s.autoEnter), changeTo AutoEnter, ofBool) |> adapt
    ("Sound", (fun (s:Settings) -> s.sound), changeTo Sound, [On, "On"; Off, "Off"; BombOnly, "Bomb"; CheerOnly, "Cheers"]) |> adapt
    ("Feedback duration", (fun s -> s.feedbackDuration), changeTo FeedbackDuration, [0, "None"; 300, "Short"; 1000, "Medium"; 5000, "Long"]) |> adapt
    ("Progressive difficulty", (fun s -> s.progressiveDifficulty), changeTo ProgressiveDifficulty, ofBool) |> adapt
    ("Base", (fun s -> s.mathBase), changeTo MathBase, ofEnum [Binary; Decimal; Hex]) |> adapt
    ("Operation", (fun s -> s.mathType), changeTo Operation, [Plus, "+"; Minus, "−"; Times, "×"; Divide, "÷" ]) |> adapt
    ]

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
        for setting in settingsControls do
            yield setting settings dispatch
        yield maxSlider
        yield button [ClassName "optionDoneButton"; OnClick (delay1 dispatch ToggleOptions)][unbox "OK"]
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
                            | Enums.Backspace -> keyButton "Back"
                            | HintKey -> keyButton "Show hints"
                ]
            if g.showHints then yield hintTable
            ]
        )
