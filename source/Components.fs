module Components

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Models
open System.Collections.Generic

// ReactHelper defines a DSL to make it easier to build
// React components from F#
module R = Fable.Helpers.React
open R.Props
open Fable.Import.React

type SoundState = On | Off | CheerOnly | BombOnly
type MathBoxViewState = { showHints: bool; showOptions: bool;  }
type HintState = unit
type HintProps = { cells: (string * AnswerState ref) list list }
type SelectorProps<'a> = { label: string; get: unit -> 'a; set: 'a -> unit; mapping: ('a * string) list }
type IntegerInputProps = { label: string; min: int; max: int; get: unit -> int; set: int -> unit }

type HintTable(props: HintProps) =
    inherit React.Component<HintProps, HintState>()
    member x.render() =
        let makeCell (cell: string * AnswerState ref) =
            let needsReview = !(snd cell)
            R.td [ClassName (needsReview |> function | NeedsReview -> "hintcell needsreview" | Good -> "hintcell correct" | NoAnswer -> "hintcell" | ChromeOnly -> "hintcell chromeOnly")] [unbox (fst cell)]

        let rows = props.cells |> List.map (fun rowvals -> R.tr [] (rowvals |> List.map makeCell))
        R.table [ClassName "hinttable"] [R.tbody [] rows]

type Selector<'a  when 'a: equality>(props: SelectorProps<'a>) =
    inherit React.Component<SelectorProps<'a>, unit>()
    member this.render() =
        let selected = props.get()
        R.div [] [
            R.text [ClassName "optionLabel"] [unbox props.label]
            R.span [ClassName "optionSpan"] (
                (props.mapping
                    |> Seq.map (fun (value, label) ->
                                    R.button
                                        [
                                            OnClick (fun _ -> props.set value; this.forceUpdate())
                                            ClassName (if value = selected then "option selected" else "option")
                                            ]
                                        [unbox label]) |> List.ofSeq)
                    )
                ]

type IntegerInput(props: IntegerInputProps) =
    inherit React.Component<IntegerInputProps, unit>()
    member this.componentWillMount() =
        // this is kind of hackish, but we're adjusting min/max whenever settings are displayed
        if props.get() < props.min then
            props.set props.min
        elif props.get() > props.max then
            props.set props.max
    member this.render() =
        R.div [] [
            R.text [ClassName "optionLabel"] [unbox props.label]
            R.span [ClassName "optionSpan"]
                [R.input [
                    ClassName "option"
                    OnChange (fun e ->
                                  let v : string = (e.target?value) |> unbox
                                  match JS.Number.parseInt v with
                                  | x when JS.isNaN x -> ()
                                  | n ->
                                    let n = int n
                                    if n < props.min then
                                        props.set props.min
                                    elif n > props.max then
                                        props.set props.max
                                    else
                                        props.set (int n)
                                    e.preventDefault()
                                  )
                    DefaultValue (U2.Case1 (props.get().ToString()))
                    ] []]
            ]

let nothing = Unchecked.defaultof<ReactElement<obj>>

module Sounds =

    let cheers = [
        Audio.Create("1_person_cheering-Jett_Rifkin-1851518140.mp3")
        Audio.Create("Cheer1.m4a")
        Audio.Create("Cheer2.m4a")
        Audio.Create("Cheer4.m4a")
        Audio.Create("Cheer5.m4a")
        Audio.Create("Cheer6.m4a")
        ]
    let bomb = Audio.Create("Grenade Explosion-SoundBible.com-2100581469.mp3")
    let SoundSetting = PersistentSetting("Sound", On)
open Sounds

type MathBox() as this =
    inherit React.Component<unit, MathBoxViewState>()
    let mutable lastCheer = -1;
    let onCorrect() =
        match Sounds.SoundSetting.Value with
        | On | CheerOnly ->
            let n = ((JS.Math.random() * 1000.) |> int) % (cheers.Length)
            // don't want to repeat cheers because that feels funny
            let n = if lastCheer = n then (n + 1) % cheers.Length else n
            lastCheer <- n
            cheers.[n].play()
        | _ -> ()
    let onIncorrect() =
        match Sounds.SoundSetting.Value with
        | On | BombOnly ->
            bomb.play()
        | _ -> ()
    let prob = MathProblems(onCorrect, onIncorrect)
    let toggleHints() =
        this.setState({ this.state with showHints = not this.state.showHints })
    let toggleOptions() =
        this.setState({ this.state with showOptions = not this.state.showOptions })
    do this.state <- { showOptions = false; showHints = false; }
    // onKeyDown is written partly dynamically because I haven't figured out the right JS types to do it statically
    let onKeyDown (ev : Event) =
        let key : string = unbox ev?key // unbox to type-cast
        if this.state.showOptions then
            match key.ToLower() with
            | "o" -> toggleOptions()
            | _ -> ()
        else
            let x = JS.Number.parseInt key
            if not (JS.Number.isNaN x) then
                prob.KeyPress(int x)
                this.forceUpdate()
            else
            match key.ToLower() with
            | "a" -> prob.KeyPress(10); this.forceUpdate()
            | "b" -> prob.KeyPress(11); this.forceUpdate()
            | "c" -> prob.KeyPress(12); this.forceUpdate()
            | "d" -> prob.KeyPress(13); this.forceUpdate()
            | "e" -> prob.KeyPress(14); this.forceUpdate()
            | "f" -> prob.KeyPress(15); this.forceUpdate()
            | "enter" ->
                prob.Advance()
                this.forceUpdate()
                ev.preventDefault()
            | "backspace" ->
                prob.Backspace()
                this.forceUpdate()
                ev.preventDefault()
            | "h" ->
                toggleHints()
            | "o" -> toggleOptions()
            | "r" ->
                if (unbox ev?ctrlKey) then
                    prob.Reset()
                    this.forceUpdate()
                    ev.preventDefault()
            | _ -> ()
    member this.componentDidMount() =
        Browser.document.addEventListener("keydown", EventListenerOrEventListenerObject.Case1(EventListener onKeyDown), true)
    member this.render () =
        let onClickDo handler =
            OnClick (fun _ ->
                        handler()
                        this.forceUpdate())
        let keyPadButton label onClick =
            R.button [
                onClickDo onClick
                ClassName "numkey"
                ] [unbox label]
        // there's a shell which holds both the hint box and the interaction keypad
        R.div [
            ClassName "shell columnDisplay"
            ] (
            if this.state.showOptions then [
                R.div [ClassName "settingsBar"] [
                    R.button [ClassName "optionsButton"; OnClick (fun _ -> toggleOptions())] [unbox "Options"]
                    ]
                R.div [ClassName "optionsDisplay"] [
                    R.com<Selector<_>, _, _> {
                            label = "Sound"
                            get = (fun() -> Sounds.SoundSetting.Value)
                            set = (fun v -> Sounds.SoundSetting.Value <- v; this.forceUpdate())
                            mapping = [On, "On"; Off, "Off"; BombOnly, "Bomb"; CheerOnly, "Cheers"; ]
                        } []
                    R.com<Selector<_>, _, _> {
                            label = "Auto-ENTER"
                            get = (fun() -> prob.AutoEnter)
                            set = (fun v -> prob.AutoEnter <- v; this.forceUpdate())
                            mapping = [true, "On"; false, "Off"]
                        } []
                    R.com<Selector<_>, _, _> {
                            label = "Progressive difficulty"
                            get = (fun() -> prob.ProgressiveDifficulty)
                            set = (fun v -> prob.ProgressiveDifficulty <- v; this.forceUpdate())
                            mapping = [true, "On"; false, "Off"]
                        } []
                    R.com<Selector<_>, _, _> {
                            label = "Base"
                            get = (fun() -> prob.MathBase)
                            set = (fun v -> prob.MathBase <- v; prob.Reset(); this.forceUpdate())
                            mapping = [Enums.Binary, "Binary"; Enums.Decimal, "Decimal"; Enums.Hex, "Hexadecimal"]
                        } []
                    R.com<Selector<_>, _, _> {
                            label = "Operation"
                            get = (fun() -> prob.MathType)
                            set = (fun v -> prob.MathType <- v; prob.Reset(); this.forceUpdate())
                            mapping = Enums.mathTypeMappings
                        } []
                    R.com<IntegerInput, _, _> {
                            label = "MaxNum"
                            min = 1
                            max = 2 * match prob.MathBase with | Enums.Hex -> 16 | Enums.Decimal -> 10 | Enums.Binary -> 2
                            get = (fun() -> prob.MaxNum)
                            set = (fun v -> prob.MaxNum <- v; prob.Reset(); this.forceUpdate())
                        } []
                    R.button [ClassName "optionDoneButton"; OnClick (fun _ -> toggleOptions())][unbox "OK"]
                    ]
                ]
            else [
                R.div [ClassName "settingsBar"] [
                    R.button [ClassName "resetButton"; onClickDo prob.Reset] [unbox "Reset"]
                    R.button [ClassName "optionsButton"; OnClick (fun _ -> toggleOptions())] [unbox "Options"]
                    ]
                R.h3 [ClassName "scoreDisplay"] [unbox ("Score: " + prob.Score.ToString())]
                R.div [ClassName "shell rowDisplay"] [
                    R.div [ClassName "keypad"] [
                        // Use ReactHelper.com to build a React Component from a type
                        R.h2 [ClassName "numDisplay"] [unbox prob.CurrentProblem]
                        R.div [ClassName "keyList"] (
                            prob.Keys |> List.map (function
                            | Enums.Number(n, label) -> keyPadButton label (fun () -> prob.KeyPress n)
                            | Enums.Backspace -> keyPadButton "Backspace" prob.Backspace
                            | Enums.Enter -> keyPadButton "ENTER" prob.Advance
                            | Enums.HintKey ->
                                R.button [
                                    ClassName "numkey"
                                    OnClick (fun _ -> toggleHints())
                                ] [unbox (if this.state.showHints then "Hide hints" else "Show hints")]
                            )
                        )
                    ]
                    (if this.state.showHints then
                        R.div [ClassName "hintDisplay"] [
                            R.com<HintTable, HintProps, HintState> {
                                cells = prob.HintCells
                            } []
                            // show review list, if any
                            (if prob.ReviewList.Length > 0 then
                                R.ul [ClassName "reviewList"] (
                                    prob.ReviewList |> List.map (fun(x, y, prob, ans, given) -> R.li [] [unbox (sprintf "%s = %s (you guessed %s)" prob ans given)])
                                )
                                else nothing)
                        ]
                        else nothing)
                    ]
                ])
