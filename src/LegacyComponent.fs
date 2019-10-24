module LegacyComponent

// Compiles but causes Fable errors, so commmenting out. Exists only for reference.

//open System
//open Fable.Core
//open Fable.Core.JsInterop
//open Fable.Import
//open Fable.Import.Browser
//open Model

//// ReactHelper defines a DSL to make it easier to build
//// React components from F#
//open Fable.React
//open Fable
//open Fable.React.Helpers
//open Fable.React.Props

//type SoundState = On | Off | CheerOnly | BombOnly
//type MathBoxViewState = { showHints: bool; showOptions: bool;  }
//type HintState = unit
//type HintProps = { cells: (string * AnswerState ref) list list }
//type SelectorProps<'a> = { label: string; get: unit -> 'a; set: 'a -> unit; mapping: ('a * string) list }
//type IntegerInputProps = { label: string; min: int; max: int; get: unit -> int; set: int -> unit }

//type HintTable(props: HintProps) =
//    inherit React.Component<HintProps, HintState>(props)
//    override x.render() =
//        let makeCell (cell: string * AnswerState ref) =
//            let needsReview = !(snd cell)
//            td [ClassName (needsReview |> function | NeedsReview -> "hintcell needsreview" | Good -> "hintcell correct" | NoAnswer -> "hintcell" | ChromeOnly -> "hintcell chromeOnly")] [unbox (fst cell)]

//        let rows = props.cells |> List.map (fun rowvals -> tr [] (rowvals |> List.map makeCell))
//        table [ClassName "hinttable"] [tbody [] rows]

//type Selector<'a  when 'a: equality>(props: SelectorProps<'a>) =
//    inherit React.Component<SelectorProps<'a>, unit>(props)
//    override this.render() =
//        let selected = props.get()
//        div [] [
//            text [ClassName "optionLabel"] [unbox props.label]
//            span [ClassName "optionSpan"] (
//                (props.mapping
//                    |> Seq.map (fun (value, label) ->
//                                    button
//                                        [
//                                            OnClick (fun _ -> props.set value; this.forceUpdate())
//                                            ClassName (if value = selected then "option selected" else "option")
//                                            ]
//                                        [unbox label]) |> List.ofSeq)
//                    )
//                ]

//type IntegerInput(props: IntegerInputProps) =
//    inherit React.Component<IntegerInputProps, unit>(props)
//    override this.componentWillMount() =
//        // this is kind of hackish, but we're adjusting min/max whenever settings are displayed
//        if props.get() < props.min then
//            props.set props.min
//        elif props.get() > props.max then
//            props.set props.max
//    override this.render() =
//        div [] [
//            text [ClassName "optionLabel"] [unbox props.label]
//            span [ClassName "optionSpan"]
//                [input [
//                    ClassName "option"
//                    OnChange (fun e ->
//                                  let v : string = (e.target?value) |> unbox
//                                  match JS.parseInt v 10 with
//                                  | x when JS.isNaN (unbox <| box x) -> ()
//                                  | n ->
//                                    let n = int n
//                                    if n < props.min then
//                                        props.set props.min
//                                    elif n > props.max then
//                                        props.set props.max
//                                    else
//                                        props.set (int n)
//                                    e.preventDefault()
//                                  )
//                    DefaultValue (U2.Case1 (props.get().ToString()))
//                    ]]
//            ]

//let nothing = Unchecked.defaultof<ReactElement>
//let inline adapt x = unbox <| box x // suppress compile errors in legacy code

//module Sounds =
//    let sound file =
//        let s = Browser.Dom.HTMLAudioElement.Create()
//        s.src = file
//        s
//    let cheers = [
//        sound("1_person_cheering-Jett_Rifkin-1851518140.mp3")
//        sound("Cheer1.m4a")
//        sound("Cheer2.m4a")
//        sound("Cheer4.m4a")
//        sound("Cheer5.m4a")
//        sound("Cheer6.m4a")
//        ]
//    let bomb = sound("Grenade Explosion-SoundBible.com-2100581469.mp3")
//    let SoundSetting = PersistentSetting("Sound", On)
//open Sounds

//type MathBox() as this =
//    inherit React.Component<unit, MathBoxViewState>()
//    let mutable lastCheer = -1;
//    let onCorrect() =
//        match Sounds.SoundSetting.Value with
//        | On | CheerOnly ->
//            let n = ((JS.Math.random() * 1000.) |> int) % (cheers.Length)
//            // don't want to repeat cheers because that feels funny
//            let n = if lastCheer = n then (n + 1) % cheers.Length else n
//            lastCheer <- n
//            cheers.[n].play()
//        | _ -> ()
//    let onIncorrect() =
//        match Sounds.SoundSetting.Value with
//        | On | BombOnly ->
//            bomb.play()
//        | _ -> ()
//    let prob = MathProblems(adapt onCorrect, adapt onIncorrect)
//    let toggleHints() =
//        this.setState({ this.state with showHints = not this.state.showHints })
//    let toggleOptions() =
//        this.setState({ this.state with showOptions = not this.state.showOptions })
//    do this.state <- { showOptions = false; showHints = false; }
//    // onKeyDown is written partly dynamically because I haven't figured out the right JS types to do it statically
//    let onKeyDown (ev : Browser.Types.Event) =
//        let key : string = unbox ev?key // unbox to type-cast
//        if this.state.showOptions then
//            match key.ToLower() with
//            | "o" -> toggleOptions()
//            | _ -> ()
//        else
//            let x = JS.parseInt key 10
//            if not (JS.Number.isNaN (box x |> unbox)) then
//                prob.KeyPress(int x)
//                this.forceUpdate()
//            else
//            match key.ToLower() with
//            | "a" -> prob.KeyPress(10); this.forceUpdate()
//            | "b" -> prob.KeyPress(11); this.forceUpdate()
//            | "c" -> prob.KeyPress(12); this.forceUpdate()
//            | "d" -> prob.KeyPress(13); this.forceUpdate()
//            | "e" -> prob.KeyPress(14); this.forceUpdate()
//            | "f" -> prob.KeyPress(15); this.forceUpdate()
//            | "enter" ->
//                //prob.Advance()
//                this.forceUpdate()
//                ev.preventDefault()
//            | "backspace" ->
//                prob.Backspace()
//                this.forceUpdate()
//                ev.preventDefault()
//            | "h" ->
//                toggleHints()
//            | "o" -> toggleOptions()
//            | "r" ->
//                if (unbox ev?ctrlKey) then
//                    prob.Reset()
//                    this.forceUpdate()
//                    ev.preventDefault()
//            | _ -> ()
//    member this.componentDidMount() =
//        Browser.Dom.document.addEventListener("keydown", onKeyDown, true)
//    override this.render () =
//        let onClickDo handler =
//            OnClick (fun _ ->
//                        handler()
//                        this.forceUpdate())
//        let keyPadButton label onClick =
//            button [
//                onClickDo onClick
//                ClassName "numkey"
//                ] [unbox label]
//        // there's a shell which holds both the hint box and the interaction keypad
//        div [
//            ClassName "shell columnDisplay"
//            ] (
//            if this.state.showOptions then [
//                div [ClassName "settingsBar"] [
//                    button [ClassName "optionsButton"; OnClick (fun _ -> toggleOptions())] [unbox "Options"]
//                    ]
//                div [ClassName "optionsDisplay"] [
//                    com<Selector<_>, _, _> {
//                            label = "Sound"
//                            get = (fun() -> Sounds.SoundSetting.Value)
//                            set = (fun v -> Sounds.SoundSetting.Value <- v; this.forceUpdate())
//                            mapping = [On, "On"; Off, "Off"; BombOnly, "Bomb"; CheerOnly, "Cheers"; ]
//                        } []
//                    com<Selector<_>, _, _> {
//                            label = "Auto-ENTER"
//                            get = (fun() -> prob.AutoEnter)
//                            set = (fun v -> prob.AutoEnter <- v; this.forceUpdate())
//                            mapping = [true, "On"; false, "Off"]
//                        } []
//                    com<Selector<_>, _, _> {
//                            label = "Progressive difficulty"
//                            get = (fun() -> prob.ProgressiveDifficulty)
//                            set = (fun v -> prob.ProgressiveDifficulty <- v; this.forceUpdate())
//                            mapping = [true, "On"; false, "Off"]
//                        } []
//                    com<Selector<_>, _, _> {
//                            label = "Base"
//                            get = (fun() -> prob.MathBase)
//                            set = (fun v -> prob.MathBase <- v; prob.Reset(); this.forceUpdate())
//                            mapping = [Enums.Binary, "Binary"; Enums.Decimal, "Decimal"; Enums.Hex, "Hexadecimal"]
//                        } []
//                    com<Selector<_>, _, _> {
//                            label = "Operation"
//                            get = (fun() -> prob.MathType)
//                            set = (fun v -> prob.MathType <- v; prob.Reset(); this.forceUpdate())
//                            mapping = Enums.mathTypeMappings
//                        } []
//                    com<IntegerInput, _, _> {
//                            label = "MaxNum"
//                            min = 1
//                            max = 2 * match prob.MathBase with | Enums.Hex -> 16 | Enums.Decimal -> 10 | Enums.Binary -> 2
//                            get = (fun() -> prob.MaxNum)
//                            set = (fun v -> prob.MaxNum <- v; prob.Reset(); this.forceUpdate())
//                        } []
//                    button [ClassName "optionDoneButton"; OnClick (fun _ -> toggleOptions())][unbox "OK"]
//                    ]
//                ]
//            else [
//                div [ClassName "settingsBar"] [
//                    button [ClassName "resetButton"; onClickDo prob.Reset] [unbox "Reset"]
//                    button [ClassName "optionsButton"; OnClick (fun _ -> toggleOptions())] [unbox "Options"]
//                    ]
//                h3 [ClassName "scoreDisplay"] [unbox ("Score: " + prob.Score.ToString())]
//                div [ClassName "shell rowDisplay"] [
//                    div [ClassName "keypad"] [
//                        // Use ReactHelpecom to build a React Component from a type
//                        h2 [ClassName "numDisplay"] [unbox prob.CurrentProblem]
//                        div [ClassName "keyList"] (
//                            prob.Keys |> List.map (function
//                            | Enums.Number(n, label) -> keyPadButton label (fun () -> prob.KeyPress n)
//                            | Enums.Backspace -> keyPadButton "Backspace" prob.Backspace
//                            | Enums.Enter -> keyPadButton "ENTER" (unbox null) // prob.Advance
//                            | Enums.HintKey ->
//                                button [
//                                    ClassName "numkey"
//                                    OnClick (fun _ -> toggleHints())
//                                ] [unbox (if this.state.showHints then "Hide hints" else "Show hints")]
//                            )
//                        )
//                    ]
//                    (if this.state.showHints then
//                        div [ClassName "hintDisplay"] [
//                            com<HintTable, HintProps, HintState> {
//                                cells = (unbox null) // prob.HintCells
//                            } []
//                            // show review list, if any
//                            (if prob.ReviewList.Length > 0 then
//                                ul [ClassName "reviewList"] []
//                                //(
//                                //    prob.ReviewList |> List.map (fun(x, y, prob, ans, given) -> li [] [unbox (sprintf "%s = %s (you guessed %s)" prob ans given)])
//                                //)
//                                else nothing)
//                        ]
//                        else nothing)
//                    ]
//                ])
