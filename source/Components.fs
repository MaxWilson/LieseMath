module Components

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Models

// ReactHelper defines a DSL to make it easier to build
// React components from F#
module R = Fable.Helpers.React
open R.Props
open Fable.Import.React

module External =
    [<Emit("document.body.addEventListener('keydown', $0, true)")>]
    let configureOnKeydown ev = failwith "JS only"

type MathBoxViewState = { showOptions: bool; showHints: bool; }
type HintState = unit
type HintProps = { cells: (string * AnswerState ref) list list }

type HintTable(props: HintProps) =
    inherit React.Component<HintProps, HintState>()
    member x.render() =
        let makeCell (cell: string * AnswerState ref) =
            let needsReview = !(snd cell)
            R.td [ClassName (needsReview |> function | NeedsReview -> "hintcell needsreview" | Good -> "hintcell correct" | NoAnswer -> "hintcell")] [unbox (fst cell)]

        let rows = props.cells |> List.map (fun rowvals -> R.tr [] (rowvals |> List.map makeCell))
        R.table [ClassName "hinttable"] [R.tbody [] rows]

let nothing = Unchecked.defaultof<ReactElement<obj>>

type MathBox() as this =
    inherit React.Component<unit, MathBoxViewState>()
    [<Emit("let cheer = new Audio('1_person_cheering-Jett_Rifkin-1851518140.mp3'); cheer.play()")>]
    let cheer() = failwith "JS only";
    [<Emit("let cheer1 = new Audio('Cheer1.m4a'); cheer1.currentTime = 0.3; cheer1.play()")>]
    let cheer1() = failwith "JS only";
    [<Emit("let cheer2 = new Audio('Cheer2.m4a'); cheer2.currentTime = 1; cheer2.play()")>]
    let cheer2() = failwith "JS only";
    [<Emit("let cheer4 = new Audio('Cheer4.m4a'); cheer4.currentTime = 0.5; cheer4.play()")>]
    let cheer4() = failwith "JS only";
    [<Emit("let cheer5 = new Audio('Cheer5.m4a'); cheer5.currentTime = 0.5; cheer5.play()")>]
    let cheer5() = failwith "JS only";
    [<Emit("let cheer6 = new Audio('Cheer6.m4a'); cheer6.play()")>]
    let cheer6() = failwith "JS only";
    [<Emit("let bomb = new Audio('Grenade Explosion-SoundBible.com-2100581469.mp3'); bomb.play()")>]
    let bomb() = failwith "JS only";
    let cheers = [|cheer; cheer1; cheer2; cheer4; cheer5; cheer6|];
    let mutable lastCheer = -1;
    let randomCheer() =
        let n = ((JS.Math.random() * 1000.) |> int) % (cheers.Length)
        // don't want to repeat cheers because that feels funny
        let n = if lastCheer = n then (n + 1) % cheers.Length else n
        lastCheer <- n
        (cheers.[n])()
    let prob = MathProblems(randomCheer, bomb)
    let updateState() = this.setState this.state
    do this.state <- { showOptions = false; showHints = false }
    // written dynamically because I haven't figured out the right JS types
    let toggleHints() =
        this.setState({ this.state with showHints = not this.state.showHints })
    let toggleOptions() =
        this.setState({ this.state with showOptions = not this.state.showOptions })
    let onKeyDown (ev : obj) =
        let key : string = unbox ev?key // unbox to type-cast
        let x = JS.Number.parseInt key
        if not (JS.Number.isNaN x) then
            prob.KeyPress(int x)
            updateState()
        else
        match key.ToLower() with
        | "a" -> prob.KeyPress(10); updateState()
        | "b" -> prob.KeyPress(11); updateState()
        | "c" -> prob.KeyPress(12); updateState()
        | "d" -> prob.KeyPress(13); updateState()
        | "e" -> prob.KeyPress(14); updateState()
        | "f" -> prob.KeyPress(15); updateState()
        | "enter" ->
            prob.Advance()
            updateState()
            ev?preventDefault() |> ignore
        | "backspace" ->
            prob.Backspace()
            updateState()
            ev?preventDefault() |> ignore
        | "h" ->
            toggleHints()
        | "o" -> toggleOptions()
        | "r" ->
            if (unbox ev?ctrlKey) then
                prob.Reset()
                updateState()
                ev?preventDefault() |> ignore
        | _ -> ()
    member this.componentDidMount() =
        External.configureOnKeydown onKeyDown
    member this.render () =
        let onClickDo handler =
            OnClick (fun _ ->
                        handler()
                        updateState())
        let keyPadButton label onClick =
            R.button [
                    onClickDo onClick
                    ClassName "numkey"
            ] [unbox label]
        // there's a shell which holds both the hint box and the interaction keypad
        R.div [
            ClassName "shell columnDisplay"
        ] (
        if this.state.showOptions then
            [
                R.div [ClassName "optionsDisplay"] [
                    R.input [Placeholder "12"; ClassName "optionInput"][]
                    R.button [ClassName "optionDoneButton"; onClickDo toggleOptions][unbox "OK"]
                ]
            ]
        else
            [
            R.div [ClassName "settingsBar"] [
                R.button [ClassName "resetButton"; onClickDo prob.Reset] [unbox "Reset"]
                R.button [ClassName "optionsButton"; onClickDo toggleOptions] [unbox "Options"]
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
                                prob.ReviewList |> List.map (fun(x, y, ans, given) -> R.li [] [unbox (sprintf "%d x %d = %s (you guessed %s)" x y ans given)])
                            )
                            else nothing)
                    ]
                    else nothing)
            ]
        ])
