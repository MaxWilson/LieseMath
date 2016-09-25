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

type MathBoxState = { showHints: bool }
type HintState = unit
type HintProps = { cells: (int * AnswerState ref) list list }

type HintTable(props: HintProps) =
    inherit React.Component<HintProps, HintState>()
    member x.render() =
        let makeCell (cell: int * AnswerState ref) =
            let needsReview = !(snd cell)
            R.td [ClassName (needsReview |> function | NeedsReview -> "hintcell needsreview" | Good -> "hintcell correct" | NoAnswer -> "hintcell")] [unbox (fst cell)]

        let rows = props.cells |> List.map (fun rowvals -> R.tr [] (rowvals |> List.map makeCell))
        R.table [ClassName "hinttable"] [R.tbody [] rows]

type MathBox() as this =
    inherit React.Component<unit, MathBoxState>()
    let prob = MathProblems(12)
    let updateState() = this.setState this.state
    do this.state <- { showHints = false }
    // written dynamically because I haven't figured out the right JS types
    let toggleHints() =
        this.setState({ this.state with showHints = not this.state.showHints })
    let onKeyDown (ev : obj) =
        let key : string = unbox ev?key // unbox to type-cast
        let x = JS.Number.parseInt key
        if not (JS.Number.isNaN x) then
            prob.KeyPress(int x)
            updateState()
        else if key = "Enter" then
            prob.Advance()
            updateState()
            ev?preventDefault() |> ignore
        else if key = "Backspace" then
            prob.Backspace()
            updateState()
            ev?preventDefault() |> ignore
        else if key = "h" || key = "H" then
            toggleHints()
        else if ((key = "R" || key = "r") && (unbox ev?ctrlKey)) then
            prob.Reset()
            updateState()
            ev?preventDefault() |> ignore
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
        let numKey (n: int) = keyPadButton (n.ToString()) (fun() -> prob.KeyPress n)
        // there's a shell which holds both the hint box and the interaction keypad
        R.div [
            ClassName "shell columnDisplay"
        ] [
            R.div [ClassName "settingsBar"] [
                R.button [ClassName "resetButton"; onClickDo prob.Reset] [unbox "Reset"]
            ]
            R.h3 [ClassName "scoreDisplay"] [unbox ("Score: " + prob.Score.ToString())]
            R.div [ClassName "shell rowDisplay"] [
                R.div [ClassName "keypad"] [
                    // Use ReactHelper.com to build a React Component from a type
                    R.h2 [ClassName "numDisplay"] [unbox prob.CurrentProblem]
                    R.div [ClassName "keyList"] [
                        numKey 1
                        numKey 2
                        numKey 3
                        numKey 4
                        numKey 5
                        numKey 6
                        numKey 7
                        numKey 8
                        numKey 9
                        keyPadButton "Backspace" prob.Backspace
                        numKey 0
                        keyPadButton "ENTER" prob.Advance
                        R.button [
                            ClassName "numkey"
                            OnClick (fun _ -> toggleHints())
                        ] [unbox (if this.state.showHints then "Hide hints" else "Show hints")]
                    ]
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
                            else
                                Unchecked.defaultof<ReactElement<obj>>)
                    ]
                 else
                    Unchecked.defaultof<ReactElement<obj>>)
            ]
        ]
