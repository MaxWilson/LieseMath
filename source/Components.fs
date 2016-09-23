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
    let onKeyDown (ev : obj) = 
        let key : string = ev?key :> obj :?> string // double cast to work around Fable issue
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
    member this.componentDidMount() =
        External.configureOnKeydown onKeyDown
    member this.render () = 
        let keyPadButton label onClick =
            R.button [
                    OnClick (fun _ ->
                                onClick()
                                updateState())
                    ClassName "numkey"
                ] [unbox label]
        let numKey (n: int) = keyPadButton (n.ToString()) (fun() -> prob.KeyPress n)
        // there's a shell which holds both the hint box and the interaction keypad
        R.div [
            ClassName "shell columnDisplay"
        ] [
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
                            OnClick (fun _ ->
                                      this.setState({ this.state with showHints = not this.state.showHints })
                            )
                        ] [unbox (if this.state.showHints then "Hide hints" else "Show hints")]
                    ]
                ]
                (if this.state.showHints then 
                    R.com<HintTable, HintProps, HintState> { 
                        cells = prob.HintCells
                    } []                
                 else
                    Unchecked.defaultof<ReactElement<obj>>)
            ]
        ]
