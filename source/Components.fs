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

type CBState = { data: string }

type HelloBox() as this =
    inherit React.Component<unit, CBState>()
    let prob = MathProblems()
    do this.state <- { data = prob.CurrentProblem }
    
    member x.handleSubmit (e: FormEvent) =
        let msg  = x.state.data
        e.preventDefault()

    member x.componentDidMount () = ()

    member x.render () =        
        let numKey (n: int) =
            R.input [
                    Type "submit"
                    Value (U2.Case1 (string n))
                    OnClick (fun _ ->
                                prob.Advance()
                                x.setState { data = prob.CurrentProblem })
                ] []
        R.div [ClassName "commentBox"] [
            // Use ReactHelper.com to build a React Component from a type
            R.h2 [] [unbox x.state.data]
            numKey 1
            numKey 2
            numKey 3
            numKey 4
            numKey 5
            numKey 6
            numKey 7
            numKey 8
            numKey 9
            numKey 0
        ]
