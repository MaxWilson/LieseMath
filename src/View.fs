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

[<Emit("parseInt($0, $1)")>]
let parseInt v radix = jsNative

let view (m:Model.Model) dispatch =
    div [ClassName "ui"][
        div[ClassName "modeSelection"] [
            label[][
                input[Type "radio"; Name "mode"]
                str "Homework"
                ]
            label[][
                input[Type "radio"; Name "mode"]
                str "Game mode"
                ]
            ]
        div[ClassName "help"][
            a[OnClick ignore][str "Help"]
            ]
        div[ClassName "equationEntry"][
            form[] [
                input[Placeholder "Enter an equation, e.g. 2y = -x + 15"]
                button[Type "submit"][str "OK"]
                ]
            ]
        div[ClassName "tableEntry"][
            table[][
                tr[][
                    th[][str "x"]
                    th[][str "y"]
                    th[][str "2y"]
                    th[][str "-x + 15"]
                    ]
                tr[][
                    td[][input [Value "3"]]
                    td[][]
                    td[][]
                    td[][]
                    ]
                tr[][
                    td[][input [Value "3"]]
                    td[][input [Value "4"]]
                    td[][str "8"]
                    td[][str "12"]
                    ]
                ]
            ]
        div[ClassName "showAnswers"][
            div[][
                button[][str "Show answers"]
                ]
            ]
        ]
