// place for reusable View abstractions
module ViewElement

open Common
open Fable.React
open Fable.React.Props

type ChangeValue<'t> = ChangeValue of 't

let setting label options currentValue dispatch =
    div[][
        text[ClassName "optionLabel"][str label]
        span[ClassName "optionSpan"][
            for (value, label) in options ->
                button[ClassName (if value = currentValue then "option selected" else "option");
                        OnClick (delay1 dispatch (ChangeValue value))][str label]
            ]
        ]
