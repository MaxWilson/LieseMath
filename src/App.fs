module App.View

open Update
open Model.Enums
open View

open Elmish
open Elmish.Debug
open Elmish.HMR

// App
Program.mkProgram init update view
//|> Program.toNavigable (parseHash pageParser) urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withSubscription(fun model ->
    Cmd.ofSub(fun dispatch ->
        Browser.Dom.document.onkeydown <- fun e ->
            let key = e.key.ToUpperInvariant()
            if Model.Enums.keysOf Model.Enums.Hex |> Array.exists(function Number n when n = key -> true | _ -> false) then
                dispatch (AnswerKey (Number key))
            elif e.keyCode = 13. then
                dispatch (AnswerKey Enter)
            elif e.keyCode = 8. then
                dispatch (AnswerKey Backspace)
            elif key = "H" then
                dispatch (AnswerKey HintKey)
            elif key = "O" then
                dispatch ToggleOptions
        )
    )
|> Program.withReactBatched "main"
|> Program.run
