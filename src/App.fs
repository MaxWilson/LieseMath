module App.View

open Update
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
            //if e.keyCode = 13. then
            //    dispatch (AnswerKey Enter)
            //elif e.keyCode = 8. then
            //    dispatch (AnswerKey Backspace)
            //elif key = "H" then
            //    dispatch (AnswerKey HintKey)
            //elif key = "O" then
            //    dispatch ToggleOptions
            //elif key = "R" && e.ctrlKey then
            //    dispatch Reset
            //e.preventDefault()
            ()
        )
    )
|> Program.withReactBatched "elmish-app"
|> Program.run
