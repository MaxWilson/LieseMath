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
|> Program.withReactBatched "main"
|> Program.run
