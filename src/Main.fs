module Main

open Fastoch.Elmish

Program.mkSimple State.init State.update View.render
|> Program.withFastoch "app"
#if DEBUG
|> Program.withTrace (fun msg model _ -> printfn "msg: %A\nmodel: %A" msg model)
#endif
|> Program.run
