[<AutoOpen>]
module Extentions

open Fastoch.Feliz

type Html with
    static member label (text: string) =
        Html.label [ prop.text text ]

module List =
    let changeAt i map list =
        list |> List.updateAt i (map list[i])

module Results =
    let allOk xs = xs |> Seq.forall Result.isOk

type Result<'a,'b> with
    member this.Value =
        match this with
        | Ok value -> value
        | Error _ -> failwith "OOPS"
