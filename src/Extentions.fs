[<AutoOpen>]
module Extentions

open System
open Fable.Core
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

let roundInt64 (x: float) =
    x |> Math.Round |> int64

type private NumberFormat =
    abstract format: int64 -> string

[<Emit("new Intl.NumberFormat('vi-VN')")>]
let private createFormatter (): NumberFormat = jsNative

let private formatter = createFormatter()

let formatNumber (value: int64) =
    formatter.format(value)

let tryParseInt64 (input: string) : int64 option =
    match Int64.TryParse input with
    | true, n -> Some n
    | _ -> None

let tup a b = (a, b)
