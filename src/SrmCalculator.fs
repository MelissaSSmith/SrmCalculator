module SrmCalculations

open Fable.Core
open System

[<Emit("isNaN(parseFloat($0)) ? null : parseFloat($0)  ")>]
let ParseFloat (e : obj) : float option = jsNative

let TransformToDecimal e = 
    match ParseFloat e with
    | Some value -> value |> decimal
    | None -> 0.0m

let TransformToDouble e : double = 
    Decimal.ToDouble e

let DecimalPower (x: decimal) (y:decimal) =
    Math.Pow ((TransformToDouble x), (TransformToDouble y))
    |> TransformToDecimal

let MultiplyBy14922 x = 
    1.4922m * x

let Mcu grainColor grainWeightLbs VolGal : decimal =
    (grainColor * grainWeightLbs) / VolGal

let SrmColor grainColor grainWeightLbs VolGal =
    Mcu grainColor grainWeightLbs VolGal
    |> DecimalPower 0.6859m
    |> MultiplyBy14922
