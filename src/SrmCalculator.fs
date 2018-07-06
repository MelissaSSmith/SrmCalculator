module SrmCalculations

open Fable.Core
open System

[<Emit("isNaN(parseFloat($0)) ? null : parseFloat($0)  ")>]
let ParseFloat (e : obj) : float option = jsNative

let TransformToString r = r |> string

let TransformToDecimal e = 
    match ParseFloat e with
    | Some value -> value |> decimal
    | None -> 0.0m

let RoundDecimal (input : decimal) (precision: int) = 
    Math.Round(input, precision)

let RoundToPrecisonTwo x =
    RoundDecimal x 2

let TransformToDouble e : double = 
    match ParseFloat e with
    | Some value -> value |> double
    | None -> 0.0

let DecimalPower (power: decimal) (b:decimal) =
    Math.Pow ((TransformToDouble b), (TransformToDouble power))
    |> TransformToDecimal

let MultiplyByConstant variable constant = 
    variable * constant

let Mcu grainColor grainWeightLbs volGal : decimal =
    (grainColor * grainWeightLbs) / volGal

let SrmColor grainColor grainWeightLbs volGal =
    Mcu grainColor grainWeightLbs volGal
    |> DecimalPower 0.6859m
    |> MultiplyByConstant 1.4922m

let Ebc grainColor grainWeightLbs volGal = 
    SrmColor grainColor grainWeightLbs volGal
    |> MultiplyByConstant 1.97m
