module SrmCalculations

open Fable.Core
open System

[<Emit("isNaN(parseFloat($0)) ? null : parseFloat($0)  ")>]
let ParseFloat (e : obj) : float option = jsNative

[<Emit("isNaN(parseInt($0)) ? null : parseInt($0)  ")>]
let ParseInt (e : obj) : int option = jsNative

let TransformToString r = r |> string

let TransformToFloat e = 
    match ParseFloat e with
    | Some value -> value |> float
    | None -> 0.0

let RoundResult (input : float) (precision: int) = 
    Math.Round(input, precision)

let RoundToPrecisonTwo x =
    RoundResult x 2

let TransformToInt e : int = 
    match ParseFloat e with
    | Some value -> value |> int
    | None -> 0

let Power power b =
    Math.Pow(b, power)

let MultiplyByConstant variable constant = 
    variable * constant

let Mcu grainColor grainWeightLbs volGal  =
    (grainColor * grainWeightLbs) / volGal

let SrmColor grainColorList grainWeightLbsList volGal =
    List.zip grainWeightLbsList grainColorList
    |> List.sumBy (fun (a, dl) -> Mcu dl a volGal)
    |> Power 0.6859
    |> MultiplyByConstant 1.4922

let Ebc srm = 
    MultiplyByConstant srm 1.97