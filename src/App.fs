module SrmCalculator

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open SrmCalculations

type IJQuery = 
    [<Emit("$0.click($1)")>]
    abstract OnClick : (obj -> unit) -> IJQuery

module JQuery =

  [<Emit("window['$']($0)")>]
  let ready (handler: unit -> unit) : unit = jsNative
  
  [<Emit("$2.css($0, $1)")>]
  let css (prop: string) (value: string) (el: IJQuery) : IJQuery = jsNative
  
  [<Emit("$1.click($0)")>]
  let click (handler: obj -> unit)  (el: IJQuery) : IJQuery = jsNative

  [<Emit("window['$']($0)")>]
  let select (selector: string) : IJQuery = jsNative

let calculateSrm batchSize amount degreesLovibond = 
    SrmColor degreesLovibond (TransformToDecimal amount) (TransformToDecimal batchSize)
    |> RoundToPrecisonTwo
    |> TransformToString

let calculateEbc batchSize amount degreesLovibond = 
    Ebc degreesLovibond (TransformToDecimal amount) (TransformToDecimal batchSize)
    |> RoundToPrecisonTwo
    |> TransformToString

let fillColor (hexColor: string) = 
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    canvas.width <- 1000.
    canvas.height <- 1000.
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- !^hexColor
    ctx.fillRect (10., 10., 55., 50.)

let mainLoop ev =
    let batchSize = document.getElementById("batchSize")?value
    let amount1 = document.getElementById("amount1")?value
    let degreesLovibond = 17.0m
    let srm = calculateSrm batchSize amount1 degreesLovibond
    let ebc = calculateEbc batchSize amount1 degreesLovibond
    document.getElementById("srmResult")?innerHTML <- srm
    document.getElementById("ebcResult")?innerHTML <- ebc
    |> ignore

JQuery.select("#calculate")
    .OnClick(mainLoop)
    |> ignore