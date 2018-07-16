module SrmCalculator

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open SrmCalculations
open System

type IJQuery = 
    [<Emit("$0.click($1)")>]
    abstract OnClick : (obj -> unit) -> IJQuery

module JQuery =

    [<Emit("window['$']($0)")>]
    let ready (handler: unit -> unit) : unit = jsNative
      
    [<Emit("$2.css($0, $1)")>]
    let css (prop: string) (value: string) (el: IJQuery) : IJQuery = jsNative
      
    [<Emit("$1.click($0)")>]
    let click (handler: obj -> unit) (el: IJQuery) : IJQuery = jsNative

    [<Emit("window['$']($0)")>]
    let select (selector: string) : IJQuery = jsNative

    [<Emit("$0.empty()")>]
    let empty (el: IJQuery) : IJQuery = jsNative

    [<Emit("$1.append($0)")>]
    let append (prop: string) (el: IJQuery) : IJQuery = jsNative

    [<Emit("$2.prop($0, $1)")>]
    let prop (property: string) (value: int) (el: IJQuery) : IJQuery = jsNative

type Fermentable = 
    { Id: int
      Name: string
      Country: string
      Category: string
      Type: string
      DegreesLovibond: int
      Ppg: int }

let calculateSrm batchSize amount degreesLovibond = 
    SrmColor (TransformToFloat degreesLovibond) (TransformToFloat amount) (TransformToFloat batchSize)
    |> RoundToPrecisonTwo
    |> TransformToString

let calculateEbc batchSize amount degreesLovibond = 
    Ebc (TransformToFloat degreesLovibond) (TransformToFloat amount) (TransformToFloat batchSize)
    |> RoundToPrecisonTwo
    |> TransformToString

let fillColor (hexColor: string) = 
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    canvas.width <- 1000.
    canvas.height <- 1000.
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- !^hexColor
    ctx.fillRect (10., 10., 55., 50.)

[<Literal>]
let FermentableFile = "file:\\\\C:\\Users\\melissaSusan\\source\\repos\\SrmCalculator\\src\\Fermentables.json" 

let Fermentables = @"[ { ""Id"": 1,""Name"": ""Abbey Malt"",""Country"": ""German"",""Category"": ""Grain"",""Type"": ""Base Malt"",""DegreesLovibond"": 17,""Ppg"": 33  }]"

let getFermentables =
    ofJson<Fermentable list> Fermentables
    |> Seq.sortBy(fun f -> f.Category)
    |> Seq.sortBy(fun f -> f.Country)
    |> Seq.sortBy(fun f -> f.Name)

let getDegreesLovibondForFermentable id =
    let ferm = Seq.find(fun f -> f.Id = id) getFermentables
    ferm.DegreesLovibond

let FillInFermentables dropdownId (element: IJQuery) =
    for f in getFermentables do
        let newRow = String.Format("<option value={0}>{1} {2}</option>", f.Id, f.Country, f.Name)
        JQuery.select("#"+dropdownId)
        |> JQuery.append newRow
        |> ignore

let fillDropdown dropdownId = 
    JQuery.select("#"+dropdownId)
        |> JQuery.empty
        |> JQuery.append "<option selected disabled>Select Grain ...</option>"
        |> JQuery.prop "selectedIndex" 0
        |> FillInFermentables dropdownId

let init() = 
    fillDropdown "grain1"
    fillDropdown "grain2"
    fillDropdown "grain3"
    fillDropdown "grain4"
    fillDropdown "grain5"
    |> ignore

let mainLoop ev =
    let batchSize = document.getElementById("batchSize")?value
    let amount1 = document.getElementById("amount1")?value
    let grain1 = TransformToInt(document.getElementById("grain1")?value)
    let degreesLovibond = getDegreesLovibondForFermentable grain1
    let srm = calculateSrm batchSize amount1 degreesLovibond
    let ebc = calculateEbc batchSize amount1 degreesLovibond
    document.getElementById("srmResult")?innerHTML <- srm
    document.getElementById("ebcResult")?innerHTML <- ebc
    |> ignore

JQuery.select("#calculate")
    .OnClick(mainLoop)
    |> ignore

init()