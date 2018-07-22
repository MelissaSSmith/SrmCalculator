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

type SrmHex =
    { SrmKey: int
      HexValue: string}

let calculateSrmWithListOfInput amountList degreesLovibondList batchSize =
    let bs = TransformToFloat batchSize
    let aList = amountList |> List.map TransformToFloat
    let dlList = degreesLovibondList |> List.map TransformToFloat
    SrmColor dlList aList bs 
    |> RoundToPrecisonTwo
    |> TransformToString

let calculateEbc srm = 
    Ebc (TransformToFloat srm)
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
[<Literal>]
let SrmHexFile = "file:\\\\C:\\Users\\melissaSusan\\source\\repos\\SrmCalculator\\src\\SrmHex.json" 

let Fermentables = @"[ { ""Id"": 1,""Name"": ""Abbey Malt"",""Country"": ""German"",""Category"": ""Grain"",""Type"": ""Base Malt"",""DegreesLovibond"": 17,""Ppg"": 33  }, { ""Id"": 2,""Name"": ""Amber"",""Country"": ""United Kingdom"",""Category"": ""Grain"",""Type"": ""Base Malt"",""DegreesLovibond"": 27,""Ppg"": 32  }]"
let SrmHexValues = @"[{""SrmKey"":1,""HexValue"":""#FFE699""},{""SrmKey"":2,""HexValue"":""#FFD878""},{""SrmKey"":3,""HexValue"":""#FFCA5A""},{""SrmKey"":4,""HexValue"":""#FFBF42""},{""SrmKey"":5,""HexValue"":""#FBB123""},{""SrmKey"":6,""HexValue"":""#F8A600""},{""SrmKey"":7,""HexValue"":""#F39C00""},{""SrmKey"":8,""HexValue"":""#EA8F00""},{""SrmKey"":9,""HexValue"":""#E58500""},{""SrmKey"":10,""HexValue"":""#DE7C00""},{""SrmKey"":11,""HexValue"":""#D77200""},{""SrmKey"":12,""HexValue"":""#CF6900""},{""SrmKey"":13,""HexValue"":""#CB6200""},{""SrmKey"":14,""HexValue"":""#C35900""},{""SrmKey"":15,""HexValue"":""#BB5100""},{""SrmKey"":16,""HexValue"":""#B54C00""},{""SrmKey"":17,""HexValue"":""#B04500""},{""SrmKey"":18,""HexValue"":""#A63E00""},{""SrmKey"":19,""HexValue"":""#A13700""},{""SrmKey"":20,""HexValue"":""#9B3200""},{""SrmKey"":21,""HexValue"":""#952D00""},{""SrmKey"":22,""HexValue"":""#8E2900""},{""SrmKey"":23,""HexValue"":""#882300""},{""SrmKey"":24,""HexValue"":""#821E00""},{""SrmKey"":25,""HexValue"":""#7B1A00""},{""SrmKey"":26,""HexValue"":""#771900""},{""SrmKey"":27,""HexValue"":""#701400""},{""SrmKey"":28,""HexValue"":""#6A0E00""},{""SrmKey"":29,""HexValue"":""#660D00""},{""SrmKey"":30,""HexValue"":""#5E0B00""},{""SrmKey"":31,""HexValue"":""#5A0A02""},{""SrmKey"":32,""HexValue"":""#600903""},{""SrmKey"":33,""HexValue"":""#520907""},{""SrmKey"":34,""HexValue"":""#4C0505""},{""SrmKey"":35,""HexValue"":""#470606""},{""SrmKey"":36,""HexValue"":""#440607""},{""SrmKey"":37,""HexValue"":""#3F0708""},{""SrmKey"":38,""HexValue"":""#3B0607""},{""SrmKey"":39,""HexValue"":""#3A070B""},{""SrmKey"":40,""HexValue"":""#36080A""}]"

let getFermentables =
    ofJson<Fermentable list> Fermentables
    |> Seq.sortBy(fun f -> f.Category)
    |> Seq.sortBy(fun f -> f.Country)
    |> Seq.sortBy(fun f -> f.Name)

let getSrmHexList =
    ofJson<SrmHex list> SrmHexValues

let getDegreesLovibondForFermentable id =
    match id with
    | 0 -> 0
    | _ -> (Seq.find(fun f -> f.Id = id) getFermentables).DegreesLovibond

let getSrmHexValue srm =
    let hex = Seq.find(fun f -> f.SrmKey = (TransformToInt srm)) getSrmHexList
    hex.HexValue

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

let setResult resultId value =
    document.getElementById(resultId)?innerHTML <- value

let init() = 
    fillDropdown "grain1"
    fillDropdown "grain2"
    fillDropdown "grain3"
    fillDropdown "grain4"
    fillDropdown "grain5"
    |> ignore

let mainLoop ev =
    let batchSize = document.getElementById("batchSize")?value
    let amountList = List.init 5 (fun i -> document.getElementById("amount"+TransformToString(i+1))?value)
    let grainList = List.init 5 (fun i -> TransformToInt(document.getElementById("grain"+TransformToString(i+1))?value))
    let degreesLovibondList = List.init 5 (fun i -> getDegreesLovibondForFermentable (grainList |> List.item i ))
    let srm = calculateSrmWithListOfInput amountList degreesLovibondList batchSize
    srm
    |> setResult "srmResult"
    |> ignore
    calculateEbc srm
    |> setResult "ebcResult"
    |> ignore
    let color = getSrmHexValue srm
    JQuery.select("#approxColor")
    |> JQuery.css "background-color" color
    |> ignore

JQuery.select("#calculate")
    .OnClick(mainLoop)
    |> ignore

init()