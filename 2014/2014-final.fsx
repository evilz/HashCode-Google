#I @"../packages/"
open System.Diagnostics
open System
#load "FSharp.Charting/lib/net45/FSharp.Charting.fsx"
//#r @"System.Xaml"
//open System.Xaml
open System.IO

#if INTERACTIVE
let msg = "Interactive"
#else
let msg = "Not Interactive"
#endif

printfn "%s" msg


module Model =

    type Direction = One | Bi

    type Street = {junctionA:int; junctionB:int;  direction:Direction; length: int; cost: int}
        
    type Junction = {lat:float; long:float}
        with static member Empty = {lat=0.0; long=0.0}
             
    type Car = obj
    type InputCount = { junctions: int; streets: int; seconds: int; cars:int; startingAt:int  }
        with static member Empty = { junctions=0; streets=0; seconds=0; cars=0; startingAt = 0}
             
    type Input = {
        counts: InputCount;
        junctions: Junction[];
        streets: Street[]
    }
    with static member Empty = {counts=InputCount.Empty; junctions= Array.empty; streets= Array.empty } 

   
module Parser =
  open Model
  open System.IO

  let toDirection (rawval:int) =   
      match rawval with
         | 1 -> One
         | 2 -> Bi
         | _ -> failwithf "Can't parse direction type %i" rawval

  let toStreet (str:string) =   
    let vals = str.Split(' ') |> Array.map int 
    { junctionA = vals.[0]; junctionB = vals.[1]; direction = vals.[2] |> toDirection; length = vals.[3]; cost = vals.[4]; }

  let toJunction (str:string) = 
    let vals = str.Split(' ') |> Array.map float 
    { lat= vals.[0]; long = vals.[1] } 

  let toCounts (str:string) = 
    let vals = str.Split(' ') |> Array.map int 
    { junctions = vals.[0];   streets= vals.[1]; seconds=vals.[2]; cars=vals.[3]; startingAt = vals.[4] } 

  let ParseInput (reader:TextReader) = 

    let parseLines count converter = seq { for i in 1 .. count -> reader.ReadLine() |> converter } |> Seq.toArray 

    let counts = reader.ReadLine() |> toCounts
    let junctions = parseLines counts.junctions toJunction
    let streets = parseLines counts.streets toStreet 

    { counts = counts; junctions = junctions; streets = streets }


module Algo =

    

    
         
        



module Solution =
    // should compute score from the output file !
    let computeScore () = 0



// === MAIN ===
open System
open System.IO
#time

let inputFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__, @"paris_54000.txt")

let input = File.OpenText(inputFile) |> Parser.ParseInput

open FSharp.Charting

Chart.Bar [ for x in 0 .. input.streets.Length -> x, x*x ]