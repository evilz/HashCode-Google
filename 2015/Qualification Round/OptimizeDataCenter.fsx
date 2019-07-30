open System
open System.IO

[<AutoOpen>]
module Model =
    type Server = { index:int; size: int; capacity: int; isUsed: bool } 
    module Server = 
        let score server = double server.capacity / double server.size

    type Pool = { index:int; servers: Server list }

    type SlotState = 
        | Empty
        | Unavailable
        | Allocated of Server
    
    type Slot = { row: int; slot: int; state: SlotState}
    module Slot =
        let capacity slot = match slot.state with
                                | Allocated s -> s.capacity
                                | _ -> 0

    // type Row = {
    //     index:int
    //     slots: int[]
    //     servers: Server list
    // }

    type DataCenter = {
        pools: Pool list
        servers: Server list
        rows: Slot[,]
       // deadSlots: Slot list
    }

module Parse =

    let private readInts (reader:StreamReader) = reader.ReadLine().Split(' ') |> Array.map int

    let readFile filePath =
        let input = File.OpenText(filePath)
        let (rowCount, slotCount, unavailableSlotCount, poolCount, serverCount ) =
            input
            |> readInts
            |> fun x -> x.[0],x.[1],x.[2],x.[3],x.[4]
 
        let deadSlots = [for i in 0..unavailableSlotCount-1 ->
                            input
                            |> readInts
                            |> fun x -> (x.[0], x.[1])
                        ]

        let servers =  [for i in 0..serverCount-1 ->
                            input
                            |> readInts
                            |> fun x -> { index = i; size = x.[0]; capacity = x.[1]; isUsed = false }
                            ]

        let rows = Array2D.init rowCount slotCount (fun y x ->
                        if deadSlots |> List.contains (y,x) 
                        then {row=y;slot=x; state= Unavailable}
                        else {row=y;slot=x; state= Empty}
                        )
        
        
        { 
            pools = [for i in 0..poolCount-1 -> {index = i; servers = list.Empty}]
            servers = servers
            rows = rows
        }


module Resolver = 


    let totalCapacity (row: Slot[]) = row |> Seq.map Slot.capacity |> Seq.reduce (+)

    let sortByCapacity rows = 
        let rowCount = (Array2D.length1 rows) 
        seq { for r in 0 .. rowCount-1 -> 
                let row = rows.[r,*]
                (r, row |> totalCapacity )
        }
        |> Seq.sortByDescending (fun (_, capacity)->capacity)
        |> Seq.map (fun (i,_) -> i, rows.[i,*])

    let serverByScoreAndSize servers = 
            servers 
            |> List.sortByDescending (fun x -> x |> Server.score)
            |> List.sortBy (fun x -> x.size)

    let availableSliceOfSlot (row: Slot[]) = 
        let lenght = row |>Array.length
        let initialState = {|slices = []; lastIndex= -1 |}
       
        (initialState , [0..lenght])
            ||> List.fold (fun state i ->
                                match row.[i].state with
                                | Unavailable | Allocated _ -> 
                                    let newSlice = (state.lastIndex, i)
                                    let slices = newSlice :: state.slices
                                    {|state with slices = slices; lastIndex = -1 |}
                                | Empty -> 
                                    if state.lastIndex = -1 then {|state with lastIndex = i |}
                                    else state
                )
        |> fun x -> x.slices
        //result.slices

    // try to add in perfect space                        
    let tryAddInRow row server =
        let slices = row |> availableSliceOfSlot

        let perfectOne = slices |> Seq.tryFind (fun (a,b) -> (b-a) = server.size)

        match perfectOne with
        | Some (a,b) -> 
            let server = {server with isUsed = true }
            for i in a..b do
                    row.[i] <- {row.[i] with state = Allocated server}
            server, row
        | None ->
            let overSized = slices |> List.sortBy (fun (a,b) -> b-a) |> List.tryHead
            match overSized with
            | Some (a,b) -> 
                let server = {server with isUsed = true }
                for i in a..b do
                    row.[i] <- {row.[i] with state = Allocated server}
                server, row
            | None -> server, [||]




    // get row
    //let row = array2d.[0,*];;

    let putInRows datacenter =

        let serversByScoreAndSize = datacenter.servers
                                    |> List.sortByDescending Server.score
                                    |> List.sortBy (fun s -> s.size)

        // for s in serverByScoreAndSize do
        //     let rowsOrderedForPoolCapacity = datacenter.rows |> List.sortBy Row.totalCapacity
        //     for r in rowsOrderedForPoolCapacity do
        //         match tryAddInRow r s with
        //         | Some x ->  /// break
        //         | None -> ///loop


        // let sorted = 
        //     datacenter.rows 
        //     |> sortByCapacity
        //     |> Seq.head 
        //     |> fun (i, row) -> row.[2] <- { row.[2] with state = Unavailable}

        // serverByScoreAndSize
        // |> 

// START


let filePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"dc.in")

let datacenter = Parse.readFile(filePath)

datacenter
