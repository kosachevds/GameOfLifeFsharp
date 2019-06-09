module Game

open System.IO

type private Change = {
    i: int
    j: int
    state: bool
}


let private doDelay (msDelay: int) =
    System.Threading.Thread.Sleep(msDelay)

let private clearScreen() =
    System.Console.Clear()

let private printGrid grid =
    let livengCellMark = "o"
    let deadCellMark = " "
    let boolArrayToMarksString array =
        array
        |> Array.map (fun x -> if x then livengCellMark else deadCellMark)
        |> String.concat ""

    let stringGrid =
        grid
        |> Array.map boolArrayToMarksString
        |> String.concat "\n"

    printfn "%s" stringGrid

// TODO: with bool[,]
type Game = { Grid: bool[][] } with
    member this.isLivingCell(i, j) =
        this.Grid.[i].[j]

    member this.countLivingNeigbours i j =

        let correctIndex index maxIndex =
            match index with
            | index when index >= maxIndex -> index % maxIndex
            | index when index < 0 -> index + maxIndex
            | _ -> index

        let correctPair (i, j) =
            (correctIndex i this.RowCount, correctIndex j this.ColumnCount)

        let addCenterIndices (iShift, jShift) =
            (i + iShift, j + jShift)

        // TODO: try calc adds once
        let shifts = { -1..1 }
        shifts
        |> Seq.collect (fun i -> seq { for j in shifts -> (i, j) })
        |> Seq.filter (fun (i, j) -> i <> 0 || j <> 0)
        // |> Seq.toList
        |> Seq.map (addCenterIndices >> correctPair >> this.isLivingCell)
        |> Seq.filter id
        |> Seq.length

    member private this.processCell i j =
        let livingCount = this.countLivingNeigbours i j
        if this.isLivingCell(i, j) then
            if livingCount < 2 || livingCount > 3 then
                Some { i = i;  j = j; state = false}
            else
                None
        else
            if livingCount = 3 then
                Some { i = i;  j = j; state = true}
            else None

    member this.doStep() =
        let processRow index row =
            row
            |> Array.mapi (fun i _ -> this.processCell index i)
        this.Grid
        |> Array.mapi processRow
        |> Array.concat
        |> Array.choose id
        |> Array.iter (fun x -> this.Grid.[x.i].[x.j] <- x.state)

    member this.RowCount = this.Grid.Length

    member this.ColumnCount = this.Grid.[0].Length

    member public this.RunInConsoleWithDelay msDelay =
        for count in Seq.initInfinite((+)1) do
            clearScreen()
            printfn "Step %d:" count
            printGrid this.Grid
            this.doStep()
            doDelay msDelay

let public readFromFileWithInt filename =
    let toBoolArray charArray =
        charArray
        |> Array.map ((=)'1')

    let boolMatrix =
        File.ReadAllLines filename
        |> Array.map (Seq.toArray >> toBoolArray)

    { Grid=boolMatrix }
