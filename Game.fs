module Game

open System.IO

type private Change = {
    i: int
    j: int
    state: bool
}

let correctIndex index maxIndex =
    match index with
    | index when index >= maxIndex -> index % maxIndex
    | index when index < 0 -> index + maxIndex
    | _ -> index

type Game(grid: bool[][]) =  // TODO: with bool[,]
    let isLivingCell(i, j) =
        let rowCount = grid.Length
        let columnCount = grid.[0].Length
        grid.[correctIndex i rowCount].[correctIndex j columnCount]

    let countLivingNeigbours i j =
        let isNeighbour iAdd jAdd = iAdd <> jAdd || iAdd <> 0
        // TODO: remade this block
        let shifts = [ -1..1 ]
        let mutable count = 0
        for iShift in shifts do
            for jShift in shifts do
                if isNeighbour iShift jShift && isLivingCell(i + iShift, j + jShift) then
                    count <- count + 1
        count

    let processCell i j =
        let livingCount = countLivingNeigbours i j
        if isLivingCell(i, j) then
            if livingCount < 2 || livingCount > 3 then
                Some { i = i;  j = j; state = false}
            else
                None
        else
            if livingCount = 3 then
                Some { i = i;  j = j; state = true}
            else None

    let doStep() =
        let processRow index row =
            row
            |> Array.mapi (fun i _ -> processCell index i)
        grid
        |> Array.mapi processRow
        |> Array.concat
        |> Array.choose id
        |> Array.iter (fun x -> grid.[x.i].[x.j] <- x.state)

    let doDelay (msDelay: int) =
        System.Threading.Thread.Sleep(msDelay)

    let clearScreen() =
        System.Console.Clear()

    let printGrid() =
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

    member public this.RunInConsoleWithDelay msDelay =
        for count in Seq.initInfinite((+)1) do
            clearScreen()
            printfn "Step %d:" count
            printGrid()
            doStep()
            doDelay(msDelay)

let public readFromFileWithInt filename =
    let toBoolArray charArray =
        charArray
        |> Array.map ((=)'1')


    let boolMatrix =
        File.ReadAllLines filename
        |> Array.map (Seq.toArray >> toBoolArray)

    Game boolMatrix
