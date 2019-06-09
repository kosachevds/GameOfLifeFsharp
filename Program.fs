module Program

[<EntryPoint>]
let main argv =
    let game = Game.readFromFileWithInt "D:\\projects\\F#\\GameOfLife\\grid"
    game.RunInConsoleWithDelay(250)
    0 // return an integer exit code

