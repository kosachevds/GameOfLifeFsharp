module Program

[<EntryPoint>]
let main argv =
    let game = Game.readFromFileWithInt "grid"
    game.RunInConsoleWithDelay(250)
    0 // return an integer exit code
