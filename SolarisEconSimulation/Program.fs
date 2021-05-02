open System.IO
open CsvHelper

type Planet = {
    terraform: int
    economy: int
    worldBuilder: bool
}

type Player = {
    name: string
    credits: int
    planet: Planet
}

type Entry = {
    name: string
    credits: int
    economy: int
    turn: int
}

let ticksPerTurn = 24

let turns = 10

let terraform player = if player.planet.worldBuilder then
                            { player with planet = { player.planet with terraform = player.planet.terraform + ticksPerTurn } }
                        else player

let upgrade player = player

let produce (player: Player) = { player with credits = player.credits + (player.planet.economy * 10) }

let turn player number = 
    // WB upgrade? -> build econ -> produce credits
    let newPlayer = player |> terraform |> upgrade |> produce
    let entry = {
        name = player.name;
        turn = number;
        economy = player.planet.economy;
        credits = player.credits
    }
    (newPlayer, entry)
    
let simulate player turns = 
    let update (player, entries) turnNumber = 
        let (newPlayer, newEntry) = turn player turnNumber
        (newPlayer, newEntry :: entries)
    seq { 1 .. turns } |> Seq.fold update (player, []) |> snd

let writeToCsv entries (name: string) =
    use writer = new StreamWriter(name)
    use csv = new CsvWriter(writer, System.Globalization.CultureInfo.InvariantCulture)
    csv.WriteRecords(entries)

[<EntryPoint>]
let main argv =
    let player1 = {
        name = "WB player"
        credits = 0
        planet = {
            terraform = 10
            economy = 5
            worldBuilder = true
        }
    }
    let player2 = {
        name = "Conventional"
        credits = 1000
        planet = {
            terraform = 10
            economy = 5
            worldBuilder = false
        }
    }
    let logs1 = simulate player1 turns
    let logs2 = simulate player2 turns
    writeToCsv logs1 "player1.csv"
    writeToCsv logs2 "player2.csv"
    0