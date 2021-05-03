open System.IO
open CsvHelper

type Planet = {
    terraform: int
    economy: int
    worldBuilder: bool
}

type Player = {
    credits: int
    planet: Planet
}

type Entry = {
    turn: int
    credits: int
    economy: int
}

let economyStart = 5

let terraformingStart = 10

let terraformingLevel = 10

let ticksPerTurn = 24

let turns = 20

let expenseConfig = 2

let costMultiplier = 2.5

let terraform player = if player.planet.worldBuilder then
                            { player with planet = { player.planet with terraform = player.planet.terraform + ticksPerTurn } }
                        else player

let calcUpgradeCosts (planet: Planet) = ((float expenseConfig) * costMultiplier * (planet.economy + 1 |> float)) / ((planet.terraform |> float) * 0.01) |> floor |> int

let rec upgradePlanet (planet: Planet) credits =
    let upgradeCost = calcUpgradeCosts planet
    if upgradeCost < credits then
        upgradePlanet { planet with economy = planet.economy + 1 } (credits - upgradeCost)
    else (planet, credits)

let upgrade player = 
    let (newPlanet, newCredits) = upgradePlanet player.planet player.credits
    { player with planet = newPlanet; credits = newCredits }

let produce (player: Player) = { player with credits = player.credits + (player.planet.economy * 10) }

let snapshot player turn = { turn = turn; economy = player.planet.economy; credits = player.credits }

let turn player number = 
    // WB upgrade? -> build econ -> produce credits
    let newPlayer = player |> terraform |> upgrade |> produce
    (newPlayer, snapshot newPlayer)
    
let simulate player turns = 
    let update (player, entries) turnNumber = 
        let (newPlayer, newEntry) = turn player turnNumber
        (newPlayer, newEntry :: entries)
    let start = [snapshot player]
    seq { 1 .. turns } |> Seq.fold update (player, start) |> snd

let writeToCsv entries (name: string) =
    use writer = new StreamWriter(name)
    use csv = new CsvWriter(writer, System.Globalization.CultureInfo.InvariantCulture)
    csv.WriteRecords(entries)

[<EntryPoint>]
let main argv =
    let terraformTotal = terraformingStart + (terraformingLevel * 5)
    
    let player1 = {
        credits = 0
        planet = {
            terraform = terraformTotal
            economy = economyStart
            worldBuilder = true
        }
    }
    let player2 = {
        credits = 1000
        planet = {
            terraform = terraformTotal
            economy = economyStart
            worldBuilder = false
        }
    }
    let logs1 = simulate player1 turns
    let logs2 = simulate player2 turns
    0