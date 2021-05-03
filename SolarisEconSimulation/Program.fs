open System.IO
open CsvHelper
open XPlot.Plotly

type Planet = {
    terraform: int
    economy: int
    worldBuilder: bool
}

type Player = {
    credits: int
    planets: Planet list
}

type Entry = {
    turn: int
    credits: int
    economy: int
}

let economyStart = 5

let terraformingStart = 10

let terraformingLevel = 10

let terraformTotal = terraformingStart + (terraformingLevel * 5)

let ticksPerTurn = 24

let turns = 20

let expenseConfig = 2

let costMultiplier = 2.5

type TickInfo =
    | Turn of int * int
    | Tick of int

let tickOnPlanet planet = if planet.worldBuilder then
                                { planet with terraform = planet.terraform + 1 }
                            else planet

let terraform (player: Player) =
    let planets = List.map tickOnPlanet player.planets
    { player with planets = planets }

let calcUpgradeCosts (planet: Planet) = ((float expenseConfig) * costMultiplier * (planet.economy + 1 |> float)) / ((planet.terraform |> float) * 0.01) |> floor |> int

let rec upgradePlanet (planet: Planet) credits =
    let upgradeCost = calcUpgradeCosts planet
    if upgradeCost < credits then
        upgradePlanet { planet with economy = planet.economy + 1 } (credits - upgradeCost)
    else (planet, credits)

let upgrade (player: Player) =
    let (planets, credits) = Seq.mapFold (fun cr planet -> upgradePlanet planet cr) player.credits player.planets
    { player with planets = Seq.toList planets; credits = credits }

let totalEconomy player = Seq.map (fun (p: Planet) -> p.economy) player.planets |> Seq.sum

let produce (player: Player) =
    { player with credits = player.credits + (totalEconomy player * 10) }

let snapshot player turn = { turn = turn; economy = totalEconomy player; credits = player.credits }

let performTurn number player = player |> upgrade |> produce
    
let performTick tickNumber player = player |> terraform
    
let tick player tickInfo =
    match tickInfo with
        | Tick t -> performTick t player
        | Turn (tick, turn) -> performTick tick player |> performTurn turn
    
let simulate player turns = 
    let update (player, entries) tickNumber =
        let (player, entry) = if tickNumber % ticksPerTurn = 0 then
                                    let turnNumber = tickNumber / ticksPerTurn
                                    let newPlayer = Turn (tickNumber, turnNumber) |> tick player
                                    (newPlayer, snapshot newPlayer turnNumber |> Some)
                                else
                                    (Tick tickNumber |> tick player, None)
        match entry with
            | None -> (player, entries)
            | Some e -> (player, e :: entries)
    let start = [snapshot player 0]
    let ticks = turns * ticksPerTurn
    seq { 1 .. ticks } |> Seq.fold update (player, start) |> snd

let writeToCsv entries (name: string) =
    use writer = new StreamWriter(name)
    use csv = new CsvWriter(writer, System.Globalization.CultureInfo.InvariantCulture)
    csv.WriteRecords(entries)

let toPlot entries name =
    let turns = Seq.map (fun e -> e.turn) entries
    let econ = Seq.map (fun e -> e.economy) entries
    Scatter(x = turns, y = econ, name = name)

let genPlanet wb = {
    terraform = terraformTotal
    economy = economyStart
    worldBuilder = wb
}

[<EntryPoint>]
let main argv =    
    let player1 = {
        credits = 0
        planets = Seq.init 10 (fun _ -> genPlanet true) |> Seq.toList
    }
    let player2 = {
        credits = 10000
        planets = Seq.init 10 (fun _ -> genPlanet false) |> Seq.toList
    }
    let logs1 = simulate player1 turns
    let logs2 = simulate player2 turns
    let layout = Layout(title = "Economy", xaxis = Xaxis(title = "Turns"), yaxis = Yaxis(title = "Economy"))
    [ toPlot logs1 "With WB"; toPlot logs2 "Without WB" ] |> Chart.Plot |> Chart.WithLayout layout |> Chart.Show
    0