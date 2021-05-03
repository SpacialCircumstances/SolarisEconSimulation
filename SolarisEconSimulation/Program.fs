open System.IO
open CsvHelper
open XPlot.Plotly

type Planet = {
    terraform: int
    economy: int
    worldBuilder: bool
    research: int
}

type Player = {
    credits: int
    planets: Planet list
    terraformingLevel: int
    researchPoints: int
}

type Entry = {
    turn: int
    credits: int
    economy: int
}

let economyStart = 5

let researchStart = 1

let terraformingStart = 10

let ticksPerTurn = 24

let turns = 20

let expenseConfig = 2

let costMultiplier = 2.5

let terraformingBonusPerLevel = 5

type TickInfo =
    | Turn of int * int
    | Tick of int

let tickOnPlanet planet = if planet.worldBuilder then
                                { planet with terraform = planet.terraform + 1 }
                            else planet

let terraform (player: Player) =
    let planets = List.map tickOnPlanet player.planets
    { player with planets = planets }

let totalTerraforming player planet = planet.terraform + (player.terraformingLevel * terraformingBonusPerLevel)

let calcUpgradeCosts (player: Player) (planet: Planet) = ((float expenseConfig) * costMultiplier * (planet.economy + 1 |> float)) / ((totalTerraforming player planet |> float) * 0.01) |> floor |> int

let rec upgradePlanet player (planet: Planet) credits =
    let upgradeCost = calcUpgradeCosts player planet
    if upgradeCost < credits then
        upgradePlanet player { planet with economy = planet.economy + 1 } (credits - upgradeCost)
    else (planet, credits)

let upgrade (player: Player) =
    let (planets, credits) = Seq.mapFold (fun cr planet -> upgradePlanet player planet cr) player.credits player.planets
    { player with planets = Seq.toList planets; credits = credits }

let totalEconomy player = Seq.map (fun (p: Planet) -> p.economy) player.planets |> Seq.sum

let totalResearch player = Seq.map (fun (p: Planet) -> p.research) player.planets |> Seq.sum

let produce (player: Player) =
    { player with credits = player.credits + (totalEconomy player * 10) }

let rec researchTerraforming level rp =
    let requiredForNextLevel = 100 * level
    if rp > requiredForNextLevel then
        researchTerraforming (level + 1) (rp - requiredForNextLevel)
    else (level, rp)

let research (player: Player) =
    let researchPoints = player.researchPoints + totalResearch player
    let (rp, level) = researchTerraforming player.terraformingLevel researchPoints
    { player with researchPoints = rp; terraformingLevel = level }

let snapshotEconomy player tickInfo =
    match tickInfo with
        | Tick t -> None
        | Turn (tick, turn) -> Some { turn = turn; economy = totalEconomy player; credits = player.credits }

let performTurn number player = player |> upgrade |> produce
    
let performTick tickNumber player = player |> terraform |> research
    
let tick player tickInfo =
    match tickInfo with
        | Tick t -> performTick t player
        | Turn (tick, turn) -> performTick tick player |> performTurn turn
    
let simulate player turns snapshot = 
    let update (player, entries) tickNumber =
        let tickInfo = if tickNumber % ticksPerTurn = 0 then
                                    let turnNumber = tickNumber / ticksPerTurn
                                    Turn (tickNumber, turnNumber)
                                else
                                    Tick tickNumber
        let player = tick player tickInfo
        match snapshot player tickInfo with
            | None -> (player, entries)
            | Some e -> (player, e :: entries)
    let start = match snapshot player (Tick 0) with
                    | None -> []
                    | Some e -> [e]
    let ticks = turns * ticksPerTurn
    seq { 1 .. ticks } |> Seq.fold update (player, start) |> snd

let writeToCsv entries (name: string) =
    use writer = new StreamWriter(name)
    use csv = new CsvWriter(writer, System.Globalization.CultureInfo.InvariantCulture)
    csv.WriteRecords(entries)

let toEconPlot entries name =
    let turns = Seq.map (fun e -> e.turn) entries
    let econ = Seq.map (fun e -> e.economy) entries
    Scatter(x = turns, y = econ, name = name)

let genPlanet wb = {
    terraform = terraformingStart
    economy = economyStart
    worldBuilder = wb
    research = researchStart
}

[<EntryPoint>]
let main argv =    
    let player1 = {
        terraformingLevel = 1
        researchPoints = 0
        credits = 0
        planets = Seq.init 10 (fun _ -> genPlanet true) |> Seq.toList
    }
    let player2 = {
        terraformingLevel = 1
        researchPoints = 0
        credits = 10000
        planets = Seq.init 10 (fun _ -> genPlanet false) |> Seq.toList
    }
    let logs1 = simulate player1 turns snapshotEconomy
    let logs2 = simulate player2 turns snapshotEconomy
    let layout = Layout(title = "Economy", xaxis = Xaxis(title = "Turns"), yaxis = Yaxis(title = "Economy"))
    [ toEconPlot logs1 "With WB"; toEconPlot logs2 "Without WB" ] |> Chart.Plot |> Chart.WithLayout layout |> Chart.Show
    0