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

type Entry<'a> = {
    tick: int
    data: 'a
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

let isLastTickBeforeTurn tickInfo = match tickInfo with
                                        | Turn _ -> false
                                        | Tick t -> (t + 1) % ticksPerTurn = 0

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
    let (level, rp) = researchTerraforming player.terraformingLevel researchPoints
    { player with researchPoints = rp; terraformingLevel = level }

let snapshotEconomy player tickInfo =
    match tickInfo with
        | Tick t -> None
        | Turn (tick, _) -> Some { tick = tick; data = totalEconomy player }

let performTurn number player = player |> produce
    
let performTick tickNumber player = player |> terraform |> research
    
let tick player tickInfo robot =
    let afterTick = match tickInfo with
                        | Tick t -> performTick t player
                        | Turn (tick, turn) -> performTick tick player |> performTurn turn
    Seq.fold (fun p action -> action p) afterTick (robot tickInfo afterTick)
    
let simulate player turns snapshot robot = 
    let update (player, entries) tickNumber =
        let tickInfo = if tickNumber % ticksPerTurn = 0 then
                                    let turnNumber = tickNumber / ticksPerTurn
                                    Turn (tickNumber, turnNumber)
                                else
                                    Tick tickNumber
        let player = tick player tickInfo robot
        match snapshot player tickInfo with
            | None -> (player, entries)
            | Some e -> (player, e :: entries)
    let start = match snapshot player (Tick 0) with
                    | None -> []
                    | Some e -> [e]
    let ticks = turns * ticksPerTurn
    seq { 1 .. ticks } |> Seq.fold update (player, start) |> snd

let worldbuilderBot tickInfo player =
    match tickInfo with
        | Turn _ -> Seq.empty //TODO: BUILD WBS here
        | _ when isLastTickBeforeTurn tickInfo -> Seq.ofList [ upgrade ]
        | _ -> Seq.empty

let normalBot tickInfo player = if isLastTickBeforeTurn tickInfo then
                                    Seq.ofList [ upgrade ]
                                else Seq.empty

let writeToCsv entries (name: string) =
    use writer = new StreamWriter(name)
    use csv = new CsvWriter(writer, System.Globalization.CultureInfo.InvariantCulture)
    csv.WriteRecords(entries)

let diagram projX projY entries name =
    let xvals = Seq.map projX entries
    let yvals = Seq.map projY entries
    Scatter(x = xvals, y = yvals, name = name)

let diagramTicksAndData = diagram (fun e -> e.tick) (fun e -> e.data)

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
    let layout = Layout(title = "Economy", xaxis = Xaxis(title = "Ticks"), yaxis = Yaxis(title = "Economy"))
    let logs1 = simulate player1 turns snapshotEconomy worldbuilderBot
    let logs2 = simulate player2 turns snapshotEconomy normalBot
    [ diagramTicksAndData logs1 "With WB"; diagramTicksAndData logs2 "Without WB" ] |> Chart.Plot |> Chart.WithLayout layout |> Chart.Show
    0