open System.IO
open System.Xml.Schema
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

type CurrentData = {
    economy: int
    averageEconomyUpgradePrice: int
    science: int
    terraformingLevel: int
}

type Entry<'a> = {
    tick: int
    data: 'a
}

let worldBuilderPrice = 1000

let economyStart = 5

let researchStart = 1

let terraformingStart = 30

let ticksPerTurn = 24

let turns = 50

let expenseConfig = 2

let costMultiplierEconomy = 2.5

let costMultiplierScience = 20.0

let terraformingBonusPerLevel = 5

type TickInfo =
    | Turn of int * int
    | Tick of int

let isLastTickBeforeTurn tickInfo = match tickInfo with
                                        | Turn _ -> false
                                        | Tick t -> (t + 1) % ticksPerTurn = 0

let getTick tickInfo = match tickInfo with
                        | Turn (t, _) -> t
                        | Tick t -> t

let tickOnPlanet planet = if planet.worldBuilder then
                                { planet with terraform = planet.terraform + 1 }
                            else planet

let terraform (player: Player) =
    let planets = List.map tickOnPlanet player.planets
    { player with planets = planets }

let totalTerraforming (player: Player) planet = planet.terraform + (player.terraformingLevel * terraformingBonusPerLevel)


let calcUpgradeCosts (player: Player) (planet: Planet) level costMultiplier = ((float expenseConfig) * costMultiplier * (level + 1 |> float)) / ((totalTerraforming player planet |> float) * 0.01) |> floor |> int
let calcEconomyUpgradeCosts (player: Player) (planet: Planet) = calcUpgradeCosts player planet planet.economy costMultiplierEconomy

let calcResearchUpgradeCosts (player: Player) (planet: Planet) = calcUpgradeCosts player planet planet.research costMultiplierScience

let rec upgradePlanetEconomy player (planet: Planet) credits =
    let upgradeCost = calcEconomyUpgradeCosts player planet
    if upgradeCost <= credits then
        upgradePlanetEconomy player { planet with economy = planet.economy + 1 } (credits - upgradeCost)
    else (planet, credits)

let upgradePlanetResearch player (planet: Planet) credits =
    let upgradeCost = calcResearchUpgradeCosts player planet
    if upgradeCost <= credits then
        { planet with research = planet.research + 1 }, credits - upgradeCost
    else (planet, credits)

let upgradeEconomy (player: Player) =
    let (planets, credits) = Seq.mapFold (fun cr planet -> upgradePlanetEconomy player planet cr) player.credits player.planets
    { player with planets = Seq.toList planets; credits = credits }

let upgradeEconomyUntilPrice maxPrice (player: Player) =
    let (planets, credits) = Seq.mapFold (fun cr planet ->
                                                let price = calcEconomyUpgradeCosts player planet
                                                if price <= maxPrice then
                                                    upgradePlanetEconomy player planet cr
                                                else (planet, cr)) player.credits player.planets
    { player with planets = Seq.toList planets; credits = credits }

let buyResearchForHalfOfCredits maxPrice (player: Player) =
    let availableCredits = player.credits / 2
    let (planets, remaining) = Seq.mapFold (fun cr planet ->
                                            let price = calcResearchUpgradeCosts player planet
                                            if price <= maxPrice then
                                                upgradePlanetResearch player planet cr
                                            else (planet, cr)) availableCredits player.planets
    let newCredits = player.credits - (availableCredits - remaining)
    { player with planets = Seq.toList planets; credits = newCredits }

let buildWorldBuilder player =
    let folder built planet =
        if not built && not planet.worldBuilder then
            ({ planet with worldBuilder = true }, true)
        else (planet, built)
    
    if player.credits > worldBuilderPrice then
        let (planets, built) = List.mapFold folder false player.planets
        if built then
            { player with planets = planets; credits = player.credits - worldBuilderPrice }
        else player
    else player

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
    seq {
        match tickInfo with
            | Turn _ -> if player.credits > worldBuilderPrice then yield buildWorldBuilder else ()
            | _ when isLastTickBeforeTurn tickInfo -> yield upgradeEconomy
            | _ -> ()
    }


let normalBot tickInfo player =
    seq {
        match tickInfo with
            | Turn _ -> yield (buyResearchForHalfOfCredits 100)
            | _ when isLastTickBeforeTurn tickInfo -> yield (upgradeEconomyUntilPrice 60)
            | _ -> ()
    }

let writeToCsv entries (name: string) =
    use writer = new StreamWriter(name)
    use csv = new CsvWriter(writer, System.Globalization.CultureInfo.InvariantCulture)
    csv.WriteRecords(entries)

let diagram projX projY entries name =
    let xvals = Seq.map projX entries
    let yvals = Seq.map projY entries
    Scatter(x = xvals, y = yvals, name = name)
    
let genPlanet wb = {
    terraform = terraformingStart
    economy = economyStart
    worldBuilder = wb
    research = researchStart
}

[<EntryPoint>]
let main argv =    
    let player = {
        terraformingLevel = 1
        researchPoints = 0
        credits = 4000
        planets = Seq.init 40 (fun _ -> genPlanet false) |> Seq.toList
    }
    let layout = Layout(title = "Economy", xaxis = Xaxis(title = "Ticks"), yaxis = Yaxis(title = "Economy"))
    let snapshot player tickInfo =
        match tickInfo with
            | Turn (tick, _) ->
                let economy = totalEconomy player
                let averagePrice = player.planets |> Seq.map (fun p -> calcEconomyUpgradeCosts player p |> float) |> Seq.average |> int
                let data = { economy = economy; averageEconomyUpgradePrice = averagePrice; science = totalResearch player; terraformingLevel = player.terraformingLevel }
                Some({ tick = tick; data = data })
            | _ -> None
            
    let logs1 = simulate player turns snapshot worldbuilderBot
    let logs2 = simulate player turns snapshot normalBot
    let diagramTicks = diagram (fun (e: Entry<_>) -> e.tick)
    [
        diagramTicks (fun e -> e.data.economy) logs1 "Economy (with WB)"
        diagramTicks (fun e -> e.data.economy) logs2 "Economy (without WB)"
    ] |> Chart.Plot |> Chart.WithLayout layout |> Chart.Show
    0