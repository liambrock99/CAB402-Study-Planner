// Learn more about F# at http://fsharp.org

open StudyPlannerModel
open BoundsOptimizer
open QUT

[<EntryPoint>]
let main argv =

    let studyPlan = seq {
        yield { code = "IFB104"; studyArea = "IN01-COREUNITS"; semester = { year = 2020; offering = Semester1 }}
        yield { code = "CAB201"; studyArea = "IN01MJR-COMPSC"; semester = { year = 2020; offering = Semester2 }}
        yield { code = "CAB203"; studyArea = "IN01MJR-COMPSC"; semester = { year = 2021; offering = Semester1 }}
        yield { code = "CAB301"; studyArea = "IN01MJR-COMPSC"; semester = { year = 2022; offering = Semester1 }} 
        yield { code = "IFB105"; studyArea = "IN01-COREUNITS"; semester = { year = 2020; offering = Semester1 }}
        yield { code = "IFB103"; studyArea = "IN01-COREUNITS"; semester = { year = 2020; offering = Semester1 }}        
        yield { code = "CAB302"; studyArea = "IN01-COREUNITS"; semester = { year = 2022; offering = Semester1 }}
        yield { code = "IFB102"; studyArea = "IN01-COREUNITS"; semester = { year = 2020; offering = Semester1 }}      
        yield { code = "IFB295"; studyArea = "IN01-COREUNITS"; semester = { year = 2022; offering = Semester2 }}
        yield { code = "IFB398"; studyArea = "IN01-COREUNITS"; semester = { year = 2023; offering = Semester2 }}    
        yield { code = "CAB202"; studyArea = "IN01-COREUNITS"; semester = { year = 2020; offering = Semester2 }}
        yield { code = "CAB303"; studyArea = "IN01-COREUNITS"; semester = { year = 2020; offering = Semester2 }}    
        yield { code = "IFB399"; studyArea = "IN01-COREUNITS"; semester = { year = 2024; offering = Semester1 }}
        yield { code = "CAB402"; studyArea = "IN01-COREUNITS"; semester = { year = 2022; offering = Semester1 }}    
    }
    
    let boundPlan = boundUnitsInPlan studyPlan { year = 2020; offering = Semester1 } { year = 2023; offering = Semester1 }

    //if Seq.isEmpty remainingUnits then Some plannedUnits
    //else
    //    let possibleUnit =  remainingUnits
    //                        |> Seq.tryFind (fun unit -> Seq.exists (fun semester -> isEnrollableIn unit.code semester plannedUnits) unit.possibleSemesters) 
    //    match possibleUnit with
    //    | Some someUnit ->
    //        let remaining = List.filter (fun unit -> unit <> someUnit) remainingUnits   
    //        let possiblePlans = 
    //           someUnit.possibleSemesters
    //           |> Seq.filter (fun semester -> isEnrollableIn someUnit.code semester plannedUnits)
    //           |> Seq.map (fun semester -> seq { yield! plannedUnits; 
    //                                             yield { code = someUnit.code; studyArea = someUnit.studyArea; semester = semester } })
    //        possiblePlans
    //        |> Seq.map (fun plan -> scheduleRemaining remaining plan)
    //        |> Seq.tryFind (fun plan -> Option.isSome plan)
    //        |> Option.flatten
    //    | None -> Option.None

    let rec scheduleRemaining (remainingUnits:BoundPlan) (plannedUnits:StudyPlan) : StudyPlan option =
        if Seq.isEmpty remainingUnits then Some plannedUnits
        else
            let possibleUnit = 
                remainingUnits
                |> Seq.tryFind (fun unit -> Seq.exists (fun semester -> isEnrollableIn unit.code semester plannedUnits) unit.possibleSemesters) 
            match possibleUnit with
            | Some someUnit ->
                someUnit.possibleSemesters
                |> Seq.filter (fun semester -> isEnrollableIn someUnit.code semester plannedUnits)
                |> Seq.map (fun semester ->
                    let newRemaining = List.filter (fun unit -> unit <> someUnit) remainingUnits
                    let newPlan = 
                        seq {
                            yield! plannedUnits;
                            yield { code = someUnit.code; studyArea = someUnit.studyArea; semester = semester }
                        }
                    scheduleRemaining newRemaining newPlan
                ) 
                |> Seq.tryFind (fun plan -> Option.isSome plan)
                |> Option.flatten
            | None -> Option.None

    let plan = scheduleRemaining boundPlan Seq.empty
    match plan with
    | Some somePlan -> Seq.iter (printfn "%A") somePlan
    | None -> ()

    0