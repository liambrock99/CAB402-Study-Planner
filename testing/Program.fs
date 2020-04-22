// Learn more about F# at http://fsharp.org

open StudyPlannerModel
open BoundsOptimizer
open QUT

[<EntryPoint>]
let main argv =

    let studyPlan = seq {
        yield { code = "IFB101"; studyArea = "IN01-COREUNITS"; semester = { year = 2020; offering = Semester1 }}
        yield { code = "IFB102"; studyArea = "IN01-COREUNITS"; semester = { year = 2022; offering = Summer }}
    }
    
    //let numSemesters numUnits =
    //    if numUnits <= 4 then 0 else
    //        let remainder = numUnits % 4
    //        let quotient = numUnits / 4
    //        match remainder with 
    //        | 0 -> quotient - 1
    //        | _ -> quotient

    0