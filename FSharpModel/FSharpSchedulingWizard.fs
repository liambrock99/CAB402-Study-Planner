module FSharpSchedulingWizard

open QUT
open StudyPlannerModel

// Functions used for optimizing study plan ...

// The semester that we are currently in
let currentSemester : Semester = 
    // Do not change
    { year = 2020; offering = Semester1 }

// Given a partial plan, try to schedule the remaining units such that all remaining units are legally scheduled 
// (with no more than 4 units per semester) .
// Should return None if it is not possible to schedule the remaining units
// We start by selecting one of the remaining units that can be scheduled in at least one of its possible semesters.
// If none of the remaining units can be scheduled then we fail.
// Otherwise we try scheduling that unit in each of the possible semesters in which it can be legally scheduled. 
// If any of those schedules can be extended into a complete plan then we succeed, otherwise we fail.
let rec private scheduleRemaining (remainingUnits:BoundPlan) (plannedUnits:StudyPlan) : StudyPlan option =
     if Seq.isEmpty remainingUnits then Some plannedUnits
     else
         let possibleUnit =  remainingUnits
                             |> Seq.tryFind (fun unit -> Seq.exists (fun semester -> isEnrollableIn unit.code semester plannedUnits) unit.possibleSemesters) 
         match possibleUnit with
         | Some someUnit ->        
             let  possiblePlans = someUnit.possibleSemesters
                                 |> Seq.filter (fun semester -> isEnrollableIn someUnit.code semester plannedUnits)
                                 |> Seq.map (fun semester -> seq { yield! plannedUnits; 
                                                                   yield { code = someUnit.code; studyArea = someUnit.studyArea; semester = semester } })
             let remaining = List.filter (fun unit -> unit <> someUnit) remainingUnits
             Seq.map (fun plan -> scheduleRemaining remaining plan) possiblePlans
             |> Seq.tryFind (fun plan -> Option.isSome plan)
             |> Option.flatten
         | None -> Option.None

// Assuming that study commences in the given first semester and that units are only studied 
// in semester 1 or semester 2, returns the earliest possible semester by which all units in
// the study plan could be completed, assuming at most 4 units per semester.
let private bestAchievable (firstSemester:Semester) (plan:StudyPlan) : Semester =
     let len = Seq.length plan
     match len with
     | x when x <= 4 -> currentSemester
     | x when x <= 8 -> { year = 2020; offering = Semester2 }
     | x when x <= 12 -> { year = 2020; offering = Summer }
     | x when x <= 16 -> { year = 2021; offering = Semester1 }
     | x when x <= 20 -> { year = 2021; offering = Semester2 }
     | x when x <= 24 -> { year = 2021; offering = Summer }
     | x when x <= 28 -> { year = 2022; offering = Semester1 }
     | x when x <= 32 -> { year = 2022; offering = Summer }
     | _ -> currentSemester
        

// Returns the last semester in which units will be studied in the study plan
let lastSemester (plan: StudyPlan): Semester =
     Seq.map (fun unitInPlan -> unitInPlan.semester) plan |> Seq.max
     

// Returns true if and only if every unit in the plan has at least one possible semester for it to be scheduled
let allBoundsFeasible (bounds:BoundPlan) =
    // do not change  (difficulty: 3/10)
    bounds |> Seq.forall (fun unit -> not (Seq.isEmpty unit.possibleSemesters)) 

// Returns a sequence of progressively better study plans.
// Each successive plan returned finishes in an earlier semester than the previous plan.
// Should return the empty sequence if the plan cannot be improved.
// The earliest semester that we can schedule units in is the current semester.
// Successively better plans are created by specifying progressively tighter target graduation semesters.
// We determine the final semester of the current plan and set our target semester as the semester before that.
// If we succeed in finding a plan that completes by that target semester then we try to improve that plan further, 
// semester by semester until it becomes impossible to improve further.
let TryToImproveSchedule (plan:StudyPlan) : seq<StudyPlan> =
    let first = currentSemester
    let last = lastSemester plan
    let bestPossible = bestAchievable first plan
    let rec TryToCompleteBy (targetGraduation:Semester) =
        Seq.empty
    TryToCompleteBy (previousSemester last)
