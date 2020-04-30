module FSharpSchedulingWizard

open QUT
open StudyPlannerModel
open BoundsOptimizer

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
     if Seq.isEmpty remainingUnits then 
        Some plannedUnits
     else
         let possibleUnit = 
             remainingUnits
             |> Seq.tryFind (fun unit -> 
                unit.possibleSemesters
                |> Seq.exists (fun semester -> isEnrollableIn unit.code semester plannedUnits)
             ) 
         match possibleUnit with
         | Some someUnit ->
             let newRemaining = List.filter (fun unit -> unit <> someUnit) remainingUnits
             let possiblePlans = 
                someUnit.possibleSemesters
                |> Seq.filter (fun semester -> isEnrollableIn someUnit.code semester plannedUnits)
                |> Seq.map (fun semester ->
                    seq {
                        yield! plannedUnits
                        yield { code = someUnit.code; studyArea = someUnit.studyArea; semester = semester}
                    }
                    |> scheduleRemaining newRemaining
                ) 
             possiblePlans     
             |> Seq.tryFind Option.isSome
             |> Option.flatten
         | None 
            -> Option.None

// Assuming that study commences in the given first semester and that units are only studied 
// in semester 1 or semester 2, returns the earliest possible semester by which all units in
// the study plan could be completed, assuming at most 4 units per semester.
let private bestAchievable (firstSemester:Semester) (plan:StudyPlan) : Semester =
     let numScheduled = Seq.length plan
     let numSemesters =
         let remainder = numScheduled % 4
         let quotient = numScheduled / 4
         if remainder = 0 then 
            quotient - 1
         else    
            quotient
     let rec getBestAchievable numSemesters startingSemester =
        if numSemesters = 0 then 
            startingSemester
        else 
            let next = nextSemester startingSemester
            if next.offering = Summer then
                // Ignore summer semesters    
                next |> getBestAchievable numSemesters
            else
                next |> getBestAchievable (numSemesters - 1)
     getBestAchievable numSemesters firstSemester   


// Returns the last semester in which units will be studied in the study plan
let lastSemester (plan: StudyPlan): Semester =
     plan |> Seq.map (fun unitInPlan -> unitInPlan.semester) |> Seq.max
     

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
        seq {
            if targetGraduation >= bestPossible then
                let boundPlan = boundUnitsInPlan plan first targetGraduation
                if allBoundsFeasible boundPlan then
                    let betterPlan = scheduleRemaining boundPlan Seq.empty
                    match betterPlan with
                    | Some somePlan ->
                        yield somePlan
                        yield! TryToCompleteBy (lastSemester somePlan |> previousSemester)
                    | None -> 
                        yield! Seq.empty
        }
    TryToCompleteBy (previousSemester last)
