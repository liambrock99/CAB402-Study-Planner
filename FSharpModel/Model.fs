﻿module StudyPlannerModel

open QUT

// Functions dealing with unit lists ...

// Loads unit information about all QUT units from a resource file
let private unitList : Map<UnitCode,UnitInfo> = 
    Parser.parseUnitData CourseData.Properties.Resources.units

// Lookup the given unit code in the unitList 
let lookup (code:UnitCode) : UnitInfo =
    Map.find code unitList

// Functions dealing with semester sequences ...

// The semester prior to the given semester
// e.g previousSemester 2020/2 = 2020/1
//     previousSemester 2020/1 = 2019/S
//     previousSemester 2020/S = 2020/2 
let previousSemester (semester:Semester) =
    match semester with
    | { year = yearVal; offering = Semester1 } -> { year = yearVal - 1; offering = Summer }
    | { year = yearVal; offering = Semester2 } -> { year = yearVal; offering = Semester1 }
    | { year = yearVal; offering = Summer } -> { year = yearVal; offering = Semester2 }

// The semester after to the given semester
// e.g nextSemester 2020/1 = 2020/2
//     nextSemester 2020/2 = 2020/S
//     nextSemester 2020/S = 2021/1
let nextSemester (semester:Semester) =
    match semester with
    | { year = yearVal; offering = Semester1 } -> { year = yearVal; offering = Semester2 }
    | { year = yearVal; offering = Semester2 } -> { year = yearVal; offering = Summer }
    | { year = yearVal; offering = Summer } -> { year = yearVal + 1; offering = Semester1 }

// Returns a sequence of consecutive semesters starting from the first semester and ending at the last semester.
// E.g. SemesterSequence 2019/2 2021/1 would return the sequence 2019/2, 2019/S, 2020/1, 2020/2, 2020/S, 2021/1.
let rec SemesterSequence (firstSemester: Semester) (lastSemester: Semester): seq<Semester> =
    seq {
        if firstSemester <= lastSemester then
            yield firstSemester
            yield! SemesterSequence (nextSemester firstSemester) lastSemester
    }





// Functions dealing with prerequisites ...

// True if and only if the prerequisites have been met based on units in the study 
// plan taken in an earlier semester (based on the before function)
let rec private satisfied (prereq:Prereq) (plannedUnits:StudyPlan) (before: Semester->bool) : bool =
    match prereq with
    | And seq ->
        Seq.map (fun prereq -> satisfied prereq plannedUnits before) seq
        |> Seq.reduce (&&)
    | Or seq -> 
        Seq.map (fun prereq -> satisfied prereq plannedUnits before) seq
        |> Seq.reduce (||)
    | Unit unit ->
        plannedUnits
        |> Seq.exists (fun unitInPlan -> unitInPlan.code = unit && before unitInPlan.semester) 
    | CreditPoints cp ->
        plannedUnits
        |> Seq.filter (fun unitInPlan -> before unitInPlan.semester)
        |> Seq.map (fun unitInPlan -> lookup(unitInPlan.code).creditpoints)
        |> Seq.sum >= cp
    | Nil -> 
        true





 // Functions used for determining when units can be studied ...

 // True if and only if the unit with the specified unit code is offered in the specified semester
let isOffered (unitCode:UnitCode) (semester:Semester) : bool =
    let unitInfo = lookup unitCode
    let offered = unitInfo.offered
    let offering = semester.offering
    Set.contains offering offered


// True if and only if the specified unit can be studied in the specified semester based on the specified study plan.
// Requires that the unit is offered in that semester and that prerequistes are meet by units studied before that semester 
let isLegalIn (unitCode:UnitCode) (semester:Semester) (plannedUnits:StudyPlan) : bool =
    let unitInfo = lookup unitCode
    let prereq = unitInfo.prereq
    let before = fun sem -> sem < semester
    isOffered unitCode semester && satisfied prereq plannedUnits before

// True if and only if the specified unit can be added to the study plan in that semester.
// Requires that the number of units currently studied in that semester is less than four and that it is legal in that semester
let isEnrollableIn (unitCode:UnitCode) (semester:Semester) (plannedUnits:StudyPlan) : bool =
    let numScheduled =
        Seq.filter (fun unitInPlan -> semester = unitInPlan.semester) plannedUnits
        |> Seq.length
    numScheduled < 4 && isLegalIn unitCode semester plannedUnits

// True if and only if the unit can be legally added to the study plan (in some semester) 
let isEnrollable (unitCode:UnitCode) (plannedUnits:StudyPlan) : bool =
    let unitInfo = lookup unitCode
    let prereq = unitInfo.prereq
    let before = fun sem -> true
    satisfied prereq plannedUnits before

// True if and only if the all of the units in the study plan are legally scheduled
let isLegalPlan (plan: StudyPlan): bool =
    plan |> Seq.forall (fun (unitInPlan:UnitInPlan) -> isLegalIn unitInPlan.code unitInPlan.semester plan)




// Functions returning various information about units ...

// Returns all of the unit codes that are mentioned anywhere in the prerequisites of the specified unit
let UnitPrereqs (unitCode:UnitCode) : seq<UnitCode> = 
    let unitInfo = lookup unitCode
    let rec getUnitPrereqs prereq  =
        seq {
            match prereq with
            | And seq 
            | Or seq ->
                yield! Seq.map getUnitPrereqs seq
                       |> Seq.concat
            | Unit unit -> 
                yield unit
            | CreditPoints _ 
            | Nil ->
                yield! Seq.empty
        }
    getUnitPrereqs unitInfo.prereq
  

// The title of the specified unit
// e.g. getUnitTitle("CAB402") = "Programming Paradigms" 
let getUnitTitle (unitCode:UnitCode) : string =
    let unitInfo = lookup unitCode
    unitCode + " " + unitInfo.title
    
   
// The prerequisites of the specified unit as a string
// e.g. getPrereq("CAB402") = "Prereqs: (CAB201 or ITD121) and CAB203"
// e.g. getPrereq("IFB104") = "Prereqs: Nil"
let getPrereq (unitCode:UnitCode) : string =
    let unitInfo = lookup unitCode 
    let prereqString = unitInfo.prereqString
    if prereqString = "" then "Prereqs: Nil" else "Prereqs: " + prereqString
    

// The semesters that the specified unit is offered in as a string
// e.g. displayOffered("CAB201") = "semester 1 or 2"
// e.g. displayOffered("CAB402") = "semester 1"
let displayOffered (unitCode:UnitCode) : string =
    let unitInfo = lookup unitCode
    let offered = unitInfo.offered
    let string = 
        offered
        |> Set.toSeq // Convert to seq because it returns and ordered view which is helpful
        |> Seq.map (fun semester ->
            match semester with
            | Semester1 -> "1"
            | Semester2 -> "2"
            | Summer -> "summer"
        )
        |> String.concat " or "
    "semester " + string
 
  
        
// The specified semester as a string (format: year/semester)
// e.g. display(currentSemester) = "2020/1"
let display (sem:Semester) : string = 
    match sem with
    | { year = yearVal; offering = Semester1 } -> string yearVal + "/1"
    | { year = yearVal; offering = Semester2 } -> string yearVal + "/2"
    | { year = yearVal; offering = Summer } -> string yearVal + "/S"