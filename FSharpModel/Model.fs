module StudyPlannerModel

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
        if firstSemester <> lastSemester 
        then
            yield firstSemester
            yield! SemesterSequence (nextSemester firstSemester) lastSemester
        else 
            yield lastSemester
    }





// Functions dealing with prerequisites ...

// True if and only if the prerequisites have been met based on units in the study 
// plan taken in an earlier semester (based on the before function)
let rec private satisfied (prereq:Prereq) (plannedUnits:StudyPlan) (before: Semester->bool) : bool =
    match prereq with
    | And _and ->
        Seq.map (fun prereq -> satisfied prereq plannedUnits before) _and
        |> Seq.reduce (&&)
    | Or _or -> 
        Seq.map (fun prereq -> satisfied prereq plannedUnits before) _or
        |> Seq.reduce (||)
    | Unit unit -> 
        Seq.exists (fun (unitInPlan:UnitInPlan) -> unitInPlan.code = unit && before unitInPlan.semester) plannedUnits
    | CreditPoints cp ->
        let numBefore = Seq.filter (fun (unitInPlan:UnitInPlan) -> before unitInPlan.semester) plannedUnits
                        |> Seq.length
        numBefore * 12 >= cp
    | Nil -> true





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
    let numInSem = Seq.filter (fun (unitInPlan:UnitInPlan) -> semester = unitInPlan.semester) plannedUnits
                   |> Seq.length
    numInSem < 4 && isLegalIn unitCode semester plannedUnits

// True if and only if the unit can be legally added to the study plan (in some semester) 
let isEnrollable (unitCode:UnitCode) (plannedUnits:StudyPlan) : bool =
    let unitInfo = lookup unitCode
    let prereq = unitInfo.prereq
    let before = fun sem -> true
    satisfied prereq plannedUnits before

// True if and only if the all of the units in the study plan are legally scheduled
let isLegalPlan (plan: StudyPlan): bool =
    Seq.forall (fun (unitInPlan:UnitInPlan) -> isLegalIn unitInPlan.code unitInPlan.semester plan) plan




// Functions returning various information about units ...

// Helper for UnitPrereqs
let rec getUnitPrereqs (prereq:Prereq) : seq<UnitCode> =
    seq {
        match prereq with
        | And prereqSeq | Or prereqSeq ->
            yield! Seq.map getUnitPrereqs prereqSeq
                   |> Seq.concat
        | Unit unit -> yield unit
        | _ -> ()
    }

// Returns all of the unit codes that are mentioned anywhere in the prerequisites of the specified unit
let UnitPrereqs (unitCode:UnitCode) : seq<UnitCode> = 
    let unitInfo = lookup unitCode
    let prereq = unitInfo.prereq
    getUnitPrereqs prereq
  

// The title of the specified unit
// e.g. getUnitTitle("CAB402") = "Programming Paradigms" 
let getUnitTitle (unitCode:UnitCode) : string =
    let unitInfo = lookup unitCode
    let title = unitInfo.title
    unitCode + " " + title
    
   
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
    let sem1 = Set.contains Semester1 offered
    let sem2 = Set.contains Semester2 offered
    let summer = Set.contains Summer offered
    let count = Set.count offered
    match count with
    | 3 -> "semester 1 or 2 or summer"
    | 2 when sem1 && sem2 -> "semester 1 or 2"
    | 2 when sem1 && summer -> "semester 1 or summer"
    | 2 when sem2 && summer -> "semester 2 or summer"
    | 1 when sem1 -> "semester 1"
    | 1 when sem2 -> "semester 2"
    | 1 when summer -> "summer"
    | _ -> ""
  
        
// The specified semester as a string (format: year/semester)
// e.g. display(currentSemester) = "2020/1"
let display (sem:Semester) : string = 
    match sem with
    | { year = yearVal; offering = Semester1 } -> string yearVal + "/1"
    | { year = yearVal; offering = Semester2 } -> string yearVal + "/2"
    | { year = yearVal; offering = Summer } -> string yearVal + "/S"