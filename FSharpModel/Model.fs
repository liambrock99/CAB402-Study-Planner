module StudyPlannerModel

open QUT

// Functions dealing with unit lists ...

// Loads unit information about all QUT units from a resource file
let private unitList : Map<UnitCode,UnitInfo> = 
    Parser.parseUnitData CourseData.Properties.Resources.units

// Lookup the given unit code in the unitList 
let lookup (code:UnitCode) : UnitInfo =
    unitList.Item(code)

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
        yield firstSemester
        if firstSemester = lastSemester then ()
        else yield! nextSemester firstSemester |> SemesterSequence lastSemester
    }





// Functions dealing with prerequisites ...

// True if and only if the prerequisites have been met based on units in the study 
// plan taken in an earlier semester (based on the before function)
let rec private satisfied (prereq:Prereq) (plannedUnits:StudyPlan) (before: Semester->bool) : bool = 
    // TODO: Fixme (difficulty: 8/10)
    false




 // Functions used for determining when units can be studied ...

 // True if and only if the unit with the specified unit code is offered in the specified semester
let isOffered (unitCode:UnitCode) (semester:Semester) : bool =
    lookup(unitCode).offered |> Seq.contains(semester.offering)


// True if and only if the specified unit can be studied in the specified semester based on the specified study plan.
// Requires that the unit is offered in that semester and that prerequistes are meet by units studied before that semester 
let isLegalIn (unitCode:UnitCode) (semester:Semester) (plannedUnits:StudyPlan) : bool =
    true

// True if and only if the specified unit can be added to the study plan in that semester.
// Requires that the number of units currently studied in that semester is less than four and that it is legal in that semester
let isEnrollableIn (unitCode:UnitCode) (semester:Semester) (plannedUnits:StudyPlan) : bool =
    // TODO: Fixme (difficulty: 2/10)
    true

// True if and only if the unit can be legally added to the study plan (in some semester) 
let isEnrollable (unitCode:UnitCode) (plannedUnits:StudyPlan) : bool =
    // TODO: Fixme (difficulty: 4/10)
    true

// True if and only if the all of the units in the study plan are legally scheduled
let isLegalPlan (plan: StudyPlan): bool =
    // TODO: Fixme (difficulty: 4/10)
    true




// Functions returning various information about units ...

// Returns all of the unit codes that are mentioned anywhere in the prerequisites of the specified unit
let UnitPrereqs (unitCode:UnitCode) : seq<UnitCode> = 
    // TODO: Fixme (difficulty: 6/10)
    Seq.empty

// The title of the specified unit
// e.g. getUnitTitle("CAB402") = "Programming Paradigms" 
let getUnitTitle (unitCode:UnitCode) : string =
    unitCode + " " + lookup(unitCode).title
    
    

// The prerequisites of the specified unit as a string
// e.g. getPrereq("CAB402") = "Prereqs: (CAB201 or ITD121) and CAB203"
// e.g. getPrereq("IFB104") = "Prereqs: Nil"
let getPrereq (unitCode:UnitCode) : string =
    let prereqString = lookup(unitCode).prereqString
    if prereqString = "" then "Prereqs: Nil" else "Prereqs: " + prereqString
    

// The semesters that the specified unit is offered in as a string
// e.g. displayOffered("CAB201") = "semester 1 or 2"
// e.g. displayOffered("CAB402") = "semester 1"
let displayOffered (unitCode:UnitCode) : string =
    let offered = lookup(unitCode).offered |> Set.toList
    match offered with
    | [Semester1] -> "semester 1"
    | [Semester2] -> "semester 2"
    | [Semester1; Semester2] -> "semester 1 or 2"
    | _ -> ""
    

    
// The specified semester as a string (format: year/semester)
// e.g. display(currentSemester) = "2020/1"
let display (sem:Semester) : string = 
    match sem with
    | { year = yearVal; offering = Semester1 } -> string yearVal + "/1"
    | { year = yearVal; offering = Semester2 } -> string yearVal + "/2"
    | { year = yearVal; offering = Summer } -> string yearVal + "/S"