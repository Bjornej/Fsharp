// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open FSharp


// Define your library scripting code here

let message = "Hello world!"
printfn "%s" message

let numbers = [1..10]
let isOdd(n) = (n%2=1)
let square n = n*n

let squared = 
    numbers
    |> List.filter isOdd
    |> List.map square

type Point = int * int

type Shape = 
    | Rectangle of Point * Point
    | Ellipse of Point * Point
    | Composed of Shape * Shape

let Area(first:Shape) = 
    match first with
    | Rectangle(a,b) -> 2
    | Ellipse(a,b) -> 3
    | Composed(Rectangle(a,b),Rectangle(c,d)) when c < d -> 111
    | Composed(_,_) -> 4


let printCity cityInfo = 
    printfn "population of %s is %d" (fst cityInfo) (snd cityInfo)

let prague = ("Prague", 1188126) 

printCity prague 

let withSecond (f,_) nsnd =  (f,nsnd)

let prague0 = ("Prague", 1188000) 
let prague1 = withSecond prague0 ((snd prague0) + 13195)
printCity prague1 

let rec factorial n = 
    match n with
    | 0 -> 1
    | 1 -> 1
    | n -> n * (factorial (n-1))

let fac2 n = List.fold (*) 1 [1..n]

let ls1 = []
let ls2 = 1::2::3::4::5::[]
let ls3 = [1..5]
let ls4 = 0::ls3

let first list = 
    match list with
    | [] -> 0
    | head::tail -> 1

let rec sumList list = 
    match list with
    | [] -> 0
    | head::tail -> head + sumList tail

let rec aggregateList (f: int -> int -> int) init list =
    match list with
    | [] -> init
    | head::tail ->  f (aggregateList f init tail) head

// capitolo 4

open System

let convertDataRow(str:string) =
    let cells = List.ofSeq (str.Split(','))
    match cells with
    |lbl::num::_ ->
        let numI = Int32.Parse(num)
        (lbl,numI)
    |_ -> failwith "incorrect"

let rec processLines(lines) = 
    match lines with
    | [] -> []
    | str::tail ->
        let row = convertDataRow(str)
        let rest = processLines(tail)
        row::rest

let processLines2(lines) = 
    lines
    |> List.map convertDataRow

let rec countSum(rows) =
    match rows with 
    |[] -> 0
    |(_,n) ::tail ->
        n + countSum(tail)

type Schedule =
    | Never
    | Once of DateTime
    | Repeatedly of DateTime * TimeSpan

let tomorrow = DateTime.Now.AddDays(1.0)

let noon = new DateTime(2014,1,1,12,0,0)
let span = new TimeSpan(24,0,0)
let schedule1=Never
let schedule2 = Once(tomorrow)
let schedule3 = Repeatedly(noon,span)

let nextWeek(schedule) =
    let isNextWeek dt =
        dt > DateTime.Now && dt < DateTime.Now.AddDays(7.0)

    match schedule with
    | Never -> false
    | Once(dt) -> isNextWeek dt
    | Repeatedly(dt,ts) -> false

let readInput() =      
    let s = Console.ReadLine()      
    let (succ, num) = Int32.TryParse(s)      
    if (succ) then
        Some(num)                       
    else          
        None;;

let testInput() = 
    let inp = readInput()
    match inp with
    | Some(v) -> v
    | None -> 0

//type Option<'T> = 
//    | Some of 'T
//    | None

  //  capitolo 6

let (+>) a b = a + "\n>>" + b

printfn "%s" ("Hello world!" +>
              "How are you today?" +>
              "I'm fine!")

let a = List.head(List.rev [1 .. 5])
let a2 = [1 .. 5] |> List.rev |> List.head 

let mapSchedule f sch =
    match sch with
    |Never -> Never
    |Once(dt) -> Once(f(dt))
    |Repeatedly(dt,ts) -> Repeatedly(f(dt),ts)

let sumRead = 
    match (readInput()) with 
    | None -> None 
    | Some(first) -> readInput() |> Option.map (fun second -> first + second )

//capitolo 7

type Rect = {
    Left : float32
    Top:float32
    Width:float32
    Height:float32
}

let moveUp (a:Rect) = 
    let rc2 = {a with Left = a.Left + 100.0f}
    rc2

open System.Drawing

let deflate(rect,w,h) = 
    {Left = rect.Top + w
     Top = rect.Left+h
     Width = rect.Width - (2.0f * w)
     Height = rect.Height - (2.0f * h) }

let toRectangleF(rc) = 
    RectangleF(rc.Left,rc.Top,rc.Width,rc.Height)


type Client = 
    {
        Name:string
        Income:int
        YearsInJob:int
        UsesCreditCards:bool
        CriminalRecord:bool
    }

let john = { 
    Name = "John Doe"
    Income = 40000
    YearsInJob = 1        
    UsesCreditCards = true
    CriminalRecord = false 
}

let tests = 
    [
     (fun cl -> cl.CriminalRecord=true);
     (fun cl -> cl.Income < 30000);
     (fun cl -> cl.UsesCreditCards = false);
     (fun cl -> cl.YearsInJob < 2)
    ]

let testClient(client) =
    let issues = tests |> List.filter ((|>) client)
    let suitable = issues.Length <= 1
    printfn "Client: %s\nOffer a loan: %s (issues = %d)" client.Name (if (suitable) then "YES" else "NO") issues.Length


type QueryInfo = 
    { 
        Title    : string      
        Check    : Client -> bool   
        Positive : Decision      
        Negative : Decision 
    }     
       
 and Decision =                   
   | Result of string      
   | Query  of QueryInfo     


let rec testTree(client,tree) = 
    match tree with
        | Result(msg) -> printf "Result : %s" msg
        | Query(qi) -> if(qi.Check(client)) then 
                            printf "yes" 
                            testTree(client, qi.Positive)
                        else 
                            printf "no"
                            testTree( client, qi.Negative)
                       
//chapter 9

type Rect2 = 
    { Left: float32
      Top:float32
      Height:float32
      Width : float32}

    /// Test commento
    /// prova
    member x.Deflate(w,h) =
        { Left = x.Top + w
          Top = x.Left + h
          Width = x.Width - w
          Height = x.Height - h}

    member x.ToRectangleF() =
        RectangleF(x.Left,x.Top,x.Width,x.Height)


type ClientTest = 
    abstract Check : Client -> bool
    abstract Report: Client -> unit

 let testCriminal =      
    { new ClientTest with    
         member x.Check(cl) = cl.CriminalRecord = true   
         member x.Report(cl) =
            printfn "'%s' has a criminal record!" cl.Name }



type ClientInfo(name, income, years) =
    let q = income / 5000 * years
    do printfn "Creating client '%s'" name

    member x.Name = name
    member x.Income = income
    member x.Years = years 
    member x.Report() =
        printfn "Client: %s, q=%d" name q   


type CoefficientTest(income, years, min) = 
   let coeff cl =
        ((float cl.Income)*income + (float cl.YearsInJob)*years)
   let report cl =
        printfn "Coefficient %f is less than %f." (coeff cl) min   
      
   member x.PrintInfo() =
        printfn "income*%f + years*%f > %f" income years min  

   interface ClientTest with 
        member x.Report cl = report cl
        member x.Check cl = (coeff cl) < min 

//capitolo 10

type IntTree = 
    | Leaf of int
    | Node of IntTree * IntTree

let rec sumTree(tree) =
    match tree with
    | Leaf(a) -> a
    | Node(a,b) -> sumTree(a) + sumTree(b)

let rec sumTreeCont tree cont =
    match tree with
    | Leaf(a) -> cont(a)
    | Node(a,b) -> sumTreeCont a (fun n -> 
                        sumTreeCont b (fun m -> cont(n+m)))

type InfiniteInts =    
    | LazyCell of int * Lazy<InfiniteInts> 

let rec infiniteNumbers(num) = LazyCell(num, lazy infiniteNumbers(num+1))

let next(LazyCell(hd,tail)) = tail.Force()


let pos = infiniteNumbers(0) |> next |> next |> next |> next |> next 

//chapter 12