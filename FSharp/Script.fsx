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

type Option<'T> = 
    | Some of 'T
    | None

  //  capitolo 6