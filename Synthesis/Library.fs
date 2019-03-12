module Synthesis

open System.Linq.Expressions

let abelar input = input>12 && input<3097 && input%12 = 0 

let area b h =
    match b<0.0 || h <0.0 with
    |true ->  failwith "Cannot have negative inputs"
    |false -> 0.5 * b * h
    

let zollo input =
    match input<0 with
    |true -> input * (-1)
    |false -> input * 2


let min one two =
    match one>two with
    |true -> two
    |false -> one

let max one two =
    match one>two with
    |true -> one
    |false -> two

let ofTime hours min sec = sec + (min * 60) + (hours * 60 * 60)
    
let toTime input =
    match input<0 with
    |true -> (0,0,0)
    |false -> 
        let hours = input/3600
        let minutes = (input/60)%60 
        let sec = input%60
        (hours,minutes,sec)


let digits num = //does not work with 10 or 0
    let rec counter n c =
        match n = 0 with
        |true -> c 
        |false -> counter (n/10) c+1
    counter num 0


let minmax (one,two,three,four) =
    (min(min one two) (min three four), max(max one two) (max three four))
    
let isLeap year =
    match year>1581 with
    |false -> failwith "Year too small"
    |true ->
        match year%4=0 with 
        |true ->
            match year%100=0 with
            |true -> 
                match year%400=0 with
                |true -> true
                |false -> false
            |false -> true
        |false -> false
        

let month num =
    match num with
    |1  -> ("January", 31)
    |2  -> ("February", 28)
    |3  -> ("March", 31)
    |4  -> ("April", 30)
    |5  -> ("May", 31)
    |6  -> ("June", 30)
    |7  -> ("July", 31)
    |8  -> ("August", 31)
    |9  -> ("September", 30)
    |10 -> ("October", 31)
    |11 -> ("November", 30)
    |12 -> ("December", 31)
    |_ -> failwith "Input is invalid"
        
        
let toBinary n =
    match n<0 with
    |true -> failwith "Number must be greater than 0"
    |false ->
        let rec conv n =
            match n with 
            |0 | 1 -> string n
            |_ -> let ans = string (n % 2)
                  (conv (n / 2)) + ans
        conv n
    

let bizFuzz _ =
    failwith "Not implemented"

let getMonth n = 
    let (st,_) = month n
    st

let monthDay d y =
    match d>0 && d<367 with
    |false -> failwith "Day is out of range"
    |true -> 
        match isLeap y with
        |false ->
            match d=366 with
            |true -> failwith "Day is out of range because it is not a leap year"
            |false ->
                match d<31 with
                |true ->  getMonth 1
                |_ -> "SOMEEVEBER"
        |true -> failwith "Incomplete"
    

    

let coord _ =
    failwith "Not implemented"