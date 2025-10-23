// ----------------------------------------------------------------------------
// 01 - Add GOTO and better PRINT for infinite loop fun!
// ----------------------------------------------------------------------------

// NOTE: You can run this using 'dotnet run' from the terminal. 
// If you want to run code in a different file, you will need to change
// the 'tinybasic.fsproj' file (which references this source file now).

// NOTE: F# code in projects is generally organized using namespaces and modules.
// Here, we declare module name for the source code in this file.
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Empty

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value : Unit = 
  match value with
  | StringValue str-> 
    printfn "%s" str

  | _ -> failwith "Not a valid value to print"

let getLine state line =
  match state.Program |> List.tryFind( fun (next_line, _) -> next_line > line) with
  | Some (next_line, cmd) -> (next_line, cmd)
  | None -> (-1, Empty)

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression (expr: Expression) : Value =  
  match expr with 
  | Const c -> c 

let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(expr) ->
      let value = evalExpression expr
      printValue value 
      runNextLine state line
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Goto targetLine ->
      let adjustedLine = targetLine - 1
      let (new_line, new_cmd) = getLine state adjustedLine
      runCommand state (new_line, new_cmd)
and runNextLine state line = 
  let (next_line, next_cmd) = getLine state line
  match next_line with 
  | -1 -> ignore
  | _ -> runCommand state (next_line,  next_cmd)
  

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  { Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) ] }

runCommand helloOnce (-1, Run) |> ignore

let helloInf = 
  { Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) 
      20, Goto 10 ] }

runCommand helloInf (-1, Run) |> ignore

