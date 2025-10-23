// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
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
let addLine state (line, cmd) = 
  let updatedProgram = 
    state.Program 
    |> List.filter (fun (existing_line,_) -> existing_line <> line)
    |> fun program -> (line, cmd) :: program
    |> List.sortBy fst
  
  {state with Program = updatedProgram}


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
  | Empty ->
      ignore

and runNextLine state line = 
  let (next_line, next_cmd) = getLine state line
  match next_line with 
  | -1 -> ignore
  | _ -> runCommand state (next_line,  next_cmd)

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput (state : State) (line :option<int>, cmd : Command) : State =
  match line with
  | None -> 
    runCommand state (-1, cmd)
    state 

  | Some ln ->
    addLine state (ln, cmd)



let runInputs (state: State) (cmds: list<option<int> * Command>) : State =
  List.fold (fun accState cmd -> runInput accState cmd) state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
