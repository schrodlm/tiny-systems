// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Empty

type State = 
  { Program : list<int * Command> 
    Variables: Map<string, Value>
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value : Unit = 
  match value with
  | StringValue str-> 
    printfn "%s" str
  | NumberValue n ->
    printfn "%d" n
  | BoolValue b ->
    if b then printfn "true" else printfn "false"


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

let rec evalExpression (expr: Expression, state: State) : Value =  
  match expr with 
  | Const c -> c 
  | Function (name, expr_list) ->
    match name with 
    | "+" -> 
        let evaluatedArgs = List.map (fun expr -> evalExpression (expr, state)) expr_list
        match evaluatedArgs with 
        | [NumberValue lhs; NumberValue rhs] -> NumberValue(lhs + rhs)
        | _ -> failwith "Invalid arguments for + function"
    
    | "-" -> 
        let evaluatedArgs = List.map (fun expr -> evalExpression (expr, state)) expr_list
        match evaluatedArgs with 
        | [NumberValue lhs; NumberValue rhs] -> NumberValue(lhs - rhs)
        | _ -> failwith "Invalid arguments for - function"

    | "=" -> 
        let evaluatedArgs = List.map (fun expr -> evalExpression (expr, state)) expr_list
        match evaluatedArgs with
        | [NumberValue lhs; NumberValue rhs] -> BoolValue(lhs = rhs)
        | _ -> failwith "Invalid arguments for = function"
    | _ -> failwith "Only binary options implemented"

  | Variable v ->
    match state.Variables |> Map.tryFind v with
    | Some value -> 
    value // Return the variable's value if it exists
    | None -> failwithf "Variable '%s' not found" v // Handle missing variable

let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(expr) ->
      let value = evalExpression(expr,state)
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

  | Assign (name, expr) -> 
    let evaluated = evalExpression (expr, state)
    let updated_context = state.Variables.Add(name, evaluated)
    runNextLine { state with Variables = updated_context } line

  | If (expr, cmd)  -> 
    let evaluated: Value = evalExpression(expr, state)
    match evaluated with
    | BoolValue(true) -> 
      runCommand state (-1, cmd)
    | BoolValue(false) ->
      runNextLine state line
    | _ -> failwith "Not valid expression in an if statement"


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

let empty = { Program = []; Variables = Map.empty} // TODO: Add empty variables to the initial state!

// let testVariables = 
//   [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
//     Some 20, Assign("I", Const(NumberValue 1))
//     Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
//     Some 40, Print(Variable "S") 
//     Some 50, Print(Variable "I") 
//     Some 60, Print(Variable "B")
//     None, Run ]

// // // NOTE: Simpler test program without 'If" (just variables and '=' function) 
// runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// // NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
