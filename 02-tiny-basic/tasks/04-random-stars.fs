// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression
  | Empty

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random
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

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

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
    
    | "||" ->
        let evaluatedArgs = List.map(fun expr -> evalExpression(expr, state)) expr_list
        match evaluatedArgs with 
        | [BoolValue lhs; BoolValue rhs;] -> BoolValue(lhs || rhs)
        | _ -> failwith "Invalid arguments for || function"

    | "<" ->
        let evaluatedArgs = List.map(fun expr -> evalExpression(expr, state)) expr_list
        match evaluatedArgs with 
        | [NumberValue lhs; NumberValue rhs;] -> BoolValue(lhs < rhs)
        | _ -> failwith "Invalid arguments for < function"

    | ">" ->
        let evaluatedArgs = List.map(fun expr -> evalExpression(expr, state)) expr_list
        match evaluatedArgs with 
        | [NumberValue lhs; NumberValue rhs;] -> BoolValue(lhs > rhs)
        | _ -> failwith "Invalid arguments for > function"
    
    | "RND" ->
        let evaluatedArgs = List.map(fun expr -> evalExpression(expr, state)) expr_list
        match evaluatedArgs with 
        | [NumberValue max]-> NumberValue(state.Random.Next(max))
        | _ -> failwith "Invalid arguments for RND function"

    | _ -> failwith $"Unsupported function {name}"

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
  | Goto (targetLine: int) ->
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
  
  // TODO: Implement two commands for screen manipulation
  | Clear -> 
    System.Console.Clear()
    runNextLine state line
  
  | Poke (x_expr, y_expr, val_expr) ->
    let x_val = evalExpression(x_expr, state)
    let y_val = evalExpression(y_expr, state)
    let value = evalExpression(val_expr, state)

    match x_val, y_val, value with
      | NumberValue(x), NumberValue(y), StringValue(v) -> 
          System.Console.SetCursorPosition(x,y)
          System.Console.Write(v)
          runNextLine state line
      | _ -> failwith "Poke expects x:number, y:number, value:string arguments"
  
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

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random()}

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num 60], "RND" @ [num 20], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num 60], "RND" @ [num 20], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
