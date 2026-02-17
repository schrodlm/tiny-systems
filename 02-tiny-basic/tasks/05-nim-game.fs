// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
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
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string 
  | Stop

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value : Unit =
  match value with
  | StringValue str -> printf "%s" str
  | NumberValue n -> printf "%d" n
  | BoolValue b -> if b then printf "true" else printf "false"

let getLine state line =
  match state.Program |> List.tryFind (fun (next_line, _) -> next_line > line) with
  | Some (next_line, cmd) -> (next_line, cmd)
  | None -> (-1, Stop)

let addLine state (line, cmd) =
  let updatedProgram =
    state.Program
    |> List.filter (fun (existing_line, _) -> existing_line <> line)
    |> fun program -> (line, cmd) :: program
    |> List.sortBy fst
  { state with Program = updatedProgram }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression (expr: Expression, state: State) : Value =
  match expr with
  | Const c -> c
  | Function (name, expr_list) ->
    let evaluatedArgs = List.map (fun e -> evalExpression (e, state)) expr_list
    match name with
    | "+" ->
        match evaluatedArgs with
        | [NumberValue lhs; NumberValue rhs] -> NumberValue(lhs + rhs)
        | _ -> failwith "Invalid arguments for +"
    | "-" ->
        match evaluatedArgs with
        | [NumberValue lhs; NumberValue rhs] -> NumberValue(lhs - rhs)
        | _ -> failwith "Invalid arguments for -"
    | "=" ->
        match evaluatedArgs with
        | [NumberValue lhs; NumberValue rhs] -> BoolValue(lhs = rhs)
        | _ -> failwith "Invalid arguments for ="
    | "||" ->
        match evaluatedArgs with
        | [BoolValue lhs; BoolValue rhs] -> BoolValue(lhs || rhs)
        | _ -> failwith "Invalid arguments for ||"
    | "<" -> binaryRelOp (<) evaluatedArgs
    | ">" -> binaryRelOp (>) evaluatedArgs
    | "RND" ->
        match evaluatedArgs with
        | [NumberValue max] -> NumberValue(state.Random.Next(max))
        | _ -> failwith "Invalid arguments for RND"
    | "MIN" ->
        match evaluatedArgs with
        | [NumberValue a; NumberValue b] -> NumberValue(min a b)
        | _ -> failwith "Invalid arguments for MIN"
    | _ -> failwith $"Unsupported function {name}"
  | Variable v ->
    match state.Variables |> Map.tryFind v with
    | Some value -> value
    | None -> failwithf "Variable '%s' not found" v

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first

  | Print(exprs) ->
      for expr in exprs do
        let value = evalExpression(expr, state)
        printValue value
      runNextLine state line

  | Goto(targetLine) ->
      let adjustedLine = targetLine - 1
      let (new_line, new_cmd) = getLine state adjustedLine
      runCommand state (new_line, new_cmd)

  | Assign(name, expr) ->
      let evaluated = evalExpression(expr, state)
      let updated = state.Variables.Add(name, evaluated)
      runNextLine { state with Variables = updated } line

  | If(expr, cmd) ->
      let evaluated = evalExpression(expr, state)
      match evaluated with
      | BoolValue true -> runCommand state (-1, cmd)
      | BoolValue false -> runNextLine state line
      | _ -> failwith "Not valid expression in an if statement"

  | Clear ->
      System.Console.Clear()
      runNextLine state line

  | Poke(x_expr, y_expr, val_expr) ->
      let x_val = evalExpression(x_expr, state)
      let y_val = evalExpression(y_expr, state)
      let value = evalExpression(val_expr, state)
      match x_val, y_val, value with
      | NumberValue x, NumberValue y, StringValue v ->
          System.Console.SetCursorPosition(x, y)
          System.Console.Write(v)
          runNextLine state line
      | _ -> failwith "Poke expects x:number, y:number, value:string arguments"

  | Input(varName) ->
      let rec readNumber () =
        let input = System.Console.ReadLine()
        match System.Int32.TryParse(input) with
        | true, n -> n
        | false, _ -> readNumber ()
      let n = readNumber ()
      let updated = state.Variables.Add(varName, NumberValue n)
      runNextLine { state with Variables = updated } line

  | Stop -> state

and runNextLine state line =
  let (next_line, next_cmd) = getLine state line
  match next_line with
  | -1 -> state
  | _ -> runCommand state (next_line, next_cmd)

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput (state: State) (line: option<int>, cmd: Command) : State =
  match line with
  | None ->
      runCommand state (-1, cmd)
  | Some ln ->
      addLine state (ln, cmd)

let runInputs (state: State) (cmds: list<option<int> * Command>) : State =
  List.fold (fun accState cmd -> runInput accState cmd) state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
