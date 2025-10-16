// ----------------------------------------------------------------------------
// 06 - Add more data types - unions
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  // NOTE: Value representing a union case. Again, we use 'bool':
  // 'true' for 'Case1' and 'false' for 'Case2'
  | ValCase of bool * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  // NOTE: 'Case' represents creating a union value and 'Match' pattern 
  // matching. You can read 'Match(e, v, e1, e2)' as F# pattern matching 
  // of the form: 'match e with v -> e1 | v -> e2'
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

and VariableContext = 
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      // TODO: We added 'ValClosure' to 'Value', so this can now fail to 
      // match (if you call binary operator with functions as arguments).
      // Add a catch-all ('_') case and throw an exception using 'failwith'
      // Also do the same for 'Unary' an 'If'!
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _ ->
        failwith "Cannot use closure in a binary operation"

  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)

  // NOTE: You have the following two from before
  | Unary(op, e) -> failwith "implemented in step 2"
  | If(econd, etrue, efalse) -> failwith "implemented in step 2"
  
  | Lambda(v, e) ->
      // TODO: Evaluate a lambda - create a closure value
      ValClosure(v,e , ctx)

  | Application(e1, e2) ->
      // TODO: Evaluate a function application. Recursively
      // evaluate 'e1' and 'e2'; 'e1' must evaluate to a closure.
      // You can then evaluate the closure body.
      let val1 = evaluate ctx e1
      let val2 = evaluate ctx e2

      match val1 with
      | ValClosure(param, body, closureCtx) ->
          // Create new context with the parameter bound to the argument value
          let newCtx = closureCtx.Add(param, val2)
          // Evaluate the body in the new context
          evaluate newCtx body
      | _ -> failwith "invalid application"
  | Let(v, e1, e2) ->
    // TODO: There are two ways to do this! A nice tricky is to 
    // treat 'let' as a syntactic sugar and transform it to the
    // 'desugared' expression and evaluating that :-)
    evaluate ctx (Application(Lambda(v, e2), e1))

  | Tuple(e1, e2) ->
    let val1 = evaluate ctx e1
    let val2 = evaluate ctx e2
    ValTuple(val1, val2)


  | TupleGet(b, e) ->
      // TODO: Access #1 or #2 element of a tuple value.
      // (If the argument is not a tuple, this fails.)
    let v = (evaluate ctx e)
    match v with
    | ValTuple(v1, v2) ->
        // Extract first or second element based on boolean
        if b then v1 else v2
    | _ -> failwith "TupleGet requires a tuple value"

  | Match(e, v, e1, e2) ->
      // TODO: Implement pattern matching. Note you need to
      // assign the right value to the variable of name 'v'!
      match evaluate ctx e with
      | ValCase(true, x) -> 
        let new_ctx = ctx.Add(v, x)
        evaluate new_ctx e1

      |  ValCase(false, x) ->
        let new_ctx = ctx.Add(v, x)
        evaluate new_ctx e2
      | _ -> failwith "invalid match expression"

  | Case(b, e) ->
      // TODO: Create a union value.
      ValCase(b, (evaluate ctx e))

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 =
  Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 = 
  Match(Case(true, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec2

//this case is very useful for list implementation - case1 for cons() and case2 for "list end"
let ec3 = 
  Match(Case(false, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec3
