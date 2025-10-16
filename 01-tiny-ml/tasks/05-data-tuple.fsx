// ----------------------------------------------------------------------------
// 05 - Add a simple data type - tuples
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  // NOTE: A tuple value consisting of two other values.
  // (Think about why we have 'Value' here but 'Expression'
  // in the case of 'ValClosure' above!)
  | ValTuple of Value * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: 'Tuple' represents two-element tuple constructor
  // and 'TupleGet' the destructor (accessing a value)
  // Use 'true' for #1 element, 'false' for #2. This is not
  // particularly descriptive, but it works OK enough.
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

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

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - simple tuple example (using the e#1, e#2 notation for field access)
//   (2*21, 123)#1
//   (2*21, 123)#2
let ed1= 
  TupleGet(true, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed1

let ed2 = 
  TupleGet(false, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed2

// // Data types - trying to get a first element of a value
// // that is not a tuple (This makes no sense and should fail)
// //   (42)#1
// let ed3 = 
//   TupleGet(true, Constant(42))
// evaluate Map.empty ed3
