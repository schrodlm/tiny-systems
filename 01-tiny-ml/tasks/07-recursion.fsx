// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
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
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  // NOTE: A recursive definition. You can think of 
  // 'Let(v, e1, e2)' as 'let rec v = e1 in e2'. 
  | Recursive of string * Expression * Expression

and VariableContext = 
  // NOTE: For recursive calls, we need to add the function
  // being defined to the variable context when defining it.
  // This can be done using 'let rec', but we need to store
  // the variables as lazy values.
  Map<string, Lazy<Value>>

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
      | Some res -> res.Value
      | _ -> failwith ("unbound variable: " + v)

  // NOTE: You have the following two from before
  | Unary(op, e) ->
      // TODO: Implement the case for 'Unary' here!
      match op with
      | "-" -> 
        let v = evaluate ctx e
        match v with
        | ValNum n -> ValNum(-n)
      | _ -> failwith "not implemented"
  | If(cond, tr: Expression, fal) -> 
    let cond_val = evaluate ctx cond
    match cond_val with
    | ValNum 1 ->
      (evaluate ctx tr)
    | ValNum n ->
      (evaluate ctx fal) 
    | _ ->
      failwith "Invalid condition"
  
  | Lambda(v, e) ->
      // TODO: Evaluate a lambda - create a closure value
      ValClosure(v,e , ctx)

  | Application(e1, e2) ->
      // e1(e2) - it is expected that function has at max 1 argument
      //        - to eval more that one argument use currying
      let val1 = evaluate ctx e1
      let val2 = evaluate ctx e2

      match val1 with
      | ValClosure(param, body, closureCtx) ->
          // Create new context with the parameter bound to the argument value
          let newCtx = closureCtx.Add(param, Lazy(val2))
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
        let new_ctx = ctx.Add(v, Lazy(x))
        evaluate new_ctx e1

      |  ValCase(false, x) ->
        let new_ctx = ctx.Add(v, Lazy(x))
        evaluate new_ctx e2
      | _ -> failwith "invalid match expression"

  | Case(b, e) ->
      // TODO: Create a union value.
      ValCase(b, (evaluate ctx e))
  | Recursive(v, e1, e2) ->
      // TODO: Implement recursion for 'let rec v = e1 in e2'.
      // (In reality, this will only work if 'e1' is a function
      // but the case can be implemented without assuming that).

      //What am I getting:
      //1. function name - is basically a variable
      //2. body of the function - 
      //3. Call of the function with provided arguments -> application with the variable name

      //What I am gonna do:
      //1. Save the name of the function as a variable and the body as the contents (can do this because lazy eval)
      //2. 

      failwith "not implemented"

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x -> 
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er = 
  Recursive("factorial", 
    Lambda("x", If(
      Variable("x"),
      Constant(1),
      Binary(
        "*", Variable("x"), 
        Application(Variable("factorial"), 
          Binary("+", Constant(-1), Variable("x")))
      )
    )),  
    Application(Variable "factorial", Constant 5)
  )
evaluate Map.empty er
