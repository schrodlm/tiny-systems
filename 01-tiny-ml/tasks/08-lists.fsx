// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value
  | ValUnit 

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
  | Recursive of string * Expression * Expression
  | Unit 

and VariableContext = 
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
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _ ->
        failwith "Cannot use closure in a binary operation"

  | Variable(v) ->
      match ctx.TryFind v with
      | Some lazyValue -> 
          let result = lazyValue.Value
          result
      | None -> 
          failwith ("unbound variable: " + v)

  | Unary(op, e) ->
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
      ValClosure(v,e , ctx)

  | Application(e1, e2) ->
      let val1: Value = evaluate ctx e1
      let val2: Value = evaluate ctx e2
      match val1 with
      | ValClosure(param: string, body: Expression, closureCtx: VariableContext) ->
          let newCtx: Map<string,Lazy<Value>> = closureCtx.Add(param, Lazy(val2))
          evaluate newCtx body
      | _ -> failwith "invalid application"
  | Let(v, e1, e2) ->
    evaluate ctx (Application(Lambda(v, e2), e1))

  | Tuple(e1, e2) ->
    let val1 = evaluate ctx e1
    let val2 = evaluate ctx e2
    ValTuple(val1, val2)


  | TupleGet(b, e) ->
    let v = (evaluate ctx e)
    match v with
    | ValTuple(v1, v2) ->
        // Extract first or second element based on boolean
        if b then v1 else v2
    | _ -> failwith "TupleGet requires a tuple value"

  | Match(e, v, e1, e2) ->
      match evaluate ctx e with
      | ValCase(true, x) -> 
        let new_ctx = ctx.Add(v, Lazy(x))
        evaluate new_ctx e1

      |  ValCase(false, x) ->
        let new_ctx = ctx.Add(v, Lazy(x))
        evaluate new_ctx e2
      | _ -> failwith "invalid match expression"

  | Case(b, e) ->
      ValCase(b, (evaluate ctx e))
  | Recursive(v: string, e1: Expression, e2: Expression) ->
      let rec lazy_closure: Lazy<Value> = lazy evaluate ctx_with_itself e1 
      and ctx_with_itself = ctx.Add(v, lazy_closure)

      let evaluated: Value = evaluate ctx_with_itself e2
      printfn "Evaluated rec: %A" evaluated
      evaluated

  | Unit -> ValUnit


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Ultimate functional programming - lists and List.map!
// We represent lists as cons cells using tuples, so [1,2,3]
//
// = Case(true, Tuple(Constant(1), Case(true, Tuple(Constant(2), 
//     Case(true, Tuple(Constant(3), Case(false, Unit) ))))))

// Helper function to construct lists, so that we 
// do not need to write them by hand!
let rec makeListExpr l = 
  match l with
  | x::xs -> Case(true, Tuple(x, makeListExpr xs))
  | [] -> Case(false, Unit)

let el = makeListExpr [ for i in 1 .. 5 -> Constant i ]
printfn "List: %A" el

// List.map function in TinyML:
//
//   let rec map = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> Case1(f x#1, (map f) x#2) 
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y * 10) l
//
let em = 
  Recursive("map",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x",
        Case(true, Tuple(
          Application(Variable "f", TupleGet(true, Variable "x")),
          Application(Application(Variable "map", Variable "f"), 
            TupleGet(false, Variable "x"))
        )),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "map", 
      Lambda("y", Binary("*", Variable "y", Constant 10))), el)
  )
evaluate Map.empty em

// TODO: Can you implement 'List.filter' in TinyML too??
// The somewhat silly example removes 3 from the list.
// Add '%' binary operator and you can remove odd/even numbers!
//
//   let rec filter = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> 
//          if f x#1 then Case1(x#1, (filter f) x#2) 
//          else (filter f) x#2
//     | Case2(Unit) -> Case2(Unit))
//   in filter (fun y -> y + (-2)) l
//
let ef = failwith "not implemented"
evaluate Map.empty ef
