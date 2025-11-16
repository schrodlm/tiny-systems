// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck (v:string) (ty:Type) = 
  match ty with 
    | TyVariable name -> name = v
    | TyBool -> false
    | TyNumber -> false 
    | TyList inner-> occursCheck v inner
    | TyFunction (arg, ret) -> occursCheck v ret || occursCheck v arg
    | TyTuple(t1, t2) -> failwith "TODO"

let rec substType (subst:Map<_, _>) t1 = 
  // TODO: Add case for 'TyFunction' (need to substitute in both nested types)
  match t1 with
  | TyVariable name -> 
    match Map.tryFind name subst with
    | Some t -> substType subst t
    | None -> t1
  | TyList inner -> TyList (substType subst inner)
  | TyFunction (arg, ret) -> 
    TyFunction(substType subst arg, substType subst ret)
  | TyTuple(t1,t2) -> failwith"TODO"
  | _ -> t1

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  cs |> List.map(fun(lhs, rhs) -> substType subst lhs, substType subst rhs)
 
let rec solve cs =
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList inner_lhs, TyList inner_rhs)::cs -> solve ((inner_lhs, inner_rhs)::cs)
  | (TyVariable v1, TyVariable v2)::cs when v1=v2 -> solve cs
  | (TyFunction(arg1, ret1), TyFunction(arg2, ret2))::cs -> 
    solve((arg1,arg2)::(ret1,ret2)::cs)
  | (TyTuple(t1, t2), TyTuple(t3,t4))::cs ->
    failwith "TODO"
  | (TyVariable v, t)::cs
  | (t, TyVariable v)::cs ->
    if occursCheck v t then
      failwith "Cycle occurs in constrains"
    else 
      let subst = Map [(v,t)]
      let new_cs = substConstrs subst cs
      let rest_substs = solve new_cs
      (v,t)::rest_substs
  | _ -> failwith "Can't solve contraints"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      TyNumber, []

  | Binary("+", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      let (t1: Type), (c1: (Type*Type) list) = generate ctx e1
      let (t2: Type), (c2: (Type*Type) list) = generate ctx e2

      TyBool, c1 @ c2 @ [t1, TyNumber;t2, TyNumber]

  | Binary("*", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]


  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v -> 
      match Map.tryFind v ctx with
        | Some t -> t, []
        | None -> failwith "Not a defined type"

  | If(econd, etrue, efalse) ->
      let (t1: Type), (c1: (Type*Type) list) = generate ctx econd
      let (t2: Type), (c2: (Type*Type) list) = generate ctx etrue
      let (t3: Type), (c3: (Type*Type) list) = generate ctx efalse

      t2, c1 @ c2 @ c3 @ [t1, TyBool;t2, t3];

  | Let(v, e1, e2) ->
      let (t1: Type), (c1: (Type*Type) list) = generate ctx e1
      let new_ctx = Map.add v t1 ctx
    
      let (t2: Type), (c2: (Type*Type) list) = generate new_ctx e2

      t2, c1 @ c2 
  
  | Lambda(v, e) ->
      let targ = newTyVariable()
      let new_ctx = Map.add v targ ctx
      let t, c = generate new_ctx e
      TyFunction(targ, t), c

  | Application(e1, e2) -> 
      //Examples:
        // Application(Variable("f"), Constant(1))
  
        //  Application(
        //   Lambda("x", Binary("*", Variable("x"), Constant(2)))
        //   Constant(21),
        // )

      let (t1: Type), (c1: (Type*Type) list) = generate ctx e1
      let (t2: Type), (c2: (Type*Type) list) = generate ctx e2
      
      let t_ret = newTyVariable()
      
      // Add constraint that e1 must be a function from t2 to t_ret
      t_ret , c1 @ c2 @ [t1, TyFunction(t2,t_ret)]

  | Tuple(e1, e2) ->
      // TODO: Easy. The returned type is composed of the types of 'e1' and 'e2'.
      failwith "TODO"

  | TupleGet(b, e) ->
      // TODO: Trickier. The type of 'e' is some tuple, but we do not know what.
      // We need to generate two new type variables and a constraint.
      failwith "TODO"

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
