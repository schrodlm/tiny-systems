// ----------------------------------------------------------------------------
// 03 - Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

// NOTE: Start with some basic expressions from TinyML
// This time, If requires a real Boolean argument and we have
// operators '+' (int -> int -> int) and '=' (int -> int -> bool)
type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck (v: string) (ty: Type) =
  match ty with 
    | TyVariable name -> name = v
    | TyBool -> false
    | TyNumber -> false 
    | TyList inner-> occursCheck v inner 
let rec substType (subst:Map<string, Type>) ty:Type = 
  match ty with
  | TyVariable name -> 
    match Map.tryFind name subst with
    | Some t -> substType subst t
    | None -> ty
  | TyList inner -> TyList (substType subst inner)
  | _ -> ty
let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  cs |> List.map(fun(lhs, rhs) -> substType subst lhs, substType subst rhs)

 
let rec solve cs =
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList inner_lhs, TyList inner_rhs)::cs -> solve ((inner_lhs, inner_rhs)::cs)
  | (TyVariable v1, TyVariable v2)::cs when v1=v2 -> solve cs
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

// Variable context to keep types of declared variables
// (those will typically be TyVariable cases, but don't have to)
type TypingContext = Map<string, Type>

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      // NOTE: If the expression is a constant number, we return
      // its type (number) and generate no further constraints.
      TyNumber, []

  | Binary("+", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      // TODO: Similar to the case for '+' but returns 'TyBool'
      let (t1: Type), (c1: (Type*Type) list) = generate ctx e1
      let (t2: Type), (c2: (Type*Type) list) = generate ctx e2

      TyBool, c1 @ c2 @ [t1, TyNumber;t2, TyNumber]

  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v -> 
      // TODO: Just get the type of the variable from 'ctx' here.
      match Map.tryFind v ctx with
        | Some t -> t, []
        | None -> failwith "Not a defined type"

  | If(econd, etrue, efalse) ->
      // TODO: Call generate recursively on all three sub-expressions,
      // collect all constraints and add a constraint that (i) the type
      // of 'econd' is 'TyBool' and (ii) types of 'etrue' and 'efalse' match.
      let (t1: Type), (c1: (Type*Type) list) = generate ctx econd
      let (t2: Type), (c2: (Type*Type) list) = generate ctx etrue
      let (t3: Type), (c3: (Type*Type) list) = generate ctx efalse

      t2, c1 @ c2 @ c3 @ [t1, TyBool;t2, t3];


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------


// Simple expressions: x = 10 + x
// Assuming x:'a, infers that 'a = int
let e1 = 
  Binary("=",   
    Variable("x"), 
    Binary("+", Constant(10), Variable("x")))

let t1, cs1 = 
  generate (Map.ofList ["x", TyVariable "a"]) e1

solve cs1

// Simple expressions: if x then 2 + 1 else y
// Assuming x:'a, y:'b, infers 'a = bool, 'b = int
let e2 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("y"))

let t2, cs2 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e2

solve cs2

// Simple expressions: if x then 2 + 1 else x
// Cannot be solved, because 'x' used as 'int' and 'bool'
let e3 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("x"))

let t3, cs3 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e3

solve cs3
