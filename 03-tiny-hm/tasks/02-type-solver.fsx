// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

let rec occursCheck (v: string) (ty: Type) =
  match ty with 
    | TyVariable name -> name = v
    | TyBool -> false
    | TyNumber -> false 
    | TyList inner-> occursCheck v inner 
 
let rec substType (subst:Map<string, Type>) ty:Type = 
  // TODO: Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  match ty with
  | TyVariable name -> 
    match Map.tryFind name subst with
    | Some t -> substType subst t
    | None -> ty
  | TyList inner -> TyList (substType subst inner)
  | _ -> ty

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  // TODO: Apply substitution 'subst' to all types in constraints 'cs'
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
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve  
  [ TyList(TyVariable("a")), TyVariable("a") ]
