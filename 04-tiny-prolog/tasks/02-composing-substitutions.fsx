// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term = 
  // TODO: Replace variables in 'term' for which there is a
  // replacement specified by 'subst.[var]' with the replacement.
  // You can assume the terms in 'subst' do not contain
  // any of the variables that we want to replace.
  failwith "not implemented"


let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  // TODO: Apply the substitution 'newSubst' to all the terms 
  // in the existing substitiution 'subst'. (We represent one 
  // as a map and the other as a list of pairs, which is a bit 
  // inelegant, but it makes calling this function easier later.)
  failwith "not implemented"


let substituteTerms (subst:Map<string, Term>) (terms:list<Term>) = 
  // TODO: Apply substitution 'subst' to all the terms in 'terms'
  failwith "not implemented"


let rec unifyLists l1 l2 : option<list<string * Term>> = 
  match l1, l2 with 
  | [], [] -> 
      Some([])

  | h1::t1, h2::t2 -> 
      let unify_head = unify h1 h2
      let unify_rest = unifyLists t1 t2

      match unify_head with 
      | Some head_bindings -> //head sucessfully unified
        match unify_rest with 
        | Some rest_bindings ->
          Some(head_bindings @ rest_bindings) 

        | None ->
          // head succeeded but rest failed
          None
      | None ->
        // head unification failed
        None

  | _ -> 
    None

and unify (t1: Term) (t2: Term) : option<list<string * Term>> =
  match t1, t2 with 
  | Atom a1, Atom a2 ->  
    Some([])
  
  | Predicate(name1, truths1), Predicate(name2, truths2) when name1 = name2 ->
    unifyLists truths1 truths2

  | Variable name, a
  | a, Variable name ->
    Some([(name, a)])

  //failed to unify
  | _ -> None

// ----------------------------------------------------------------------------
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
unify
  (Predicate("loves", [Atom("narcissus"); Atom("narcissus")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
unify
  (Predicate("loves", [Atom("odysseus"); Atom("penelope")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
unify
  (Predicate("add", [Atom("zero"); Predicate("succ", [Atom("zero")])]))
  (Predicate("add", [Variable("Y"); Predicate("succ", [Variable("Y")])]))

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
unify
  (Predicate("loves", [Variable("X"); Atom("narcissus")]))
  (Predicate("loves", [Variable("Y"); Variable("X")]))

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
unify
  (Predicate("add", 
      [ Predicate("succ", [Variable("X")]); 
        Variable("X") ]))
  (Predicate("add", 
      [ Variable("Y"); 
        Predicate("succ", [Variable("Z")]) ]))

