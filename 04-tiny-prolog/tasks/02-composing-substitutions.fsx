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

// Core recursive function to apply a substitution map to a single Term.
// Use Case: Variable Instantiation (e.g., f(X) -> f(a)).
let rec substitute (subst:Map<string, Term>) term = 
  match term with 
  | Variable looking_for ->
    match Map.tryFind looking_for subst with
    | Some substition -> 
      substition
    | None -> term

  | Atom a -> 
    // Atoms are unchanged.
    term

  | Predicate (name, body)-> 
    // Recursively apply substitution to every term in the body list.
    let substituted_body = List.map (substitute subst) body
    Predicate(name, substituted_body)


// Applies a new substitution map to the terms within a list of existing variable bindings.
// Use Case: Composing Bindings (Transitivity). Ensures old bindings are updated 
// (e.g., updates X -> Y to X -> a after finding Y -> a).
let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  // List.map iterates over the existing list of bindings (subst).
  List.map (fun (name, term) -> 
    // Variable 'name' is preserved; 'term' (the value) is substituted using newSubst.
    (name, substitute newSubst term)
  ) subst


// Applies a substitution map across an entire list of terms.
// Use Case: Preparing for Recursion in unifyLists (substituting t1 and t2).
let substituteTerms (subst:Map<string, Term>) (terms:list<Term>) = 
  List.map (fun term -> substitute subst term) terms


let rec unifyLists l1 l2 : option<list<string * Term>> = 
  match l1, l2 with 
  | [], [] -> 
      Some([])

  | h1::t1, h2::t2 -> 
      let unify_head = unify h1 h2
      match unify_head with 
      | Some head_bindings -> //head sucessfully unified
        let head_subst = Map.ofList head_bindings

        let sub_t1 = substituteTerms head_subst t1
        let sub_t2 = substituteTerms head_subst t2

        let unify_rest = unifyLists sub_t1 sub_t2

        match unify_rest with 
        | Some rest_bindings ->
          let rest_subst = Map.ofList rest_bindings
          let substituted_head_bindings = substituteSubst rest_subst head_bindings

          Some(substituted_head_bindings @ rest_bindings)

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
  | Atom a1, Atom a2 when a1 = a2->  
    Some([])
  
  | Predicate(name1, truths1), Predicate(name2, truths2) when name1 = name2 ->
    unifyLists truths1 truths2

  | Variable name1, Variable name2 when name1 = name2 ->
    Some([]) // X=X is a trivial success, no new binding is created
    
  | Variable name1, Variable name2 -> 
    Some([(name1, Variable(name2))]) // X=Y binding

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

// // Requires (1)
// // Example: loves(odysseus, penelope) ~ loves(X, X)
// // Returns: None (cannot unify)
unify
  (Predicate("loves", [Atom("odysseus"); Atom("penelope")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// // Requires (1)
// // Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// // Returns: [ Y -> zero ]
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

