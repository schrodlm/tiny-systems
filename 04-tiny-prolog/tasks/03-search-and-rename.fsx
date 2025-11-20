// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
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
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  // TODO: Return a list of all variables that appear in 'term'
  // (this may contain duplicates, we will eliminate them below)
  // HINT: Use List.collect: ('a -> list<'b>) -> list<'a> -> list<'b>
  failwith "not implemented"


let withFreshVariables (clause:Clause) : Clause =
  // TODO: Get a list of distinct variables in the clause (using 
  // 'freeVariables' and 'List.distinct'), generate a substitution 
  // that append a number 'n' obtained by 'nextNumber()' to the end
  // of all the variable names, and apply the substitutions to the 
  // head and body of the clause.
  //
  // For example, 'grandparent(X,Y) :- parent(X,Z), parent(Z,Y)' may
  // become 'grandparent(X3,Y3) :- parent(X3,Z3), parent(Z3,Y3)'
  //
  // This may not be correct if the user-provided names of variables
  // had numbers in them in a certain format, but that's OK for now! 
  failwith "not implemented"


let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =
  // TODO: Return all clauses from 'program' whose 'Head' can be
  // unified with the specified 'query' and return the resulting
  // substitutions. Before unifying, rename variables in the program
  // rule using 'withFreshVariables'. You can do this using 'List.choose' 
  // or by using list comprehension.
  // 
  // The return type of this is a list of tuples consisting of the matching
  // clause and a substitution (list<string * Term>). Calling 'unify'
  // gives you 'option<list<string * Term>>', so you need to pattern match
  // on this and if it is 'Some(subst)' return 'Some(clause, subst)'.
  failwith "not implemented"


// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule (Predicate("grandparent", [Variable("X"); Variable("Y")])) [
  Predicate("parent", [Variable("X"); Variable("Z")])
  Predicate("parent", [Variable("Z"); Variable("Y")]) ]
|> withFreshVariables

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [Variable("X")]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [Variable("X"); Atom("William")]))
