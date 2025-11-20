// ----------------------------------------------------------------------------
// 01 - Implementing basic unication of terms
// ----------------------------------------------------------------------------

// A term is a recursive type, which can be either an atom (a known fact 
// like 'socrates'), a variable (to which we want to asign a term by 
// unification), or a predicate (with predicate name and a list of arguments). 
type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list
  //| Call of Term * Term list

// A clause is for example 'mortal(X) :- human(X)' or 'human(socrates)'. It
// consists of a head and body (head :- body). Body is a sequence of terms.
type Clause =
  { Head : Term
    Body : Term list }

// A program is just a list of clauses
type Program = Clause list

// Create a clause that states a fact
let fact p = { Head = p; Body = [] }

// Create a clause that defines a rule  
let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Unification of terms and lists
// ----------------------------------------------------------------------------

let rec unifyLists l1 l2 : option<list<string * Term>> = 
  match l1, l2 with 
  | [], [] -> 
      Some([])

  //Unify 'h1' with 'h2' using 'unify' and
  // 't1' with 't2' using 'unifyLists'. If both 
  // succeed, return the generated joint substitution!
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
// Basic unification tests 
// ----------------------------------------------------------------------------

// Example: human(socrates) ~ human(X) 
// Returns: [X -> socrates]
unify
  (Predicate("human", [Atom("socrates")]))
  (Predicate("human", [Variable("X")]))

// Example: human(socrates) ~ mortal(X) 
// Returns: None (fail)
unify
  (Predicate("human", [Atom("socrates")]))
  (Predicate("mortal", [Variable("X")]))

// Example: parent(charles, harry) ~ parent(charles, X)
// Returns: [X -> harry]
unify
  (Predicate("parent", [Atom("charles"); Atom("harry")]))
  (Predicate("parent", [Atom("charles"); Variable("X")]))

// Example: parent(X, harry) ~ parent(charles, Y)
// Returns: [X -> charles; Y -> harry]
unify
  (Predicate("parent", [Variable("X"); Atom("harry")]))
  (Predicate("parent", [Atom("charles"); Variable("Y")]))

// Example: succ(succ(succ(zero))) ~ succ(X)
// Returns: [X -> succ(succ(zero))]
unify
  (Predicate("succ", [Predicate("succ", [Predicate("succ", [Atom("zero")])])]))
  (Predicate("succ", [Variable("X")]))

// Example: succ(succ(zero)) ~ succ(zero)
// Returns: None (fail)
unify
  (Predicate("succ", [Predicate("succ", [Atom("zero")])]))
  (Predicate("succ", [Atom("zero")]))

