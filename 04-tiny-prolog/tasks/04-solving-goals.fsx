// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
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

let rec freeVariables (term: Term) = 
  match term with
    | Atom _ ->
      [] 
      
    | Variable v -> 
      // A variable term returns a list containing its own name.
      [v] 
      
    | Predicate (_, body) -> 
      List.collect freeVariables body

let withFreshVariables (clause:Clause) : Clause =
  let free_head_vars = freeVariables clause.Head
  let free_body_vars = List.collect (fun term -> freeVariables term) clause.Body
  let all_vars = List.distinct (free_head_vars @ free_body_vars)

  let substitution_list = 
    List.map (fun old_name -> 
        // Create the new variable name (e.g., X -> X3)
        let new_name = old_name + (string (nextNumber()))
        // The substitution is (old_variable, new_variable_term)
        (old_name, Variable(new_name))
    ) all_vars
  let fresh_subst = Map.ofList substitution_list

  let new_head = substitute fresh_subst clause.Head
  let new_body = substituteTerms fresh_subst clause.Body

  {Head = new_head; Body = new_body}

let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =

  let fresh_program = List.map withFreshVariables program

  [| for fresh_clause in fresh_program do
        // Attempt to unify the clause's Head with the Query.
        // NOTE: We only care about the Head for initial matching.
        let unification = unify fresh_clause.Head query

        match unification with
        | Some bindings -> 
            yield (fresh_clause, bindings)

        | None -> 
            () // Empty expression does nothing

    // Convert the resulting sequence array into a list.
    |] |> Array.toList

let rec solve (program : Clause list) subst (queries : Term list) = 
  // Extract original variable names from the query
  let rec extractVars term =
      match term with
      | Variable(v) -> [v]
      | Predicate(_, args) -> List.collect extractVars args
      | Atom(_) -> []
  
  let originalVars = queries |> List.collect extractVars |> Set.ofList
  
  let rec solveQuery program subst goals =
    match goals with 
    | g::goals -> 
        // TODO: We need to solve the goal (term) 'g'. To do so, find all 
        // matching clauses in the 'program' using 'query' and iterate over
        // the returned list using 'for clause, newSubst in matches do'.
        // For each possible solution, we need to add the 'clause.Body' to 
        // the list of 'goals' and apply the substitution 'newSubst' to the
        // new concatentated list of 'goals'. Then we need to apply the 
        // substitution 'newSubst' to the substitution 'subst' we have so far,
        // append the two and call 'solve' recursively with this new substitution
        // to solve the new goals.
    
        let matches =  query program g

        for clause, newSubst in matches do
          // 1. Compose the new substitution (S' o S)
          let substMap = Map.ofList newSubst
          let appliedSubst = substituteSubst substMap subst
          let composedSubst = newSubst @ appliedSubst // This is S'' = S' o S

          // 2. The remaining goals for the next call: new goals appended to existing goals.
          let nextGoalsUnsubstituted = goals @ clause.Body
          
          // 3. Apply the full composed substitution (S'') to the next goals (Goals'')
          //    We need a Map for S''
          let composedSubstMap = Map.ofList composedSubst
          let nextGoals = substituteTerms composedSubstMap nextGoalsUnsubstituted
        
          solveQuery program composedSubst nextGoals // Recurse with S'' and Goals''

    | [] -> 
      // TODO: We solved all goals, which means 'subst' is a possible solution!
      // Print 'subst' (either using printfn "%A" or in some nicer way).
      let filteredSubst = 
                subst |> List.filter (fun (var, _) -> Set.contains var originalVars)

      printfn "Solution found: %A" filteredSubst

  solveQuery program subst queries
// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Some information about the British royal family 
let family: Clause list = [ 
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

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> William, Y -> George, ... ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]

