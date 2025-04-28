type expr = 
  | Let(string, expr, expr) 
  | Var(string) 
  | Int(int) 
  | Abs(expr) 
  | App(expr, expr) 
  | Ind(int);

type simple_expr = 
  | SAbs(simple_expr) 
  | SApp(simple_expr, simple_expr) 
  | SInd(int);

module StringMap = Map.Make(String); 
type var_map = StringMap.t(expr);

let int_of_church = term => {
  let rec int_of_church_inner = (term, acc) => {
    switch (term) {
    | SInd(0) => Some(acc)
    | SApp(SInd(1), inner) => int_of_church_inner(inner, acc + 1)
    | _ => None
    };
  };
  
  switch (term) {
  | SAbs(SAbs(inner)) => int_of_church_inner(inner, 0)
  | _ => None
  };
};

let church_of_int = int => {
  let rec church_inner_of_int = (int, acc) => {
    switch (int) {
    | 0 => acc
    | _ => church_inner_of_int(int - 1, SApp(SInd(1), acc))
    };
  };
  SAbs(SAbs(church_inner_of_int(int, SInd(0))));
};

let bool_of_church = term => {
  switch (term) {
  | SAbs(SAbs(SInd(0))) => Some(false)
  | SAbs(SAbs(SInd(1))) => Some(true)
  | _ => None
  };
};

let rec simplify = (term, env: var_map) => {
  switch (term) {
  | Let(name, value, scope) => simplify(scope, StringMap.add(name, value, env))
  | Var(name) => 
      switch (StringMap.find_opt(name, env)) {
      | Some(value) => simplify(value, env)
      | None => failwith("Undefined variable: " ++ name)
      }
  | Int(int) => church_of_int(int)
  | Abs(body) => SAbs(simplify(body, env))
  | App(func, param) => SApp(simplify(func, env), simplify(param, env))
  | Ind(n) => SInd(n)
  };
};

let rec shift = (term, inc, cutoff) => {
  switch (term) {
  | SInd(n) when n < cutoff => term
  | SInd(n) => SInd(n + inc)
  | SApp(func, param) => SApp(shift(func, inc, cutoff), shift(param, inc, cutoff))
  | SAbs(body) => SAbs(shift(body, inc, cutoff + 1))
  };
};

let rec beta_reduce = (term, replacement, index, depth) => {
  switch (term) {
  // Our substitution. Shift all the free variables in the replacement by the lambda
  // depth and make the replacement.
  | SInd(n) when n == index + depth => shift(replacement, depth, 0)
  // Free variable, leave unchanged.
  | SInd(n) when n < depth => term
  // Non-free variable, decrease index to account for removed outer lambda.
  | SInd(n) => SInd(n - 1)
  | SAbs(body) => SAbs(beta_reduce(body, replacement, index, depth + 1))
  | SApp(func, param) => SApp(
      beta_reduce(func, replacement, index, depth),
      beta_reduce(param, replacement, index, depth),
    )
  };
};

let rec eval_simple_expr = (term, steps) => {
  let add_step = (term, desc) => {
    [(term, desc), ...steps];
  };
  
  switch (term) {
  | SInd(_) as ind => (ind, add_step(ind, "Variable reference"))
  | SAbs(_) as abs => (abs, add_step(abs, "Lambda abstraction"))
  | SApp(func, param) =>
      let (func_eval, func_steps) = eval_simple_expr(func, steps);
      
      switch (func_eval) {
      | SAbs(body) => 
          let steps = add_step(SApp(func_eval, param), "Beta reduction");
          let reduced = beta_reduce(body, param, 0, 0);
          eval_simple_expr(reduced, steps)
      | _ => 
          let (param_eval, param_steps) = eval_simple_expr(param, func_steps);
          (SApp(func_eval, param_eval), param_steps)
      }
  };
};

let eval = (expr, trace) => {
  let simple = simplify(expr, StringMap.empty);
  let initial_steps = if (trace) {
    [(simple, "Initial simplified term")];
  } else {
    [];
  };
  
  let (result, steps) = eval_simple_expr(simple, initial_steps);
  
  if (trace) {
    (result, steps);
  } else {
    (result, []);
  };
};
