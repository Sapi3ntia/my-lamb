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

let int_of_church: simple_expr => option(int);
let church_of_int: int => simple_expr;
let bool_of_church: simple_expr => option(bool);
let eval: (expr, bool) => (simple_expr, list((simple_expr, string)));
