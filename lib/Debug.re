open Lang;

let prelude = {|
true: \\..
false: \\.
pair: \\\. ... ..
first: \. true
second: \. false
ident: \.
if: ident
pred: \\\... (\\. (.. ....)) (\..) (\.)
succ: (+ 1)
+: \\\\.... .. (... .. .)
*: \\\\.... (... ..) .
-: \\(. pred) ..
/: \\\\ ((.... \\. ..) \..) ((.... \((.... \\. ..) \(.... (. ..))) \.) .)
exp: \\. ..
&&: \\.. . ..
||: \\.. .. .
is_0: \. (\false) true
<=: \\is_0 (- .. .)
==: \\&& (<= .. .) (<= . ..)
not: \\. false true
fix: \(. \.. (. ..)) \(. \.. (. ..))
ident
|}; // Added a final expression 'ident'

let prelude = Parser.parse(prelude);

let wrap_with_prelude = prog => {
  let rec compose_prelude_helper = term => {
    switch (term) {
    | Let(name, value, scope) =>
      Let(name, value, compose_prelude_helper(scope))
    | _ => prog
    };
  };
  compose_prelude_helper(prelude);
};

let wrap_with_prelude_and_trace = (prog, trace) => {
  let wrapped = wrap_with_prelude(prog);
  eval(wrapped, trace);
};

let rec string_of_expr = term => {
  switch (term) {
  | Let(name, value, scope) => 
      name ++ ": " ++ string_of_expr(value) ++ "\n" ++ string_of_expr(scope)
  | Var(name) => name
  | App(func, param) => 
      let func_format = switch (func) {
      | Abs(_) as func => "(" ++ string_of_expr(func) ++ ")"
      | _ => string_of_expr(func)
      };
      let param_format = switch (param) {
      | App(_, _) as param => "(" ++ string_of_expr(param) ++ ")"
      | _ => string_of_expr(param)
      };
      func_format ++ " " ++ param_format;
  | Int(int) => Int.to_string(int)
  | Abs(body) => "\\" ++ string_of_expr(body)
  | Ind(n) => String.make(n + 1, '.')
  };
};

let rec string_of_simple_expr = term => {
  switch (term) {
  | SApp(func, param) => 
      let func_format = switch (func) {
      | SAbs(_) as func => "(" ++ string_of_simple_expr(func) ++ ")"
      | _ => string_of_simple_expr(func)
      };
      let param_format = switch (param) {
      | SApp(_, _) as param => "(" ++ string_of_simple_expr(param) ++ ")"
      | _ => string_of_simple_expr(param)
      };
      func_format ++ " " ++ param_format;
  | SAbs(body) => "\\" ++ string_of_simple_expr(body)
  | SInd(n) => String.make(n + 1, '.')
  };
};

let print_eval_steps = steps => {
  let dim = "\x1b[0;2m";
  let cyan = "\x1b[0;36m";
  let reset = "\x1b[0m";
  
  List.iteri(
    (i, (term, desc)) => {
      print_endline(
        cyan ++ "Step " ++ string_of_int(i) ++ ": " ++ desc ++ reset
      );
      print_endline(dim ++ string_of_simple_expr(term) ++ reset);
      print_newline();
    },
    List.rev(steps)
  );
};
