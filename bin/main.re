open My_lamb.Lang;
open My_lamb.Debug;

let blue = "\x1b[0;34m";
let yellow = "\x1b[0;33m";
let red = "\x1b[0;31m";
let green = "\x1b[0;32m";
let dim = "\x1b[0;2m";
let cyan = "\x1b[0;36m";
let reset = "\x1b[0m";

let print_result = result => {
  print_endline("Evaluated to: " ++ dim ++ string_of_simple_expr(result) ++ reset);
  
  switch (int_of_church(result)) {
  | Some(num) => print_endline("=> " ++ blue ++ "Number: " ++ Int.to_string(num) ++ reset)
  | None => ()
  };
  
  switch (bool_of_church(result)) {
  | Some(bool) => print_endline("=> " ++ yellow ++ "Boolean: " ++ Bool.to_string(bool) ++ reset)
  | None => ()
  };
};

let trace_mode = ref(false); // Track whether tracing is enabled

let exec_line = line => {
  if (String.trim(line) == ":trace") {
    print_endline(cyan ++ "Tracing mode enabled for next evaluation" ++ reset);
    trace_mode := true; // Enable tracing for the next evaluation
    ignore(LNoise.history_add(":trace"));
    ();
  } else if (String.trim(line) == ":help") {
    print_endline(green ++ "Available commands:" ++ reset);
    print_endline("  :help  - Show this help message");
    print_endline("  :trace - Enable tracing for the next evaluation");
    print_endline("  :exit  - Exit the REPL");
    ignore(LNoise.history_add(":help"));
    ();
  } else if (String.trim(line) == ":exit") {
    exit(0);
  } else {
    let should_trace = trace_mode.contents; // Extract the bool value explicitly
    try {
      let prog = My_lamb.Parser.parse(line);
      let (result, steps) = wrap_with_prelude_and_trace(prog, should_trace);
      if (should_trace && List.length(steps) > 0) {
        print_eval_steps(steps);
      };
      print_result(result);
      trace_mode := false; // Reset tracing after successful evaluation
    } {
    | Failure(e) => 
        print_endline(red ++ e ++ reset);
        trace_mode := false; // Reset tracing on error
    };
  };
};

LNoise.set_multiline(true);

print_endline(green ++ "my-lamb REPL - Lambda calculus interpreter" ++ reset);
print_endline("Type expressions to evaluate, or :help for commands");
print_newline();

let rec read_line = () => {
  switch (LNoise.linenoise("> ")) {
  | Some(line) => 
      if (String.trim(line) != ":trace") {
        ignore(LNoise.history_add(line));
      };
      exec_line(line);
      print_newline();
      read_line();
  | None | exception Stdlib.Sys.Break => ()
  };
};

read_line();
