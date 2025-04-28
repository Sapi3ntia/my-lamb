open Lang;

type state = {
  text: string,
  i: int,
  parentheses: int,
  line_statements: int,
};

let peek = state =>
  if (state.i >= String.length(state.text)) {
    None;
  } else if (state.parentheses > 0 && state.text.[state.i] == ')') {
    None;
  } else if (state.line_statements > 0 && state.text.[state.i] == '\n') {
    None;
  } else {
    Some(state.text.[state.i]);
  };

let advance = state => {
  ...state,
  i: state.i + 1,
};

let nest = state => {
  ...state,
  parentheses: state.parentheses + 1,
  line_statements: state.line_statements - 1,
};

let unnest = state => {
  ...state,
  parentheses: state.parentheses - 1,
  line_statements: state.line_statements + 1,
};

let rec eat_whitespace = state =>
  switch (peek(state)) {
  | Some(' ' | '\t' | '\n') => eat_whitespace(advance(state))
  | _ => state
  };

let rec parse_dots = (state, count) => {
  switch (peek(state)) {
  | Some('.') => parse_dots(advance(state), count + 1)
  | _ => (state, count)
  };
};

let rec parse_int = (state, acc) => {
  switch (peek(state)) {
  | Some('0' .. '9' as c) =>
    parse_int(advance(state), acc * 10 + Char.compare(c, '0'))
  | _ => (state, acc)
  };
};

let rec parse_right_assoc = state => {
  let state = eat_whitespace(state);
  switch (peek(state)) {
  | Some('.') =>
    let (state, count) = parse_dots(state, 0);
    (state, Ind(count - 1));
  | Some('0' .. '9') =>
    let (state, int) = parse_int(state, 0);
    (state, Int(int));
  | Some('\\') =>
    let (state, expr) = parse_left_assoc(advance(state));
    (state, Abs(expr));
  | Some('(') =>
    let (state, expr) = parse_left_assoc(nest(advance(state)));
    let state = unnest(advance(state));
    (state, expr);
  | Some(':') =>
    failwith(
      "Error: Found a colon (:) without a variable name before it. Example: 'myvar: \\.'",
    )
  | Some(_) => parse_ident(state, "")
  | None =>
    failwith(
      "Error: Reached the end of input unexpectedly. Check for missing parentheses or incomplete expressions.",
    )
  };
}
and parse_ident = (state, acc) => {
  let eaten = eat_whitespace(state);
  switch (peek(eaten)) {
  | Some(':') =>
    let state = advance(state);
    let (state, value) =
      parse_right_assoc({
        ...state,
        line_statements: state.line_statements + 1,
      });
    let state = {
      ...state,
      line_statements: state.line_statements - 1,
    };
    let (state, scope) = parse_left_assoc(state);
    (state, Let(acc, value, scope));
  | Some('.' | '\\' | '(') | None => (state, Var(acc))
  | Some(_) when eaten.i > state.i => (state, Var(acc))
  | Some(c) => parse_ident(advance(state), acc ++ String.make(1, c))
  };
}
and collect_left_assoc = (state, acc) => {
  let state = eat_whitespace(state);
  switch (peek(state)) {
  | None => (state, acc)
  | Some(_) =>
    let (state, param) = parse_right_assoc(state);
    collect_left_assoc(state, App(acc, param));
  };
}
and parse_left_assoc = state => {
  let (state, func) = parse_right_assoc(state);
  collect_left_assoc(state, func);
}
and parse = text => {
  let (state, expr) =
    parse_left_assoc({
      text,
      i: 0,
      parentheses: 0,
      line_statements: 0,
    });
  if (state.i < String.length(text) - 1) {
    failwith(
      "Error: Unexpected end of file. You might have extra characters or an incomplete expression at position "
      ++ string_of_int(state.i),
    );
  };
  expr;
};
