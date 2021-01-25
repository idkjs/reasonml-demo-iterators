/*
 Copyright (C) 2012 Simon Cruanes

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 This is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 02110-1301 USA.
 */

/* {1 Basic S-expressions, with printing and parsing} */

/** S-expression */

type t =
  | /** An atom */
    Atom(string)
  | /** A list of S-expressions */
    List(list(t));

/** Token that compose a Sexpr once serialized */

type token = [ | `Open | `Close | `Atom(string)];

/** {2 Traverse an iterator of tokens} */;

/** Iterate on the S-expression, calling the callback with tokens */

let rec iter = (f, s) =>
  switch (s) {
  | Atom(a) => f(`Atom(a))
  | List(l) =>
    f(`Open);
    iter_list(f, l);
    f(`Close);
  }
and iter_list = (f, l) =>
  switch (l) {
  | [] => ()
  | [x, ...l'] =>
    iter(f, x);
    iter_list(f, l');
  };

/** Traverse. This yields an iterator of tokens */

let traverse = s => Iter.from_iter(k => iter(k, s));

/** Returns the same iterator of tokens, but during iteration, if
    the structure of the Sexpr corresponding to the iterator
    is wrong (bad parenthesing), Invalid_argument is raised
    and iteration is stoped */

let validate = seq => {
  let depth = ref(0);
  Iter.map(
    tok =>
      switch (tok) {
      | `Open =>
        incr(depth);
        tok;
      | `Close =>
        if (depth^ == 0) {
          raise(Invalid_argument("wrong parenthesing"));
        } else {
          decr(depth);
        };
        tok;
      | _ => tok
      },
    seq,
  );
};

/** {2 Text <-> tokens} */;

/** Lex: create an iterator of tokens from the given in_channel. */

let lex = input => {
  let seq_fun = k => {
    let in_word = ref(false);
    let buf = Buffer.create(128);
    /* loop. TODO handle escaping of (), and "" */
    let rec next = c =>
      switch (c) {
      | '(' => k(`Open)
      | ')' =>
        flush_word();
        k(`Close);
      | ' '
      | '\t'
      | '\n' => flush_word()
      | c =>
        in_word := true;
        Buffer.add_char(buf, c);
      }
    /* finish the previous word token */
    and flush_word = () =>
      if (in_word^) {
        /* this whitespace follows a word */
        let word = Buffer.contents(buf);
        Buffer.clear(buf);
        in_word := false;
        k(`Atom(word));
      };

    Iter.iter(next, input);
  };

  Iter.from_iter(seq_fun);
};

/** Build a Sexpr from an iterator of tokens */

let of_seq = seq => {
  /* called on every token */
  let rec k = (stack, token) =>
    switch (token) {
    | `Open => [`Open, ...stack]
    | `Close => collapse([], stack)
    | `Atom(a) => [`Expr(Atom(a)), ...stack]
    }
  /* collapse last list into an `Expr */
  and collapse = (acc, stack) =>
    switch (stack) {
    | [`Open, ...stack'] => [`Expr(List(acc)), ...stack']
    | [`Expr(a), ...stack'] => collapse([a, ...acc], stack')
    | _ => assert(false)
    };

  /* iterate, given an empty initial stack */
  let stack = Iter.fold(k, [], seq);
  /* stack should contain exactly one expression */
  switch (stack) {
  | [`Expr(expr)] => expr
  | [] => failwith("no Sexpr could be parsed")
  | _ => failwith("too many elements on the stack")
  };
};

/** {2 Printing} */;

/** Print a token on the given formatter */

let pp_token = (formatter, token) =>
  switch (token) {
  | `Open => Format.fprintf(formatter, "@[(")
  | `Close => Format.fprintf(formatter, ")@]")
  | `Atom(s) => Format.pp_print_string(formatter, s)
  };

/** Print an iterator of Sexpr tokens on the given formatter */

let pp_tokens = (formatter, tokens) => {
  let first = ref(true);
  let last = ref(false);
  Iter.iter(
    token => {
      switch (token) {
      | `Open =>
        if (! first^) {
          Format.fprintf(formatter, " ");
        };
        first := true;
      | `Close =>
        first := false;
        last := true;
      | _ =>
        if (first^) {
          first := false;
        } else {
          Format.fprintf(formatter, " ");
        }
      };
      pp_token(formatter, token);
      if (last^) {
        last := false;
      };
    },
    tokens,
  );
};

/** Pretty-print the S-expr. If [indent] is true, the S-expression
    is printed with indentation. */

let pp_sexpr = (~indent=false, formatter, s) =>
  if (indent) {
    Format.fprintf(formatter, "@[<hov 4>%a@]", pp_tokens, traverse(s));
  } else {
    pp_tokens(formatter, traverse(s));
  };

/** {2 Serializing} */;

let output_seq = (name, subexpr, k) => {
  k(`Open);
  k(`Atom(name));
  Iter.iter(k, subexpr);
  k(`Close);
};

let output_str = (name, str, k) => {
  k(`Open);
  k(`Atom(name));
  k(`Atom(str));
  k(`Close);
};

/** {2 Parsing} */;

/** Monadic combinators for parsing data from an iterator of tokens,
    without converting to concrete S-expressions.

    The [one] parser can raise ParseFailure if it fails to parse
    the atomic type. */;

/** parser that returns a 'a */

type parser('a) =
  | Return('a): parser('a)
  | One(token => 'a): parser('a)
  | Zero(token => parser('a)): parser('a)
  /* | Maybe of (token -> 'a option) */
  | Bind((parser('b), 'b => parser('a))): parser('a)
  | Fail(string): parser('a);

exception ParseFailure(string);

let (>>=) = (p, f) => [@implicit_arity] Bind(p, f);

let (>>) = (p, p') => p >>= (_ => p');

let return = x => Return(x);

let fail = reason => Fail(reason);

let one = f => One(f);

let skip = One(_ => ());

let lookahead = f => Zero(f);

let left =
  One(
    fun
    | `Open => ()
    | _ => raise(ParseFailure("expected '('")),
  );

let right =
  One(
    fun
    | `Close => ()
    | _ => raise(ParseFailure("expected ')'")),
  );

let pair = (f, g) => f >>= (x => g >>= (y => return((x, y))));

let triple = (f, g, h) =>
  f >>= (x => g >>= (y => h >>= (z => return((x, y, z)))));

/** [(name,p) ^|| p'] behaves as p if the next token is [`Atom name], and
    like [p'] otherwise */

let (^||) = ((name, p), p') =>
  lookahead(token =>
    switch (token) {
    | `Atom(s) when s == name => skip >> p()
    | _ => p'
    }
  );

/** Maps the value returned by the parser */

let map = (p, f) => p >>= (x => return(f(x)));

let p_str =
  one(
    fun
    | `Atom(s) => s
    | _ => raise(ParseFailure("expected string")),
  );

let p_int =
  one(
    fun
    | `Atom(s) =>
      try(int_of_string(s)) {
      | Failure(_) => raise(ParseFailure("expected int"))
      }
    | _ => raise(ParseFailure("expected int")),
  );

let p_bool =
  one(
    fun
    | `Atom(s) =>
      try(bool_of_string(s)) {
      | Failure(_) => raise(ParseFailure("expected bool"))
      }
    | _ => raise(ParseFailure("expected bool")),
  );

let p_float =
  one(
    fun
    | `Atom(s) =>
      try(float_of_string(s)) {
      | Failure(_) => raise(ParseFailure("expected float"))
      }
    | _ => raise(ParseFailure("expected float")),
  );

let many = p => {
  let rec elements = token =>
    switch (token) {
    | `Close => return([])
    | _ => p >>= (x => lookahead(elements) >>= (l => return([x, ...l])))
    };

  left >> lookahead(elements) >>= (l => right >> return(l));
};

let many1 = p => p >>= (x => many(p) >>= (l => return([x, ...l])));

/** parsing state that returns a 'a */

type state('a) =
  | Bottom: state('a)
  | Push((parser('b), 'b => state('a))): state('a);

/** Actually parse the iterator of tokens, with a callback to be called
    on every parsed value. The callback decides whether to push another
    state or whether to continue. */

let parse_k = (p, tokens, k) => {
  let rec state =
    [@implicit_arity]
    Push(
      p,
      x =>
        switch (k(x)) {
        | `Stop => Bottom
        | `Continue => state
        },
    );
  /* Token handler. It also takes the current parser. */
  let rec one_step = (state, token) =>
    switch (reduce(state)) {
    | Bottom =>
      /* should not happen, unless there are too many tokens */
      raise(ParseFailure("unexpected ')'"))
    | [@implicit_arity] Push(Return(_), _cont) => assert(false) /* should be reduced */
    | [@implicit_arity] Push(Zero(f), cont) =>
      let p' = f(token);
      let state' = [@implicit_arity] Push(p', cont);
      one_step(state', token); /* do not consume token */
    | [@implicit_arity] Push(One(f), cont) =>
      let x = f(token);
      let state' = cont(x);
      reduce(state'); /* consume token */
    /* | Maybe f, _ -> let x = f token in (Obj.magic cont) x */
    | [@implicit_arity] Push([@implicit_arity] Bind(p', cont'), cont) =>
      let cont'' = x => {
        let p'' = cont'(x);
        [@implicit_arity] Push(p'', cont);
      };

      let state' = [@implicit_arity] Push(p', cont'');
      one_step(state', token); /* do not consume token */
    | [@implicit_arity] Push(Fail(reason), _) =>
      raise(ParseFailure(reason))
    }
  /* Reduce parser state */
  and reduce = state =>
    switch (state) {
    | [@implicit_arity] Push(Return(x), cont) =>
      let state' = cont(x);
      reduce(state');
    | _ => state
    };

  /* iterate on the tokens */
  ignore(Iter.fold(one_step, state, tokens));
};

/** Parse one value */

let parse = (p, tokens) => {
  let res = ref(None);
  parse_k(
    p,
    tokens,
    x => {
      res := Some(x);
      `Stop;
    },
  );
  /* return result */
  switch (res^) {
  | None => raise(ParseFailure("incomplete input"))
  | Some(x) => x
  };
};

/** Parse an iterator of values */

let parse_seq = (p, tokens) => {
  let seq_fun = k =>
    parse_k(
      p,
      tokens,
      x => {
        k(x);
        `Continue;
      },
    );

  Iter.from_iter(seq_fun);
};
