/** {2 Test iterators} */;

/** print a list of items using the printing function */

let pp_list = (~sep=", ", pp_item, formatter, l) =>
  Iter.pp_seq(~sep, pp_item, formatter, Iter.of_list(l));

/** Set of integers */
module ISet =
  Set.Make({
    type t = int;
    let compare = compare;
  });
let iset: module Set.S with type elt = int and type t = ISet.t = (module ISet);

module OrderedString = {
  type t = string;
  let compare = compare;
};
module SMap = Iter.Map.Make(OrderedString);

let my_map =
  SMap.of_seq(
    Iter.of_list([("1", 1), ("2", 2), ("3", 3), ("answer", 42)]),
  );

let sexpr = "(foo bar (bazz quux hello 42) world (zoo foo bar (1 2 (3 4))))";

type term =
  | Lambda(term)
  | Const(string)
  | Var(int)
  | Apply(term, term);

let random_term = () => {
  let max = 10
  and num = ref(0);
  let rec build = depth =>
    if (depth > 4 || num^ > max) {
      Const(random_const());
    } else {
      switch (Random.int(6)) {
      | 0 =>
        if (depth > 0) {
          Var(Random.int(depth));
        } else {
          Const(random_const());
        }
      | 1 =>
        incr(num);
        Lambda(build(depth + 1));
      | 2 => Const(random_const())
      | _ =>
        incr(num);
        [@implicit_arity] Apply(build(depth), build(depth));
      };
    }
  and random_const = () => [|"a", "b", "c", "f", "g", "h"|][Random.int(6)];
  build(0);
};

let rec sexpr_of_term = t => {
  let f = (t, k) =>
    switch (t) {
    | Var(i) => Sexpr.output_str("var", string_of_int(i), k)
    | Lambda(t') => Sexpr.output_seq("lambda", sexpr_of_term(t'), k)
    | [@implicit_arity] Apply(t1, t2) =>
      Sexpr.output_seq(
        "apply",
        Iter.append(sexpr_of_term(t1), sexpr_of_term(t2)),
        k,
      )
    | Const(s) => Sexpr.output_str("const", s, k)
    };
  Iter.from_iter(f(t));
};

let term_parser = {
  open Sexpr;
  let rec p_term = () =>
    left
    >> ("lambda", p_lambda)
    ^|| ("var", p_var)
    ^|| ("const", p_const)
    ^|| ("apply", p_apply)
    ^|| fail("bad term")
    >>= (x => right >> return(x))
  and p_apply = () =>
    p_term()
    >>= (x => p_term() >>= (y => return([@implicit_arity] Apply(x, y))))
  and p_var = () => p_int >>= (i => return(Var(i)))
  and p_const = () => p_str >>= (s => return(Const(s)))
  and p_lambda = () => p_term() >>= (t => return(Lambda(t)));
  p_term();
};

let term_of_sexp = seq => Sexpr.parse(term_parser, seq);

let test_term = () => {
  let t = random_term();
  Format.printf(
    "@[<h>random term: %a@]@.",
    Sexpr.pp_tokens,
    sexpr_of_term(t),
  );
  let tokens = sexpr_of_term(t);
  let t' = term_of_sexp(tokens);
  Format.printf("@[<h>parsed: %a@]@.", Sexpr.pp_tokens, sexpr_of_term(t'));
  ();
};

let _ = {
  /* lists */
  let l = [0, 1, 2, 3, 4, 5, 6];
  let l' = Iter.to_list(Iter.filter(x => x mod 2 == 0, Iter.of_list(l)));
  let l'' = Iter.to_list(Iter.take(3, Iter.drop(1, Iter.of_list(l))));
  let h = Hashtbl.create(3);
  for (i in 0 to 5) {
    Hashtbl.add(h, i, i * i);
  };
  let l2 =
    Iter.to_list(
      Iter.map(
        ((x, y)) => string_of_int(x) ++ " -> " ++ string_of_int(y),
        Iter.of_hashtbl(h),
      ),
    );

  let l3 = Iter.to_list(Iter.rev(Iter.int_range(~start=0, ~stop=42)));
  let set =
    List.fold_left(
      (set, x) => ISet.add(x, set),
      ISet.empty,
      [4, 3, 100, 42],
    );
  let l4 = Iter.to_list(Iter.of_set(iset, set));
  Format.printf("l=@[<h>[%a]@]@.", pp_list(Format.pp_print_int), l);
  Format.printf("l'=@[<h>[%a]@]@.", pp_list(Format.pp_print_int), l');
  Format.printf("l''=@[<h>[%a]@]@.", pp_list(Format.pp_print_int), l'');
  Format.printf("l2=@[<h>[%a]@]@.", pp_list(Format.pp_print_string), l2);
  Format.printf("l3=@[<h>[%a]@]@.", pp_list(Format.pp_print_int), l3);
  Format.printf(
    "s={@[<h>%a@]}@.",
    Iter.pp_seq(Format.pp_print_int),
    Iter.of_set(iset, set),
  );
  Format.printf("l4=@[<h>[%a]@]@.", pp_list(Format.pp_print_int), l4);
  Format.printf(
    "l3[:5]+l4=@[<h>[%a]@]@.",
    Iter.pp_seq(Format.pp_print_int),
    Iter.of_array(
      Iter.to_array(
        Iter.append(Iter.take(5, Iter.of_list(l3)), Iter.of_list(l4)),
      ),
    ),
  );
  /* iterator, persistent, etc */
  let seq = Iter.int_range(~start=0, ~stop=100000);
  let seq' = Iter.persistent(seq);
  let stream = Iter.to_stream(seq');
  Format.printf(
    "test length [0..100000]: persistent1 %d, stream %d, persistent2 %d",
    Iter.length(seq'),
    Iter.length(Iter.of_stream(stream)),
    Iter.length(seq'),
  );
  /* maps */
  Format.printf(
    "@[<h>map: %a@]@.",
    Iter.pp_seq((formatter, (k, v)) =>
      Format.fprintf(formatter, "\"%s\" -> %d", k, v)
    ),
    SMap.to_seq(my_map),
  );
  module MyMapSeq = Iter.Map.Adapt((Map.Make(OrderedString)));
  let my_map' =
    MyMapSeq.of_seq(
      Iter.of_list([("1", 1), ("2", 2), ("3", 3), ("answer", 42)]),
    );
  Format.printf(
    "@[<h>map: %a@]@.",
    Iter.pp_seq((formatter, (k, v)) =>
      Format.fprintf(formatter, "\"%s\" -> %d", k, v)
    ),
    MyMapSeq.to_seq(my_map'),
  );
  /* sum */
  let n = 1000000;
  let sum = Iter.fold((+), 0, Iter.take(n, Iter.repeat(1)));
  Format.printf("%dx1 = %d@.", n, sum);
  assert(n == sum);
  /* sexpr */
  let s = Sexpr.of_seq(Sexpr.lex(Iter.of_str(sexpr)));
  let s =
    Sexpr.of_seq(
      Iter.map(
        fun
        | `Atom(s) => `Atom(String.capitalize_ascii(s))
        | tok => tok,
        Sexpr.traverse(s),
      ),
    );

  Format.printf(
    "@[<hov2>transform @[<h>%s@] into @[<h>%a@]@]@.",
    sexpr,
    Sexpr.pp_sexpr(~indent=false),
    s,
  );
  Format.printf(
    "@[<hv2> cycle:%a@]@.",
    Sexpr.pp_tokens,
    Iter.concat(Iter.take(10, Iter.repeat(Sexpr.traverse(s)))),
  );
  /* sexpr parsing/printing */
  for (i in 0 to 20) {
    Format.printf("%d-th term test@.", i);
    test_term();
  };
  ();
};
