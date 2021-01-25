/* This file is free software, part of iter. See file "license" for more details. */
/** {1 Simple and Efficient Iterators} */;

// open Iter_shims_;

/** Iter abstract iterator type */
type t('a) = ('a => unit) => unit;

type iter('a) = t('a);

/*$inject
    let pp_ilist = Q.Print.(list int)
  */
type equal('a) = ('a, 'a) => bool;

type hash('a) = 'a => int;

/** Build an iterator from a iter function */
let from_iter = (f) => f;

let from_labelled_iter = (iter, f) => iter(~f);

let rec from_fun = (f, k) =>
  switch (f()) {
  | None => ()
  | Some(x) =>
    k(x);
    from_fun(f, k);
  };

let empty = (_) => ();

/*$R
    let seq = empty in
    OUnit.assert_bool "empty" (is_empty seq);
    OUnit.assert_bool "empty"
      (try iter (fun _ -> raise Exit) seq; true with Exit -> false);
  */
let return = (x, k) => k(x);

let singleton = return;

let pure = return;

let doubleton = (x, y, k) => {
  k(x);
  k(y);
};

let cons = (x, l, k) => {
  k(x);
  l(k);
};

let snoc = (l, x, k) => {
  l(k);
  k(x);
};

let repeat = (x, k) =>
  while (true) {
    k(x);
  };

/*$R
    let seq = repeat "hello" in
    OUnit.assert_equal ["hello"; "hello"; "hello"]
      (seq |> take 3 |> to_list);
  */
let init = (f, yield) => {
  let rec aux = (i) => {
    yield(f(i));
    aux(i + 1);
  };
  aux(0);
};

/*$=
    [0;1;2;3;4] (init (fun x->x) |> take 5 |> to_list)
  */
let rec iterate = (f, x, k) => {
  k(x);
  iterate(f, f(x), k);
};

let rec forever = (f, k) => {
  k(f());
  forever(f, k);
};

let cycle = (s, k) =>
  while (true) {
    s(k);
  };

let iter = (f, seq) => seq(f);

let iteri = (f, seq) => {
  let r = ref(0);
  seq(
    (x) => {
      f(r^, x);
      incr(r);
    }
  );
};

let fold = (f, init, seq) => {
  let r = ref(init);
  seq((elt) => r := f(r^, elt));
  r^;
};

/*$R
    let n = (1 -- 10)
      |> fold (+) 0 in
    OUnit.assert_equal 55 n;
  */
let foldi = (f, init, seq) => {
  let i = ref(0);
  let r = ref(init);
  seq(
    (elt) => {
      r := f(r^, i^, elt);
      incr(i);
    }
  );
  r^;
};

/*$R
    let l = ["hello"; "world"]
      |> of_list
      |> foldi (fun acc i x -> (i,x) :: acc) [] in
    OUnit.assert_equal [1, "world"; 0, "hello"] l;
  */
let fold_map = (f, init, seq, yield) => {
  let r = ref(init);
  seq(
    (x) => {
      let (acc', y) = f(r^, x);
      r := acc';
      yield(y);
    }
  );
};

/*$= & ~printer:Q.Print.(list int)
    [0;1;3;5] (0--3 |> fold_map (fun prev x -> x,prev+x) 0 |> to_list)
  */
let fold_filter_map = (f, init, seq, yield) => {
  let r = ref(init);
  seq(
    (x) => {
      let (acc', y) = f(r^, x);
      r := acc';
      switch y {
      | None => ()
      | Some(y') => yield(y')
      };
    }
  );
};

let map = (f, seq, k) => seq((x) => k(f(x)));

let mapi = (f, seq, k) => {
  let i = ref(0);
  seq(
    (x) => {
      k(f(i^, x));
      incr(i);
    }
  );
};

let map_by_2 = (f, seq, k) => {
  let r = ref(None);
  let f = (y) =>
    switch r^ {
    | None => r := Some(y)
    | Some(x) => k(f(x, y))
    };
  seq(f);
  switch r^ {
  | None => ()
  | Some(x) => k(x)
  };
};

let filter = (p, seq, k) =>
  seq(
    (x) =>
      if (p(x)) {
        k(x);
      }
  );

let append = (s1, s2, k) => {
  s1(k);
  s2(k);
};

let append_l = (l, k) => List.iter((sub) => sub(k), l);

let concat = (s, k) => s((s') => s'(k));

/*$R
    let s1 = (1 -- 5) in
    let s2 = (6 -- 10) in
    let l = [1;2;3;4;5;6;7;8;9;10] in
    OUnit.assert_equal l (to_list (append s1 s2));
  */
/*$R
    (1 -- 1000)
      |> map (fun i -> i -- (i+1))
      |> concat
      |> length
      |> OUnit.assert_equal 2000
  */
let flatten = concat;

let flat_map = (f, seq, k) => seq((x) => f(x, k));

/*$R
    (1 -- 1000)
      |> flat_map (fun i -> i -- (i+1))
      |> length
      |> OUnit.assert_equal 2000
  */
let flat_map_l = (f, seq, k) => seq((x) => List.iter(k, f(x)));

let rec seq_list_map = (f, l, k) =>
  switch l {
  | [] => k([])
  | [x, ...tail] => f(x, (x') => seq_list_map(f, tail, (tail') => k([x', ...tail'])))
  };

let seq_list = (l) => seq_list_map((x) => x, l);

/*$= & ~printer:Q.Print.(list @@ list int)
    [[1;2];[1;3]] (seq_list [singleton 1; doubleton 2 3] |> to_list)
    [] (seq_list [singleton 1; empty; doubleton 2 3] |> to_list)
    [[1;2;4];[1;3;4]] (seq_list [singleton 1; doubleton 2 3; singleton 4] |> to_list)
  */
let filter_map = (f, seq, k) =>
  seq(
    (x) =>
      switch (f(x)) {
      | None => ()
      | Some(y) => k(y)
      }
  );

let filter_mapi = (f, seq, k) => {
  let i = ref(0);
  seq(
    (x) => {
      let j = i^;
      incr(i);
      switch (f(j, x)) {
      | None => ()
      | Some(y) => k(y)
      };
    }
  );
};

let filter_count = (f, seq) => {
  let i = ref(0);
  seq(
    (x) =>
      if (f(x)) {
        incr(i);
      }
  );
  i^;
};

/*$Q
    Q.(list int) (fun l -> \
      let seq = of_list l and f x = x mod 2 = 0 in \
      filter_count f seq = (filter f seq |> length))
  */
let intersperse = (elem, seq, k) => {
  let first = ref(true);
  seq(
    (x) => {
      if (first^) {
        first := false;
      } else {
        k(elem);
      };
      k(x);
    }
  );
};

/*$R
    (1 -- 100)
      |> (fun seq -> intersperse 0 seq)
      |> take 10
      |> to_list
      |> OUnit.assert_equal [1;0;2;0;3;0;4;0;5;0]
  */
let keep_some = (seq, k) =>
  seq(
    fun
    | Some(x) => k(x)
    | None => ()
  );

let keep_ok = (seq, k) =>
  seq(
    fun
    | Result.Ok(x) => k(x)
    | Result.Error(_) => ()
  );

let keep_error = (seq, k) =>
  seq(
    fun
    | Result.Error(x) => k(x)
    | Result.Ok(_) => ()
  );

/** Mutable unrolled list to serve as intermediate storage */
module MList = {
  type node('a) =
    | Nil
    | Cons(array('a), ref(int), ref(node('a)));
  /* build and call callback on every element */
  let of_iter_with = (seq, k) => {
    let start = ref(Nil);
    let chunk_size = ref(8);
    /* fill the list. prev: tail-reference from previous node */
    let (prev, cur) = (ref(start), ref(Nil));
    seq(
      (x) => {
        k(x); /* callback */
        switch cur^ {
        | Nil =>
          let n = chunk_size^;
          if (n < 4096) {
            chunk_size := 2 * chunk_size^;
          };
          cur := [@implicit_arity] Cons(Array.make(n, x), ref(1), ref(Nil));
        | [@implicit_arity] Cons(a, n, next) =>
          assert (n^ < Array.length(a));
          a[n^] = x;
          incr(n);
          if (n^ == Array.length(a)) {
            prev^ := cur^;
            prev := next;
            cur := Nil;
          };
        };
      }
    );
    prev^ := cur^;
    start^;
  };
  let of_iter = (seq) => of_iter_with(seq, (_) => ());
  let rec iter = (f, l) =>
    switch l {
    | Nil => ()
    | [@implicit_arity] Cons(a, n, tl) =>
      for (i in 0 to n^ - 1) {
        f(a[i]);
      };
      iter(f, tl^);
    };
  let iteri = (f, l) => {
    let rec iteri = (i, f, l) =>
      switch l {
      | Nil => ()
      | [@implicit_arity] Cons(a, n, tl) =>
        for (j in 0 to n^ - 1) {
          f(i + j, a[j]);
        };
        iteri(i + n^, f, tl^);
      };
    iteri(0, f, l);
  };
  let rec iter_rev = (f, l) =>
    switch l {
    | Nil => ()
    | [@implicit_arity] Cons(a, n, tl) =>
      iter_rev(f, tl^);
      for (i in n^ - 1 downto 0) {
        f(a[i]);
      };
    };
  let length = (l) => {
    let rec len = (acc, l) =>
      switch l {
      | Nil => acc
      | [@implicit_arity] Cons(_, n, tl) => len(acc + n^, tl^)
      };
    len(0, l);
  };
  /** Get element by index */
  let rec get = (l, i) =>
    switch l {
    | Nil => raise(Invalid_argument("MList.get"))
    | [@implicit_arity] Cons(a, n, _) when i < n^ => a[i]
    | [@implicit_arity] Cons(_, n, tl) => get(tl^, i - n^)
    };
  let to_iter = (l, k) => iter(k, l);
  let _to_next = (arg, l) => {
    let cur = ref(l);
    let i = ref(0); /* offset in cons */
    let rec get_next = (_) =>
      switch cur^ {
      | Nil => None
      | [@implicit_arity] Cons(_, n, tl) when i^ == n^ =>
        cur := tl^;
        i := 0;
        get_next(arg);
      | [@implicit_arity] Cons(a, _, _) =>
        let x = a[i^];
        incr(i);
        Some(x);
      };
    get_next;
  };
  let to_gen = (l) => _to_next((), l);
  let to_stream = (l) =>
    Stream.from(_to_next(42, l)) /* 42=magic cookiiiiiie */;
  let to_klist = (l) => {
    let rec make = ((l, i), ()) =>
      switch l {
      | Nil => `Nil
      | [@implicit_arity] Cons(_, n, tl) when i == n^ => make((tl^, 0), ())
      | [@implicit_arity] Cons(a, _, _) => `Cons((a[i], make((l, i + 1))))
      };
    make((l, 0));
  };
};

let persistent = (seq) => {
  let l = MList.of_iter(seq);
  MList.to_iter(l);
};

/*$R
    let printer = pp_ilist in
    let stream = Stream.from (fun i -> if i < 5 then Some i else None) in
    let seq = of_stream stream in
    OUnit.assert_equal ~printer [0;1;2;3;4] (seq |> to_list);
    OUnit.assert_equal ~printer [] (seq |> to_list);
  */
/*$R
    let printer = pp_ilist in
    let stream = Stream.from (fun i -> if i < 5 then Some i else None) in
    let seq = of_stream stream in
    (* consume seq into a persistent version of itself *)
    let seq' = persistent seq in
    OUnit.assert_equal ~printer [] (seq |> to_list);
    OUnit.assert_equal ~printer [0;1;2;3;4] (seq' |> to_list);
    OUnit.assert_equal ~printer [0;1;2;3;4] (seq' |> to_list);
    OUnit.assert_equal ~printer [0;1;2;3;4] (seq' |> to_stream |> of_stream |> to_list);
  */
/*$R
    let printer = pp_ilist in
    let seq = (0 -- 10_000) in
    let seq' = persistent seq in
    OUnit.assert_equal 10_001 (length seq');
    OUnit.assert_equal 10_001 (length seq');
    OUnit.assert_equal ~printer [0;1;2;3] (seq' |> take 4 |> to_list);
  */
type lazy_state('a) =
  | LazySuspend
  | LazyCached(t('a));

let persistent_lazy = (seq: t('a)) => {
  let r = ref(LazySuspend);
  (k) =>
    switch r^ {
    | LazyCached(seq') => seq'(k)
    | LazySuspend =>
      /* here if this traversal is interruted, no caching occurs */
      let seq' = MList.of_iter_with(seq, k);
      r := LazyCached(MList.to_iter(seq'));
    };
};

let sort = (~cmp=Pervasives.compare, seq) => {
  /* use an intermediate list, then sort the list */
  let l = fold((l, x) => [x, ...l], [], seq);
  let l = List.fast_sort(cmp, l);
  (k) => List.iter(k, l);
};

/*$R
    (1 -- 100)
      |> sort ~cmp:(fun i j -> j - i)
      |> take 4
      |> to_list
      |> OUnit.assert_equal [100;99;98;97]
  */
exception Exit_sorted;

let sorted = (~cmp=Pervasives.compare, seq) => {
  let prev = ref(None);
  try {
    seq(
      (x) =>
        switch prev^ {
        | Some(y) when cmp(y, x) > 0 => raise_notrace(Exit_sorted)
        | _ => prev := Some(x)
        }
    );
    true;
  } {
  | Exit_sorted => false
  };
};

/*$T
    of_list [1;2;3;4] |> sorted
    not (of_list [1;2;3;0;4] |> sorted)
    sorted empty
  */
let group_succ_by = (~eq=(x, y) => x == y, seq, k) => {
  let cur = ref([]);
  seq(
    (x) =>
      switch cur^ {
      | [] => cur := [x]
      | [y, ..._] as l when eq(x, y) =>
        cur := [x, ...l] /* [x] belongs to the group */
      | [_, ..._] as l =>
        k(l); /* yield group, and start another one */
        cur := [x];
      }
  );
  /* last list */
  switch cur^ {
  | [] => ()
  | [_, ..._] as l => k(l)
  };
};

/*$R
    [1;2;3;3;2;2;3;4]
      |> of_list |> group_succ_by ?eq:None |> to_list
      |> OUnit.assert_equal [[1];[2];[3;3];[2;2];[3];[4]]
  */
let group_by = (type k, ~hash=Hashtbl.hash, ~eq=(==), seq) => {
  module Tbl =
    Hashtbl.Make(
      {
        type t = k;
        let equal = eq;
        let hash = hash;
      }
    );
  /* compute group table */
  let tbl =
    lazy {
      let tbl = Tbl.create(32);
      seq(
        (x) => {
          let l =
            try (Tbl.find(tbl, x)) {
            | Not_found => []
            };
          Tbl.replace(tbl, x, [x, ...l]);
        }
      );
      tbl;
    };
  (yield) => Tbl.iter((_, l) => yield(l), Lazy.force(tbl));
};

/*$R
    [1;2;3;3;2;2;3;4]
      |> of_list |> group_by ?eq:None ?hash:None |> sort ?cmp:None |> to_list
      |> OUnit.assert_equal [[1];[2;2;2];[3;3;3];[4]]
  */
let count = (type k, ~hash=Hashtbl.hash, ~eq=(==), seq) => {
  module Tbl =
    Hashtbl.Make(
      {
        type t = k;
        let equal = eq;
        let hash = hash;
      }
    );
  /* compute group table */
  let tbl =
    lazy {
      let tbl = Tbl.create(32);
      seq(
        (x) => {
          let n =
            try (Tbl.find(tbl, x)) {
            | Not_found => 0
            };
          Tbl.replace(tbl, x, n + 1);
        }
      );
      tbl;
    };
  (yield) => Tbl.iter((x, n) => yield((x, n)), Lazy.force(tbl));
};

/*$R
    [1;2;3;3;2;2;3;4]
      |> of_list |> count ?eq:None ?hash:None |> sort ?cmp:None |> to_list
      |> OUnit.assert_equal [1,1;2,3;3,3;4,1]
  */
let uniq = (~eq=(x, y) => x == y, seq, k) => {
  let has_prev = ref(false)
  and prev = ref(Obj.magic(0)); /* avoid option type, costly */
  seq(
    (x) =>
      if (has_prev^ && eq(prev^, x)) {
        (); /* duplicate */
      } else {
        has_prev := true;
        prev := x;
        k(x);
      }
  );
};

/*$R
    [1;2;2;3;4;4;4;3;3]
      |> of_list |> uniq ?eq:None |> to_list
      |> OUnit.assert_equal [1;2;3;4;3]
  */
let sort_uniq = (type elt, ~cmp=Pervasives.compare, seq) => {
  module S =
    Set.Make(
      {
        type t = elt;
        let compare = cmp;
      }
    );
  let set = fold((acc, x) => S.add(x, acc), S.empty, seq);
  (k) => S.iter(k, set);
};

/*$R
    [42;1;2;3;4;5;4;3;2;1]
      |> of_list
      |> sort_uniq ?cmp:None
      |> to_list
      |> OUnit.assert_equal [1;2;3;4;5;42]
  */
let product = (outer, inner, k) => outer((x) => inner((y) => k((x, y))));

/*$R
    let stream = Stream.from (fun i -> if i < 3 then Some i else None) in
    let a = of_stream stream in
    let b = of_list ["a";"b";"c"] in
    let s = product a b |> map (fun (x,y) -> y,x)
      |> to_list |> List.sort compare in
    OUnit.assert_equal ["a",0; "a", 1; "a", 2;
                        "b",0; "b", 1; "b", 2;
                        "c",0; "c", 1; "c", 2;] s
  */
let rec diagonal_l = (l, yield) =>
  switch l {
  | [] => ()
  | [x, ...tail] =>
    List.iter((y) => yield((x, y)), tail);
    diagonal_l(tail, yield);
  };

/*$=
  [0,1; 0,2; 1,2] (diagonal_l [0;1;2] |> to_list)
  */
let diagonal = (seq) => {
  let l = ref([]);
  seq((x) => l := [x, ...l^]);
  diagonal_l(List.rev(l^));
};

/*$=
  [0,1; 0,2; 1,2] (of_list [0;1;2] |> diagonal |> to_list)
  */
let join = (~join_row, s1, s2, k) =>
  s1(
    (a) =>
      s2(
        (b) =>
          switch (join_row(a, b)) {
          | None => ()
          | Some(c) => k(c)
          }
      )
  );

/*$R
    let s1 = (1 -- 3) in
    let s2 = of_list ["1"; "2"] in
    let join_row i j =
      if string_of_int i = j then Some (string_of_int i ^ " = " ^ j) else None
    in
    let s = join ~join_row s1 s2 in
    OUnit.assert_equal ["1 = 1"; "2 = 2"] (to_list s);
  */
let join_by = (type a, ~eq=(==), ~hash=Hashtbl.hash, f1, f2, ~merge, c1, c2) => {
  module Tbl =
    Hashtbl.Make(
      {
        type t = a;
        let equal = eq;
        let hash = hash;
      }
    );
  let tbl = Tbl.create(32);
  c1(
    (x) => {
      let key = f1(x);
      Tbl.add(tbl, key, x);
    }
  );
  let res = ref([]);
  c2(
    (y) => {
      let key = f2(y);
      let xs = Tbl.find_all(tbl, key);
      List.iter(
        (x) =>
          switch (merge(key, x, y)) {
          | None => ()
          | Some(z) => res := [z, ...res^]
          },
        xs
      );
    }
  );
  (yield) => List.iter(yield, res^);
};

type join_all_cell('a, 'b) = {
  mutable ja_left: list('a),
  mutable ja_right: list('b)
};

let join_all_by = (type a, ~eq=(==), ~hash=Hashtbl.hash, f1, f2, ~merge, c1, c2) => {
  module Tbl =
    Hashtbl.Make(
      {
        type t = a;
        let equal = eq;
        let hash = hash;
      }
    );
  let tbl = Tbl.create(32);
  /* build the map [key -> cell] */
  c1(
    (x) => {
      let key = f1(x);
      try {
        let c = Tbl.find(tbl, key);
        c.ja_left = [x, ...c.ja_left];
      } {
      | Not_found => Tbl.add(tbl, key, {ja_left: [x], ja_right: []})
      };
    }
  );
  c2(
    (y) => {
      let key = f2(y);
      try {
        let c = Tbl.find(tbl, key);
        c.ja_right = [y, ...c.ja_right];
      } {
      | Not_found => Tbl.add(tbl, key, {ja_left: [], ja_right: [y]})
      };
    }
  );
  let res = ref([]);
  Tbl.iter(
    (key, cell) =>
      switch (merge(key, cell.ja_left, cell.ja_right)) {
      | None => ()
      | Some(z) => res := [z, ...res^]
      },
    tbl
  );
  (yield) => List.iter(yield, res^);
};

let group_join_by = (type a, ~eq=(==), ~hash=Hashtbl.hash, f, c1, c2) => {
  module Tbl =
    Hashtbl.Make(
      {
        type t = a;
        let equal = eq;
        let hash = hash;
      }
    );
  let tbl = Tbl.create(32);
  c1((x) => Tbl.replace(tbl, x, []));
  c2(
    (y) => {
      /* project [y] into some element of [c1] */
      let key = f(y);
      try {
        let l = Tbl.find(tbl, key);
        Tbl.replace(tbl, key, [y, ...l]);
      } {
      | Not_found => ()
      };
    }
  );
  (yield) => Tbl.iter((k, l) => yield((k, l)), tbl);
};

/*$=
    ['a', ["abc"; "attic"]; \
     'b', ["barbary"; "boom"; "bop"]; \
     'c', []] \
    (group_join_by (fun s->s.[0]) \
      (of_str "abc") \
      (of_list ["abc"; "boom"; "attic"; "deleted"; "barbary"; "bop"]) \
    |> map (fun (c,l)->c,List.sort Stdlib.compare l) \
    |> sort |> to_list)
  */
let union = (type a, ~eq=(==), ~hash=Hashtbl.hash, c1, c2) => {
  module Tbl =
    Hashtbl.Make(
      {
        type t = a;
        let equal = eq;
        let hash = hash;
      }
    );
  let tbl = Tbl.create(32);
  c1((x) => Tbl.replace(tbl, x, ()));
  c2((x) => Tbl.replace(tbl, x, ()));
  (yield) => Tbl.iter((x, _) => yield(x), tbl);
};

type inter_status =
  | Inter_left
  | Inter_both;

let inter = (type a, ~eq=(==), ~hash=Hashtbl.hash, c1, c2) => {
  module Tbl =
    Hashtbl.Make(
      {
        type t = a;
        let equal = eq;
        let hash = hash;
      }
    );
  let tbl = Tbl.create(32);
  c1((x) => Tbl.replace(tbl, x, Inter_left));
  c2(
    (x) =>
      try (
        switch (Tbl.find(tbl, x)) {
        | Inter_left =>
          Tbl.replace(tbl, x, Inter_both) /* save */
        | Inter_both => ()
        }
      ) {
      | Not_found => ()
      }
  );
  (yield) =>
    Tbl.iter(
      (x, res) =>
        if (res == Inter_both) {
          yield(x);
        },
      tbl
    );
};

let diff = (type a, ~eq=(==), ~hash=Hashtbl.hash, c1, c2) => {
  module Tbl =
    Hashtbl.Make(
      {
        type t = a;
        let equal = eq;
        let hash = hash;
      }
    );
  let tbl = Tbl.create(32);
  c2((x) => Tbl.replace(tbl, x, ()));
  (yield) =>
    c1(
      (x) =>
        if (! Tbl.mem(tbl, x)) {
          yield(x);
        }
    );
};

exception Subset_exit;

let subset = (type a, ~eq=(==), ~hash=Hashtbl.hash, c1, c2) => {
  module Tbl =
    Hashtbl.Make(
      {
        type t = a;
        let equal = eq;
        let hash = hash;
      }
    );
  let tbl = Tbl.create(32);
  c2((x) => Tbl.replace(tbl, x, ()));
  try {
    c1(
      (x) =>
        if (! Tbl.mem(tbl, x)) {
          raise_notrace(Subset_exit);
        }
    );
    true;
  } {
  | Subset_exit => false
  };
};

let rec unfoldr = (f, b, k) =>
  switch (f(b)) {
  | None => ()
  | Some((x, b')) =>
    k(x);
    unfoldr(f, b', k);
  };

/*$R
    let f x = if x < 5 then Some (string_of_int x,x+1) else None in
    unfoldr f 0
      |> to_list
      |> OUnit.assert_equal ["0"; "1"; "2"; "3"; "4"]
  */
let scan = (f, acc, seq, k) => {
  k(acc);
  let acc = ref(acc);
  seq(
    (elt) => {
      let acc' = f(acc^, elt);
      k(acc');
      acc := acc';
    }
  );
};

/*$R
    (1 -- 5)
      |> scan (+) 0
      |> to_list
      |> OUnit.assert_equal ~printer:pp_ilist [0;1;3;6;10;15]
  */
let max = (~lt=(x, y) => x < y, seq) => {
  let ret = ref(None);
  seq(
    (x) =>
      switch ret^ {
      | None => ret := Some(x)
      | Some(y) =>
        if (lt(y, x)) {
          ret := Some(x);
        }
      }
  );
  ret^;
};

let max_exn = (~lt=?, seq) =>
  switch (max(~lt?, seq)) {
  | Some(x) => x
  | None => raise_notrace(Not_found)
  };

let min = (~lt=(x, y) => x < y, seq) => {
  let ret = ref(None);
  seq(
    (x) =>
      switch ret^ {
      | None => ret := Some(x)
      | Some(y) =>
        if (lt(x, y)) {
          ret := Some(x);
        }
      }
  );
  ret^;
};

let min_exn = (~lt=?, seq) =>
  switch (min(~lt?, seq)) {
  | Some(x) => x
  | None => raise(Not_found)
  };

/*$= & ~printer:string_of_int
    100 (0 -- 100 |> max_exn ?lt:None)
    0 (0 -- 100 |> min_exn ?lt:None)
  */
let sum = (seq) => {
  let n = ref(0);
  seq((x) => n := n^ + x);
  n^;
};

/*$T
    (of_list [1;2;3] |> sum) = 6
  */
/* https://en.wikipedia.org/wiki/Kahan_summation_algorithm */
let sumf = (seq) : float => {
  let sum = ref(0.);
  let c = ref(0.); /* error compensation */
  seq(
    (x) => {
      let y = x -. c^;
      let t = sum^ +. y;
      c := t -. sum^ -. y;
      sum := t;
    }
  );
  sum^;
};

/*$R
    let seq = of_list [10000.0; 3.14159; 2.71828] in
    assert_equal ~printer:string_of_float 10005.85987 (sumf seq)
  */
exception ExitHead;

let head = (seq) => {
  let r = ref(None);
  try {
    seq(
      (x) => {
        r := Some(x);
        raise_notrace(ExitHead);
      }
    );
    None;
  } {
  | ExitHead => r^
  };
};

let head_exn = (seq) =>
  switch (head(seq)) {
  | None => invalid_arg("Iter.head_exn")
  | Some(x) => x
  };

exception ExitTake;

let take = (n, seq, k) => {
  let count = ref(0);
  try (
    seq(
      (x) => {
        if (count^ == n) {
          raise_notrace(ExitTake);
        };
        incr(count);
        k(x);
      }
    )
  ) {
  | ExitTake => ()
  };
};

/*$R
    let l = to_list (take 0 (of_list [1])) in
    OUnit.assert_equal ~printer:pp_ilist [] l;
    let l = to_list (take 5 (of_list [1;2;3;4;5;6;7;8;9;10])) in
    OUnit.assert_equal ~printer:pp_ilist [1;2;3;4;5] l;
  */
exception ExitTakeWhile;

let take_while = (p, seq, k) =>
  try (
    seq(
      (x) =>
        if (p(x)) {
          k(x);
        } else {
          raise_notrace(ExitTakeWhile);
        }
    )
  ) {
  | ExitTakeWhile => ()
  };

exception ExitFoldWhile;

let fold_while = (f, s, seq) => {
  let state = ref(s);
  let consume = (x) => {
    let (acc, cont) = f(state^, x);
    state := acc;
    switch cont {
    | `Stop => raise_notrace(ExitFoldWhile)
    | `Continue => ()
    };
  };
  try {
    seq(consume);
    state^;
  } {
  | ExitFoldWhile => state^
  };
};

/*$R
    let n = of_list [true;true;false;true]
      |> fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0 in
    OUnit.assert_equal 2 n;
  */
let drop = (n, seq, k) => {
  let count = ref(0);
  seq(
    (x) =>
      if (count^ >= n) {
        k(x);
      } else {
        incr(count);
      }
  );
};

/*$R
    (1 -- 5) |> drop 2 |> to_list |> OUnit.assert_equal [3;4;5]
  */
let drop_while = (p, seq, k) => {
  let drop = ref(true);
  seq(
    (x) =>
      if (drop^) {
        if (p(x)) {
          ();
        } else {
          drop := false;
          k(x);
        };
      } else {
        k(x);
      }
  );
};

let rev = (seq) => {
  let l = MList.of_iter(seq);
  (k) => MList.iter_rev(k, l);
};

/*$R
    (1 -- 5) |> rev |> to_list |> OUnit.assert_equal [5;4;3;2;1]
  */
exception ExitForall;

let for_all = (p, seq) =>
  try {
    seq(
      (x) =>
        if (! p(x)) {
          raise_notrace(ExitForall);
        }
    );
    true;
  } {
  | ExitForall => false
  };

/*$R
    OUnit.assert_bool "true" (for_all (fun x -> x < 10) (1--9));
    OUnit.assert_bool "false" (not (for_all (fun x -> x < 10) (2--11)));
    OUnit.assert_bool "true" (for_all (fun _ -> false) empty);
    OUnit.assert_bool "nested"
      (for_all
        (fun seq -> not (for_all (fun x -> x < 8) seq))
        (1 -- 10 >|= fun x -> x--20));
  */
exception ExitExists;

/** Exists there some element satisfying the predicate? */
let exists = (p, seq) =>
  try {
    seq(
      (x) =>
        if (p(x)) {
          raise_notrace(ExitExists);
        }
    );
    false;
  } {
  | ExitExists => true
  };

/*$R
    (1 -- 100)
      |> exists (fun x -> x = 59)
      |> OUnit.assert_bool "exists";
    (1 -- 100)
      |> exists (fun x -> x < 0)
      |> (fun x -> not x)
      |> OUnit.assert_bool "not exists";
  */
let mem = (~eq=(==), x, seq) => exists(eq(x), seq);

exception ExitFind;

let find_map = (f, seq) => {
  let r = ref(None);
  try (
    seq(
      (x) =>
        switch (f(x)) {
        | None => ()
        | Some(_) as res =>
          r := res;
          raise_notrace(ExitFind);
        }
    )
  ) {
  | ExitFind => ()
  };
  r^;
};

let find = find_map;

let find_mapi = (f, seq) => {
  let i = ref(0);
  let r = ref(None);
  try (
    seq(
      (x) =>
        switch (f(i^, x)) {
        | None => incr(i)
        | Some(_) as res =>
          r := res;
          raise_notrace(ExitFind);
        }
    )
  ) {
  | ExitFind => ()
  };
  r^;
};

let findi = find_mapi;

let find_pred = (f, seq) =>
  find_map(
    (x) =>
      if (f(x)) {
        Some(x);
      } else {
        None;
      },
    seq
  );

let find_pred_exn = (f, seq) =>
  switch (find_pred(f, seq)) {
  | Some(x) => x
  | None => raise(Not_found)
  };

let length = (seq) => {
  let r = ref(0);
  seq((_) => incr(r));
  r^;
};

/*$R
    (1 -- 1000) |> length |> OUnit.assert_equal 1000
  */
exception ExitIsEmpty;

let is_empty = (seq) =>
  try {
    seq((_) => raise_notrace(ExitIsEmpty));
    true;
  } {
  | ExitIsEmpty => false
  };

/** {2 Transform an iterator} */;

/** {2 Transform an iterator} */
let zip_i = (seq, k) => {
  let r = ref(0);
  seq(
    (x) => {
      let n = r^;
      incr(r);
      k((n, x));
    }
  );
};

let fold2 = (f, acc, seq2) => {
  let acc = ref(acc);
  seq2(((x, y)) => acc := f(acc^, x, y));
  acc^;
};

let iter2 = (f, seq2) => seq2(((x, y)) => f(x, y));

let map2 = (f, seq2, k) => seq2(((x, y)) => k(f(x, y)));

let map2_2 = (f, g, seq2, k) => seq2(((x, y)) => k((f(x, y), g(x, y))));

/** {2 Basic data structures converters} */;

/** {2 Basic data structures converters} */
let to_list = (seq) => List.rev(fold((y, x) => [x, ...y], [], seq));

let to_rev_list = (seq) => fold((y, x) => [x, ...y], [], seq);

let of_list = (l, k) => List.iter(k, l);

let on_list = (f, l) => to_list(f(of_list(l)));

let pair_with_idx = (seq, k) => {
  let r = ref(0);
  seq(
    (x) => {
      let n = r^;
      incr(r);
      k((n, x));
    }
  );
};

let to_opt = head;

let of_opt = (o, k) =>
  switch o {
  | None => ()
  | Some(x) => k(x)
  };

let to_array = (seq) => {
  let l = MList.of_iter(seq);
  let n = MList.length(l);
  if (n == 0) {
    [||];
  } else {
    let a = Array.make(n, MList.get(l, 0));
    MList.iteri((i, x) => a[i] = x, l);
    a;
  };
};

let of_array = (a, k) => Array.iter(k, a);

let of_array_i = (a, k) =>
  for (i in 0 to Array.length(a) - 1) {
    k((i, Array.unsafe_get(a, i)));
  };

let array_slice = (a, i, j, k) => {
  assert (i >= 0 && j < Array.length(a));
  for (idx in i to j) {
    k(a[idx]); /* iterate on sub-array */
  };
};

let of_stream = (s, k) => Stream.iter(k, s);

let to_stream = (seq) => {
  let l = MList.of_iter(seq);
  MList.to_stream(l);
};

let to_stack = (s, seq) => iter((x) => Stack.push(x, s), seq);

let of_stack = (s, k) => Stack.iter(k, s);

let to_queue = (q, seq) => seq((x) => Queue.push(x, q));

let of_queue = (q, k) => Queue.iter(k, q);

let hashtbl_add = (h, seq) => seq(((k, v)) => Hashtbl.add(h, k, v));

/*$R
    let h = (1 -- 5)
      |> zip_i
      |> to_hashtbl in
    (0 -- 4)
      |> iter (fun i -> OUnit.assert_equal (i+1) (Hashtbl.find h i));
    OUnit.assert_equal [0;1;2;3;4] (hashtbl_keys h |> sort ?cmp:None |> to_list);
  */
let hashtbl_replace = (h, seq) => seq(((k, v)) => Hashtbl.replace(h, k, v));

let to_hashtbl = (seq) => {
  let h = Hashtbl.create(3);
  hashtbl_replace(h, seq);
  h;
};

let of_hashtbl = (h, k) => Hashtbl.iter((a, b) => k((a, b)), h);

let hashtbl_keys = (h, k) => Hashtbl.iter((a, _) => k(a), h);

let hashtbl_values = (h, k) => Hashtbl.iter((_, b) => k(b), h);

let of_str = (s, k) => String.iter(k, s);

let to_str = (seq) => {
  let b = Buffer.create(64);
  iter((c) => Buffer.add_char(b, c), seq);
  Buffer.contents(b);
};

let concat_str = (seq) => {
  let b = Buffer.create(64);
  iter(Buffer.add_string(b), seq);
  Buffer.contents(b);
};

exception OneShotSequence;

let of_in_channel = (ic) => {
  let first = ref(true);
  (k) =>
    if (! first^) {
      raise(OneShotSequence);
    } else {
      first := false;
      try (
        while (true) {
          let c = input_char(ic);
          k(c);
        }
      ) {
      | End_of_file => ()
      };
    };
};

let to_buffer = (seq, buf) => seq((c) => Buffer.add_char(buf, c));

/*$R
    let b = Buffer.create 4 in
    let upp = function 'a'..'z' as c -> Char.chr (Char.code c - Char.code 'a' + Char.code 'A') | c -> c in
    "hello world"
      |> of_str |> rev |> map upp
      |> (fun seq -> to_buffer seq b);
    OUnit.assert_equal "DLROW OLLEH" (Buffer.contents b);
  */
/** Iterator on integers in [start...stop] by steps 1 */
let int_range = (~start, ~stop, k) =>
  for (i in start to stop) {
    k(i);
  };

/*$R
    OUnit.assert_equal ~printer:pp_ilist [1;2;3;4] (to_list (1--4));
    OUnit.assert_equal ~printer:pp_ilist [10;9;8;7;6] (to_list (10 --^ 6));
    OUnit.assert_equal ~printer:pp_ilist [] (to_list (10--4));
    OUnit.assert_equal ~printer:pp_ilist [] (to_list (10 --^ 60));
  */
let int_range_dec = (~start, ~stop, k) =>
  for (i in start downto stop) {
    k(i);
  };

let int_range_by = (~step, i, j, yield) => {
  if (step == 0) {
    invalid_arg("int_range_by");
  };
  for (k in 0 to (j - i) / step) {
    yield(k * step + i);
  };
};

/*$= & ~printer:Q.Print.(list int)
    [1;2;3;4] (int_range_by ~step:1 1 4 |> to_list)
    [4;3;2;1] (int_range_by ~step:~-1 4 1 |> to_list)
    [6;4;2] (int_range_by 6 1 ~step:~-2 |> to_list)
    [] (int_range_by ~step:1 4 1 |> to_list)
  */
/*$Q
    Q.(pair small_int small_int) (fun (i,j) -> \
      let i = Stdlib.min i j and j = Stdlib.max i j in \
      (i--j |> to_list) = (int_range_by ~step:1 i j |> to_list))
    Q.(pair small_int small_int) (fun (i,j) -> \
      let i = Stdlib.min i j and j = Stdlib.max i j in \
      (i--j |> to_rev_list) = (int_range_by ~step:~-1 j i |> to_list))
  */
let bools = (k) => {
  k(false);
  k(true);
};

let of_set = (type s, type v, m, set) => {
  module S = (val (m: (module Set.S with type t = s and type elt = v)));
  (k) => S.iter(k, set);
};

let to_set = (type s, type v, m, seq) => {
  module S = (val (m: (module Set.S with type t = s and type elt = v)));
  fold((set, x) => S.add(x, set), S.empty, seq);
};

type gen('a) = unit => option('a);

type klist('a) = unit => [ | `Nil | `Cons('a, klist('a))];

let of_gen = (g) => {
  /* consume the generator to build a MList */
  let rec iter1 = (k) =>
    switch (g()) {
    | None => ()
    | Some(x) =>
      k(x);
      iter1(k);
    };
  let l = MList.of_iter(iter1);
  MList.to_iter(l);
};

let to_gen = (seq) => {
  let l = MList.of_iter(seq);
  MList.to_gen(l);
};

let rec of_klist = (l, k) =>
  switch (l()) {
  | `Nil => ()
  | `Cons(x, tl) =>
    k(x);
    of_klist(tl, k);
  };

let to_klist = (seq) => {
  let l = MList.of_iter(seq);
  MList.to_klist(l);
};

/** {2 Functorial conversions between sets and iterators} */;

module Set = {
  module type S = {
    include Set.S;
    let of_iter: iter(elt) => t;
    let to_iter: t => iter(elt);
    let to_list: t => list(elt);
    let of_list: list(elt) => t;
    /** @deprecated use {!of_iter} instead */
    let of_seq: iter(elt) => t;
    /** @deprecated use {!to_iter} instead */
    let to_seq: t => iter(elt);
  };
  /** Create an enriched Set module from the given one */
  module Adapt = (X: Set.S) : (S with type elt = X.elt and type t = X.t) => {
    let to_iter_ = (set, k) => X.iter(k, set);
    let of_iter_ = (seq) => fold((set, x) => X.add(x, set), X.empty, seq);
    include X;
    let to_iter = to_iter_;
    let of_iter = of_iter_;
    let to_seq = to_iter_;
    let of_seq = of_iter_;
    let of_list = (l) => List.fold_left((set, x) => add(x, set), empty, l);
    let to_list = elements;
  };
  /** Functor to build an extended Set module from an ordered type */
  module Make = (X: Set.OrderedType) => {
    module MySet = Set.Make(X);
    include Adapt(MySet);
  };
};

/** {2 Conversion between maps and iterators.} */;

module Map = {
  module type S = {
    include Map.S;
    let to_iter: t('a) => iter((key, 'a));
    let of_iter: iter((key, 'a)) => t('a);
    let keys: t('a) => iter(key);
    let values: t('a) => iter('a);
    let to_list: t('a) => list((key, 'a));
    let of_list: list((key, 'a)) => t('a);
    /** @deprecated use {!to_iter} instead */
    let to_seq: t('a) => iter((key, 'a));
    /** @deprecated use {!of_iter} instead */
    let of_seq: iter((key, 'a)) => t('a);
  };
  /** Adapt a pre-existing Map module to make it iterator-aware */
  module Adapt = (M: Map.S) => {
    let to_iter_ = (m) => from_iter((k) => M.iter((x, y) => k((x, y)), m));
    let of_iter_ = (seq) => fold((m, (k, v)) => M.add(k, v, m), M.empty, seq);
    let keys = (m) => from_iter((k) => M.iter((x, _) => k(x), m));
    let values = (m) => from_iter((k) => M.iter((_, y) => k(y), m));
    let of_list = (l) => of_iter_(of_list(l));
    let to_list = (x) => to_list(to_iter_(x));
    include M;
    let to_iter = to_iter_;
    let of_iter = of_iter_;
    let to_seq = to_iter_;
    let of_seq = of_iter_;
  };
  /** Create an enriched Map module, with iterator-aware functions */
  module Make = (V: Map.OrderedType) : (S with type key = V.t) => {
    module M = Map.Make(V);
    include Adapt(M);
  };
};

/** {2 Infinite iterators of random values} */;

/** {2 Infinite iterators of random values} */
let random_int = (bound) => forever(() => Random.int(bound));

let random_bool = forever(Random.bool);

let random_float = (bound) => forever(() => Random.float(bound));

let random_array = (a, k) => {
  assert (Array.length(a) > 0);
  while (true) {
    let i = Random.int(Array.length(a));
    k(a[i]);
  };
};

let random_list = (l) => random_array(Array.of_list(l));

/* See http://en.wikipedia.org/wiki/Fisher-Yates_shuffle */
let shuffle_array = (a) =>
  for (k in Array.length(a) - 1 downto 0 + 1) {
    let l = Random.int(k + 1);
    let tmp = a[l];
    a[l] = a[k];
    a[k] = tmp;
  };

let shuffle = (seq) => {
  let a = to_array(seq);
  shuffle_array(a);
  of_array(a);
};

let shuffle_buffer = (n, seq, k) => {
  let seq_front = take(n, seq);
  let a = to_array(seq_front);
  let l = Array.length(a);
  if (l < n) {
    shuffle_array(a);
    of_array(a, k);
  } else {
    let seq = drop(n, seq);
    let f = (x) => {
      let i = Random.int(n);
      let y = a[i];
      a[i] = x;
      k(y);
    };
    seq(f);
  };
};

/** {2 Sampling} */;

/** {2 Sampling} */
/** See https://en.wikipedia.org/wiki/Reservoir_sampling#Algorithm_R */
let sample = (k, seq) =>
  switch (head(seq)) {
  | None => [||]
  | Some(x) =>
    let a = Array.make(k, x);
    let i = ref(-1);
    let f = (x) => {
      incr(i);
      if (i^ < k) {
        a[i^] = x;
      } else {
        let j = Random.int(i^);
        if (j < k) {
          a[j] = x;
        } else {
          ();
        };
      };
    };
    seq(f);
    if (i^ < k) {
      Array.sub(a, 0, i^ + 1);
    } else {
      a;
    };
  };

/*$inject
    let array_for_all f a =
      try
      for i=0 to Array.length a-1 do
        if not (f a.(i)) then raise Exit
       done; true
    with Exit -> false
  */
/*$QR
    Q.(pair (list int) (1 -- 20)) (fun (l, n) ->
      let seq = of_list l in
      let a = sample n seq in
      (array_for_all (fun x -> exists ((=) x) seq) a)
      && (Array.length a = Stdlib.min (length seq) n) )
  */
/** {2 Infix functions} */;

module Infix = {
  let (--) = (i, j) => int_range(~start=i, ~stop=j);
  let (--^) = (i, j) => int_range_dec(~start=i, ~stop=j);
  let (>>=) = (x, f) => flat_map(f, x);
  let (>|=) = (x, f) => map(f, x);
  let (<*>) = (funs, args, k) => funs((f) => args((x) => k(f(x))));
  let (<+>) = append;
};

include Infix;

/** {2 Pretty printing of iterators} */;

/** {2 Pretty printing of iterators} */
/** Pretty print an ['a iter], using the given pretty printer
    to print each elements. An optional separator string can be provided. */
let pp_seq = (~sep=", ", pp_elt, formatter, seq) => {
  let first = ref(true);
  seq(
    (x) => {
      if (first^) {
        first := false;
      } else {
        Format.pp_print_string(formatter, sep);
        Format.pp_print_cut(formatter, ());
      };
      pp_elt(formatter, x);
    }
  );
};

let pp_buf = (~sep=", ", pp_elt, buf, seq) => {
  let first = ref(true);
  seq(
    (x) => {
      if (first^) {
        first := false;
      } else {
        Buffer.add_string(buf, sep);
      };
      pp_elt(buf, x);
    }
  );
};

let to_string = (~sep=?, pp_elt, seq) => {
  let buf = Buffer.create(25);
  pp_buf(~sep?, (buf, x) => Buffer.add_string(buf, pp_elt(x)), buf, seq);
  Buffer.contents(buf);
};

/** {2 Basic IO} */;

module IO = {
  let lines_of = (~mode=420, ~flags=[Open_rdonly], filename, k) => {
    let ic = open_in_gen(flags, mode, filename);
    try (
      while (true) {
        let line = input_line(ic);
        k(line);
      }
    ) {
    | End_of_file => close_in(ic)
    | e =>
      close_in_noerr(ic);
      raise(e);
    };
  };
  let chunks_of = (~mode=420, ~flags=[], ~size=1024, filename, k) => {
    let ic = open_in_gen(flags, mode, filename);
    try {
      let buf = Bytes.create(size);
      let n = ref(0);
      let stop = ref(false);
      while (! stop^) {
        n := 0;
        /* try to read [size] chars. If [input] returns [0] it means
           the end of file, so we stop, but first we yield the current chunk */
        while (n^ < size && ! stop^) {
          let n' = input(ic, buf, n^, size - n^);
          if (n' == 0) {
            stop := true;
          } else {
            n := n^ + n';
          };
        };
        if (n^ > 0) {
          k(Bytes.sub_string(buf, 0, n^));
        };
      };
      close_in(ic);
    } {
    | e =>
      close_in_noerr(ic);
      raise(e);
    };
  };
  let write_bytes_to = (~mode=420, ~flags=[Open_creat, Open_wronly], filename, seq) => {
    let oc = open_out_gen(flags, mode, filename);
    try {
      seq((s) => output(oc, s, 0, Bytes.length(s)));
      close_out(oc);
    } {
    | e =>
      close_out(oc);
      raise(e);
    };
  };
  let write_to = (~mode=?, ~flags=?, filename, seq) =>
    write_bytes_to(~mode?, ~flags?, filename, map(Bytes.unsafe_of_string, seq));
  let write_bytes_lines = (~mode=?, ~flags=?, filename, seq) => {
    let ret = Bytes.unsafe_of_string("\n");
    write_bytes_to(~mode?, ~flags?, filename, snoc(intersperse(ret, seq), ret));
  };
  let write_lines = (~mode=?, ~flags=?, filename, seq) =>
    write_bytes_lines(~mode?, ~flags?, filename, map(Bytes.unsafe_of_string, seq));
};
/* regression tests */
/*$R
  let s = (take 10 (repeat 1)) in
  OUnit.assert_bool "not empty" (not (is_empty s));
  */
