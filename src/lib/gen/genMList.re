/* This file is free software, part of gen. See file "license" for more details. */

/** {1 Efficient Mutable Lists} */;

type gen('a) = unit => option('a);
type clonable('a) = {
  .
  /** Generator of values tied to this copy */ gen: gen('a),
  /** Clone the internal state */ clone: clonable('a),
};

type node('a) =
  | Nil
  | Cons(array('a), ref(int), ref(node('a)))
  | Cons1('a, ref(node('a)))
  | Suspend(gen('a));

type t('a) = {
  start: ref(node('a)), /* first node. */
  mutable chunk_size: int,
  max_chunk_size: int,
};

let _make = (~max_chunk_size, gen) => {
  start: ref(Suspend(gen)),
  chunk_size: 8,
  max_chunk_size,
};

let _make_no_buffer = gen => {
  start: ref(Suspend(gen)),
  chunk_size: 1,
  max_chunk_size: 1,
};

/* increment the size of chunks */
let _incr_chunk_size = mlist =>
  if (mlist.chunk_size < mlist.max_chunk_size) {
    mlist.chunk_size = 2 * mlist.chunk_size;
  };

/* read one chunk of input; return the corresponding node.
   will potentially change [mlist.chunk_size]. */
let _read_chunk = (mlist, gen) =>
  switch (gen()) {
  | None => Nil /* done */
  | Some(x) when mlist.max_chunk_size == 1 =>
    let tail = ref(Suspend(gen));
    let node = [@implicit_arity] Cons1(x, tail);
    node;
  | Some(x) =>
    /* new list node */
    let r = ref(1);
    let a = Array.make(mlist.chunk_size, x);
    let tail = ref(Suspend(gen));
    let stop = ref(false);
    let node = [@implicit_arity] Cons(a, r, tail);
    /* read the rest of the chunk */
    while (! stop^ && r^ < mlist.chunk_size) {
      switch (gen()) {
      | None =>
        tail := Nil;
        stop := true;
      | Some(x) =>
        a[r^] = x;
        incr(r);
      };
    };
    _incr_chunk_size(mlist);
    node;
  };

/* eager construction */
let of_gen = gen => {
  let mlist = _make(~max_chunk_size=4096, gen);
  let rec _fill = prev =>
    switch (_read_chunk(mlist, gen)) {
    | Nil => prev := Nil
    | Suspend(_) => assert(false)
    | [@implicit_arity] Cons1(_, prev') as node =>
      prev := node;
      _fill(prev');
    | [@implicit_arity] Cons(_, _, prev') as node =>
      prev := node;
      _fill(prev');
    };

  _fill(mlist.start);
  mlist;
};

/* lazy construction */
let of_gen_lazy = (~max_chunk_size=2048, ~caching=true, gen) =>
  if (caching) {
    let max_chunk_size = max(max_chunk_size, 2);
    _make(~max_chunk_size, gen);
  } else {
    _make_no_buffer(gen);
  };

let to_gen = l => {
  let cur = ref(l.start);
  let i = ref(0);
  let rec next = () =>
    switch (cur^ ^) {
    | Nil => None
    | [@implicit_arity] Cons1(x, l') =>
      cur := l';
      Some(x);
    | [@implicit_arity] Cons(a, n, l') =>
      if (i^ == n^) {
        cur := l';
        i := 0;
        next();
      } else {
        let y = a[i^];
        incr(i);
        Some(y);
      }
    | Suspend(gen) =>
      let node = _read_chunk(l, gen);
      cur^ := node;
      next();
    };

  next;
};

let to_clonable = (l): clonable('a) => {
  let rec make = (node, i) => {
    let cur = ref(node)
    and i = ref(i);
    let rec next = () =>
      switch (cur^ ^) {
      | Nil => None
      | [@implicit_arity] Cons(a, n, l') =>
        if (i^ == n^) {
          cur := l';
          i := 0;
          next();
        } else {
          let y = a[i^];
          i := i^ + 1;
          Some(y);
        }
      | [@implicit_arity] Cons1(x, l') =>
        cur := l';
        Some(x);
      | Suspend(gen) =>
        let node = _read_chunk(l, gen);
        cur^ := node;
        next();
      };

    {as _; pub gen = next; pub clone = make(cur^, i^)};
  };

  make(l.start, 0);
};
