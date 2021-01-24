/* This file is free software, part of gen. See file "license" for more details. */

/** {1 Restartable generators} */;

/** {2 Global type declarations} */;

type t('a) = unit => option('a);

type gen('a) = t('a);

module type S = Gen_intf.S;

/*$inject

    let pint i = string_of_int i
    let pilist l =
      let b = Buffer.create 15 in
      let fmt = Format.formatter_of_buffer b in
      Format.fprintf fmt "%a@?"
        (Gen.pp Format.pp_print_int) (Gen.of_list l);
      Buffer.contents b
    let pi2list l =
      let b = Buffer.create 15 in
      let fmt = Format.formatter_of_buffer b in
      Format.fprintf fmt "%a@?"
        (Gen.pp (fun fmt (a,b) -> Format.fprintf fmt "%d,%d" a b))
        (Gen.of_list l);
      Buffer.contents b
    let pstrlist l =
      let b = Buffer.create 15 in
      let fmt = Format.formatter_of_buffer b in
      Format.fprintf fmt "%a@?"
        (Gen.pp Format.pp_print_string) (Gen.of_list l);
      Buffer.contents b

  */

/** {2 Transient generators} */;

let empty = () => None;

/*$T empty
    empty |> to_list = []
  */

let singleton = x => {
  let first = ref(true);
  () =>
    if (first^) {
      first := false;
      Some(x);
    } else {
      None;
    };
};

/*$T singleton
    singleton 1 |> to_list = [1]
    singleton "foo" |> to_list = ["foo"]
  */

/*$R
    let gen = Gen.singleton 42 in
    OUnit.assert_equal (Some 42) (Gen.get gen);
    OUnit.assert_equal None (Gen.get gen);
    let gen = Gen.singleton 42 in
    OUnit.assert_equal 1 (Gen.length gen);
  */

let return = singleton;

let repeat = (x, ()) => Some(x);

/*$T repeat
    repeat 42 |> take 3 |> to_list = [42; 42; 42]
  */

let repeatedly = (f, ()) => Some(f());

/*$T repeatedly
    repeatedly (let r = ref 0 in fun () -> incr r; !r) \
      |> take 5 |> to_list = [1;2;3;4;5]
  */

let iterate = (x, f) => {
  let cur = ref(x);
  () => {
    let x = cur^;
    cur := f(cur^);
    Some(x);
  };
};

/*$T iterate
    iterate 0 ((+)1) |> take 5 |> to_list = [0;1;2;3;4]
  */

let next = gen => gen();

let get = gen => gen();

let get_exn = gen =>
  switch (gen()) {
  | Some(x) => x
  | None => raise(Invalid_argument("Gen.get_exn"))
  };

/*$R get_exn
    let g = of_list [1;2;3] in
    assert_equal 1 (get_exn g);
    assert_equal 2 (get_exn g);
    assert_equal 3 (get_exn g);
    assert_raises (Invalid_argument "Gen.get_exn") (fun () -> get_exn g)
  */

let junk = gen => ignore(gen());

let rec fold = (f, acc, gen) =>
  switch (gen()) {
  | None => acc
  | Some(x) => fold(f, f(acc, x), gen)
  };

/*$Q
    (Q.list Q.small_int) (fun l -> \
      of_list l |> fold (fun l x->x::l) [] = List.rev l)
  */

let reduce = (f, g) => {
  let acc =
    switch (g()) {
    | None => raise(Invalid_argument("reduce"))
    | Some(x) => x
    };

  fold(f, acc, g);
};

/* Dual of {!fold}, with a deconstructing operation */
let unfold = (f, acc) => {
  let acc = ref(acc);
  () =>
    switch (f(acc^)) {
    | None => None
    | Some((x, acc')) =>
      acc := acc';
      Some(x);
    };
};

/*$T unfold
    unfold (fun (prev,cur) -> Some (prev, (cur,prev+cur))) (0,1) \
      |> take 7 |> to_list = [0; 1; 1; 2; 3; 5; 8]
  */

let init = (~limit=max_int, f) => {
  let r = ref(0);
  () =>
    if (r^ >= limit) {
      None;
    } else {
      let x = f(r^);
      let _ = incr(r);
      Some(x);
    };
};

/*$T init
    init ~limit:5 (fun i->i) |> to_list = [0;1;2;3;4]
  */

let rec iter = (f, gen) =>
  switch (gen()) {
  | None => ()
  | Some(x) =>
    f(x);
    iter(f, gen);
  };

/*$R iter
  let e = Restart.(1 -- 10) in
  OUnit.assert_equal ~printer:pint 10 (Restart.length e);
  OUnit.assert_equal [1;2] Restart.(to_list (1 -- 2));
  OUnit.assert_equal [1;2;3;4;5] (Restart.to_list (Restart.take 5 e));
  */

let iteri = (f, gen) => {
  let rec iteri = i =>
    switch (gen()) {
    | None => ()
    | Some(x) =>
      f(i, x);
      iteri(i + 1);
    };

  iteri(0);
};

let is_empty = gen =>
  switch (gen()) {
  | None => true
  | Some(_) => false
  };

/*$T
    is_empty empty
    not (is_empty (singleton 2))
  */

let length = gen => fold((acc, _) => acc + 1, 0, gen);

/*$Q
    (Q.list Q.small_int) (fun l -> \
      of_list l |> length = List.length l)
  */

/* useful state */
module RunState = {
  type t('a) =
    | Init
    | Run('a)
    | Stop;
};

let scan = (f, acc, g) => {
  open RunState;
  let state = ref(Init);
  () =>
    switch (state^) {
    | Init =>
      state := Run(acc);
      Some(acc);
    | Stop => None
    | Run(acc) =>
      switch (g()) {
      | None =>
        state := Stop;
        None;
      | Some(x) =>
        let acc' = f(acc, x);
        state := Run(acc');
        Some(acc');
      }
    };
};

/*$T scan
    scan (fun acc x -> x+1::acc) [] (1--5) |> to_list \
      = [[]; [2]; [3;2]; [4;3;2]; [5;4;3;2]; [6;5;4;3;2]]
  */

let unfold_scan = (f, acc, g) => {
  open RunState;
  let state = ref(Run(acc));
  () =>
    switch (state^) {
    | Init => assert(false)
    | Stop => None
    | Run(acc) =>
      switch (g()) {
      | None =>
        state := Stop;
        None;
      | Some(x) =>
        let (acc', y) = f(acc, x);
        state := Run(acc');
        Some(y);
      }
    };
};

/*$T unfold_scan
    unfold_scan (fun acc x -> x+acc,acc) 0 (1--5) |> to_list \
      = [0; 1; 3; 6; 10]
  */

/** {3 Lazy} */;

let map = (f, gen) => {
  let stop = ref(false);
  () =>
    if (stop^) {
      None;
    } else {
      switch (gen()) {
      | None =>
        stop := true;
        None;
      | Some(x) => Some(f(x))
      };
    };
};

/*$Q map
    (Q.list Q.small_int) (fun l -> \
      let f x = x*2 in \
      of_list l |> map f |> to_list = List.map f l)
  */

/*$R
    let e = 1 -- 10 in
    let e' = e >>| string_of_int in
    OUnit.assert_equal ~printer:pstrlist ["9"; "10"] (Gen.to_list (Gen.drop 8 e'));
  */

let mapi = f => {
  let cnt = ref(0);
  let cnt_map = x => {
    let i = cnt^;
    cnt := i + 1;
    f(i, x);
  };
  map(cnt_map);
};

/*$Q mapi
    (Q.list Q.small_int) (fun l -> \
      let len = List.length l in \
      let f i x = i+x+1 in \
      of_list l |> mapi f |> to_list |> fun l' -> List.fold_left (+) 0 l'= \
        len*(len+1)/2 + List.fold_left (+) 0 l)
  */

let fold_map = (f, s, gen) =>
  map(
    {
      let state = ref(s);
      x => {
        state := f(state^, x);
        state^;
      };
    },
    gen,
  );

/*$T
    fold_map (+) 0 (1--3) |> to_list = [1;3;6]
  */

let append = (gen1, gen2) => {
  let first = ref(true);
  () =>
    if (first^) {
      switch (gen1()) {
      | Some(_) as x => x
      | None =>
        first := false;
        gen2();
      };
    } else {
      gen2();
    };
};

/*$Q
    (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
      append (of_list l1) (of_list l2) |> to_list = l1 @ l2)
  */

/*$R
    let e = Gen.append (1 -- 5) (6 -- 10) in
    OUnit.assert_equal [10;9;8;7;6;5;4;3;2;1] (Gen.to_rev_list e);
  */

let flatten = next_gen => {
  open RunState;
  let state = ref(Init);
  /* get next element */
  let rec next = () =>
    switch (state^) {
    | Init => get_next_gen()
    | Run(gen) =>
      switch (gen()) {
      | None => get_next_gen()
      | Some(_) as x => x
      }
    | Stop => None
    }
  and get_next_gen = () =>
    switch (next_gen()) {
    | None =>
      state := Stop;
      None;
    | Some(gen) =>
      state := Run(gen);
      next();
    };

  next;
};

let flat_map = (f, next_elem) => {
  open RunState;
  let state = ref(Init);
  let rec next = () =>
    switch (state^) {
    | Init => get_next_gen()
    | Run(gen) =>
      switch (gen()) {
      | None => get_next_gen()
      | Some(_) as x => x
      }
    | Stop => None
    }
  and get_next_gen = () =>
    switch (next_elem()) {
    | None =>
      state := Stop;
      None;
    | Some(x) =>
      try(state := Run(f(x))) {
      | e =>
        state := Stop;
        raise(e);
      };
      next();
    };

  next;
};

/*$Q flat_map
    (Q.list Q.small_int) (fun l -> \
      let f x = of_list [x;x*2] in \
      eq (map f (of_list l) |> flatten) (flat_map f (of_list l)))
  */

/*$T
    flat_map (fun x -> if x mod 1_500_000=0 then singleton x else empty) (1 -- 6_000_000) \
      |> to_list = [1_500_000; 3_000_000; 4_500_000; 6_000_000]
  */

/*$R
    let e = 1 -- 3 in
    let e' = e >>= (fun x -> x -- (x+1)) in
    OUnit.assert_equal [1;2;2;3;3;4] (Gen.to_list e');
  */

let mem = (~eq=(==), x, gen) => {
  let rec mem = (eq, x, gen) =>
    switch (gen()) {
    | Some(y) => eq(x, y) || mem(eq, x, gen)
    | None => false
    };
  mem(eq, x, gen);
};

let take = (n, gen) => {
  assert(n >= 0);
  let count = ref(0); /* how many yielded elements */
  () =>
    if (count^ == n || count^ == - 1) {
      None;
    } else {
      switch (gen()) {
      | None =>
        count := - 1;
        None; /* indicate stop */
      | Some(_) as x =>
        incr(count);
        x;
      };
    };
};

/*$Q
    (Q.pair Q.small_int (Q.list Q.small_int)) (fun (n,l) -> \
      of_list l |> take n |> length = Pervasives.min n (List.length l))
  */

/* call [gen] at most [n] times, and stop */
let rec __drop = (n, gen) =>
  if (n == 0) {
    ();
  } else {
    switch (gen()) {
    | Some(_) => __drop(n - 1, gen)
    | None => ()
    };
  };

let drop = (n, gen) => {
  assert(n >= 0);
  let dropped = ref(false);
  () =>
    if (dropped^) {
      gen();
    } else {
      /* drop [n] elements and yield the next element */
      dropped := true;
      __drop(n, gen);
      gen();
    };
};

/*$Q
    (Q.pair Q.small_int (Q.list Q.small_int)) (fun (n,l) -> \
      let g1,g2 = take n (of_list l), drop n (of_list l) in \
      append g1 g2 |> to_list = l)
  */

let nth = (n, gen) => {
  assert(n >= 0);
  __drop(n, gen);
  switch (gen()) {
  | None => raise(Not_found)
  | Some(x) => x
  };
};

/*$= nth & ~printer:string_of_int
    4 (nth 4 (0--10))
    8 (nth 8 (0--10))
  */

/*$T
    (try ignore (nth 11 (1--10)); false with Not_found -> true)
  */

let take_nth = (n, gen) => {
  assert(n >= 1);
  let i = ref(n);
  let rec next = () =>
    switch (gen()) {
    | None => None
    | Some(_) as res when i^ == n =>
      i := 1;
      res;
    | Some(_) =>
      incr(i);
      next();
    };
  next;
};

let filter = (p, gen) => {
  let rec next = () =>
    /* wrap exception into option, for next to be tailrec */
    switch (gen()) {
    | None => None
    | Some(x) as res =>
      if (p(x)) {
        res;
      } else {
        /* yield element */
        next();
      }
    }; /* discard element */
  next;
};

/*$T
    filter (fun x ->x mod 2 = 0) (1--10) |> to_list = [2;4;6;8;10]
  */

let take_while = (p, gen) => {
  let stop = ref(false);
  () =>
    if (stop^) {
      None;
    } else {
      switch (gen()) {
      | Some(x) as res =>
        if (p(x)) {
          res;
        } else {
          stop := true;
          None;
        }
      | None =>
        stop := true;
        None;
      };
    };
};

/*$T
    take_while (fun x ->x<10) (1--1000) |> eq (1--9)
  */

let fold_while = (f, s, gen) => {
  let state = ref(s);
  let rec consume = gen =>
    switch (gen()) {
    | None => ()
    | Some(x) =>
      let (acc, cont) = f(state^, x);
      state := acc;
      switch (cont) {
      | `Stop => ()
      | `Continue => consume(gen)
      };
    };

  consume(gen);
  state^;
};

/*$T
    fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0 \
      (of_list [true;true;false;true]) = 2
  */

module DropWhileState = {
  type t =
    | Stop
    | Drop
    | Yield;
};

/* state machine starts at Drop:
       Drop:
         - If next element doesn't satisfy predicate, goto yield
         - if no more elements, goto stop
       Yield:
         - if there is a next element, yield it
         - if no more elements, goto stop
       Stop: just return None
   */
let drop_while = (p, gen) => {
  open DropWhileState;
  let state = ref(Drop);
  let rec next = () =>
    switch (state^) {
    | Stop => None
    | Drop =>
      switch (gen()) {
      | None =>
        state := Stop;
        None;
      | Some(x) as res =>
        if (p(x)) {
          next();
        } else {
          state := Yield;
          res;
        }
      }
    | Yield =>
      switch (gen()) {
      | None =>
        state := Stop;
        None;
      | Some(_) as res => res
      }
    };
  next;
};

/*$T
    drop_while (fun x-> x<10) (1--20) |> eq (10--20)
  */

let filter_map = (f, gen) => {
  /* tailrec */
  let rec next = () =>
    switch (gen()) {
    | None => None
    | Some(x) =>
      switch (f(x)) {
      | None => next()
      | Some(_) as res => res
      }
    };
  next;
};

/*$T
    filter_map (fun x-> if x mod 2 = 0 then Some (string_of_int x) else None) (1--10) \
      |> to_list = List.map string_of_int [2;4;6;8;10]
  */

/*$R
    let f x = if x mod 2 = 0 then Some (string_of_int x) else None in
    let e = Gen.filter_map f (1 -- 10) in
    OUnit.assert_equal ["2"; "4"; "6"; "8"; "10"] (Gen.to_list e);
  */

let zip_index = gen => {
  let r = ref(- 1);
  () =>
    switch (gen()) {
    | None => None
    | Some(x) =>
      incr(r);
      Some((r^, x));
    };
};

/*$T
    zip_index (1--5) |> to_list = [0,1; 1,2; 2,3; 3,4; 4,5]
  */

let unzip = gen => {
  let stop = ref(false);
  let q1 = Queue.create();
  let q2 = Queue.create();
  let next_left = () =>
    if (Queue.is_empty(q1)) {
      if (stop^) {
        None;
      } else {
        switch (gen()) {
        | Some((x, y)) =>
          Queue.push(y, q2);
          Some(x);
        | None =>
          stop := true;
          None;
        };
      };
    } else {
      Some(Queue.pop(q1));
    };

  let next_right = () =>
    if (Queue.is_empty(q2)) {
      if (stop^) {
        None;
      } else {
        switch (gen()) {
        | Some((x, y)) =>
          Queue.push(x, q1);
          Some(y);
        | None =>
          stop := true;
          None;
        };
      };
    } else {
      Some(Queue.pop(q2));
    };

  (next_left, next_right);
};

/*$T
    unzip (of_list [1,2;3,4]) |> (fun (x,y)-> to_list x, to_list y) \
      = ([1;3], [2;4])
  */

/*$Q
    (Q.list (Q.pair Q.small_int Q.small_int)) (fun l -> \
      of_list l |> unzip |> (fun (x,y) -> to_list x,to_list y) = \
      List.split l)
  */

/* [partition p l] returns the elements that satisfy [p],
   and the elements that do not satisfy [p] */
let partition = (p, gen) => {
  let qtrue = Queue.create();
  let qfalse = Queue.create();
  let stop = ref(false);
  let rec nexttrue = () =>
    if (Queue.is_empty(qtrue)) {
      if (stop^) {
        None;
      } else {
        switch (gen()) {
        | Some(x) as res =>
          if (p(x)) {
            res;
          } else {
            Queue.push(x, qfalse);
            nexttrue();
          }
        | None =>
          stop := true;
          None;
        };
      };
    } else {
      Some(Queue.pop(qtrue));
    }
  and nextfalse = () =>
    if (Queue.is_empty(qfalse)) {
      if (stop^) {
        None;
      } else {
        switch (gen()) {
        | Some(x) as res =>
          if (p(x)) {
            Queue.push(x, qtrue);
            nextfalse();
          } else {
            res;
          }
        | None =>
          stop := true;
          None;
        };
      };
    } else {
      Some(Queue.pop(qfalse));
    };

  (nexttrue, nextfalse);
};

/*$T
    partition (fun x -> x mod 2 = 0) (1--10) |> \
      (fun (x,y)->to_list x, to_list y) = ([2;4;6;8;10], [1;3;5;7;9])
  */

let rec for_all = (p, gen) =>
  switch (gen()) {
  | None => true
  | Some(x) => p(x) && for_all(p, gen)
  };

let rec exists = (p, gen) =>
  switch (gen()) {
  | None => false
  | Some(x) => p(x) || exists(p, gen)
  };

let min = (~lt=(x, y) => x < y, gen) => {
  let first =
    switch (gen()) {
    | Some(x) => x
    | None => raise(Invalid_argument("min"))
    };

  fold(
    (min, x) =>
      if (lt(x, min)) {
        x;
      } else {
        min;
      },
    first,
    gen,
  );
};

/*$T
    min (of_list [1;4;6;0;11; -2]) = ~-2
    (try ignore (min empty); false with Invalid_argument _ -> true)
  */

let max = (~lt=(x, y) => x < y, gen) => {
  let first =
    switch (gen()) {
    | Some(x) => x
    | None => raise(Invalid_argument("max"))
    };

  fold(
    (max, x) =>
      if (lt(max, x)) {
        x;
      } else {
        max;
      },
    first,
    gen,
  );
};

/*$T
    max (of_list [1;4;6;0;11; -2]) = 11
    (try ignore (max empty); false with Invalid_argument _ -> true)
  */

let eq = (~eq=(==), gen1, gen2) => {
  let rec check = () =>
    switch (gen1(), gen2()) {
    | (None, None) => true
    | (Some(x1), Some(x2)) when eq(x1, x2) => check()
    | _ => false
    };

  check();
};

/*$Q
    (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
      eq (of_list l1)(of_list l2) = (l1 = l2))
  */

let lexico = (~cmp=Pervasives.compare, gen1, gen2) => {
  let rec lexico = () =>
    switch (gen1(), gen2()) {
    | (None, None) => 0
    | (Some(x1), Some(x2)) =>
      let c = cmp(x1, x2);
      if (c != 0) {
        c;
      } else {
        lexico();
      };
    | (Some(_), None) => 1
    | (None, Some(_)) => (-1)
    };
  lexico();
};

let compare = (~cmp=?, gen1, gen2) => lexico(~cmp?, gen1, gen2);

/*$Q
    (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
      let sign x = if x < 0 then -1 else if x=0 then 0 else 1 in \
      sign (compare (of_list l1)(of_list l2)) = sign (Pervasives.compare l1 l2))
  */

let rec find = (p, e) =>
  switch (e()) {
  | None => None
  | Some(x) when p(x) => Some(x)
  | Some(_) => find(p, e)
  };

/*$T
     find (fun x -> x>=5) (1--10) = Some 5
     find (fun x -> x>5) (1--4) = None
  */

let sum = e => {
  let rec sum = acc =>
    switch (e()) {
    | None => acc
    | Some(x) => sum(x + acc)
    };
  sum(0);
};

/*$T
    sum (1--10) = 55
  */

/** {2 Multiple Iterators} */;

let map2 = (f, e1, e2, ()) =>
  switch (e1(), e2()) {
  | (Some(x), Some(y)) => Some(f(x, y))
  | _ => None
  };

/*$T
    map2 (+) (1--5) (1--4) |> eq (of_list [2;4;6;8])
    map2 (+) (1--5) (repeat 0) |> eq (1--5)
  */

let rec iter2 = (f, e1, e2) =>
  switch (e1(), e2()) {
  | (Some(x), Some(y)) =>
    f(x, y);
    iter2(f, e1, e2);
  | _ => ()
  };

/*$T iter2
    let r = ref 0 in iter2 (fun _ _ -> incr r) (1--10) (4--6); !r = 3
  */

let rec fold2 = (f, acc, e1, e2) =>
  switch (e1(), e2()) {
  | (Some(x), Some(y)) => fold2(f, f(acc, x, y), e1, e2)
  | _ => acc
  };

let rec for_all2 = (p, e1, e2) =>
  switch (e1(), e2()) {
  | (Some(x), Some(y)) => p(x, y) && for_all2(p, e1, e2)
  | _ => true
  };

let rec exists2 = (p, e1, e2) =>
  switch (e1(), e2()) {
  | (Some(x), Some(y)) => p(x, y) || exists2(p, e1, e2)
  | _ => false
  };

let zip_with = (f, a, b) => {
  let stop = ref(false);
  () =>
    if (stop^) {
      None;
    } else {
      switch (a(), b()) {
      | (Some(xa), Some(xb)) => Some(f(xa, xb))
      | _ =>
        stop := true;
        None;
      };
    };
};

let zip = (a, b) => zip_with((x, y) => (x, y), a, b);

/*$Q
    (Q.list Q.small_int) (fun l -> \
      zip_with (fun x y->x,y) (of_list l) (of_list l) \
        |> unzip |> fst |> to_list = l)
  */

/*$R
    let e = Gen.zip_with (+) (Gen.repeat 1) (4--7) in
    OUnit.assert_equal [5;6;7;8] (Gen.to_list e);
  */

/** {3 Complex combinators} */;

module MergeState = {
  type t('a) = {
    gens: Queue.t(gen('a)),
    mutable state: my_state,
  }

  and my_state =
    | NewGen /* obtain a new generator and push it in queue */
    | YieldAndNew /* yield element from queue, then behave like NewGen */
    | Yield /* just yield elements from queue */
    | Stop; /* no more elements */
};

/* state machine starts at NewGen:
      NewGen: use next_gen to push a new gen into the queue
      Yield:
       while the queue is not empty:
         pop gen g from it
         if g is empty continue
         else:
           pop element x from g
           push g at back of queue
           yield x
      YieldAndNew: mix of Yield and NewGen.
       if next_gen is exhausted, goto Yield;
       if queue is empty, goto NewGen
      Stop: do nothing
   */
let merge = next_gen => {
  open MergeState;
  let state = {gens: Queue.create(), state: NewGen};
  /* recursive function to get next element */
  let rec next = () =>
    switch (state.state) {
    | Stop => None
    | Yield =>
      /* only yield from generators in state.gens */
      if (Queue.is_empty(state.gens)) {
        state.state = Stop;
        None;
      } else {
        let gen = Queue.pop(state.gens);
        switch (gen()) {
        | None => next()
        | Some(_) as res =>
          Queue.push(gen, state.gens); /* put gen back in queue */
          res;
        };
      }
    | NewGen =>
      switch (next_gen()) {
      | None =>
        state.state = Yield; /* exhausted */
        next();
      | Some(gen) =>
        Queue.push(gen, state.gens);
        state.state = YieldAndNew;
        next();
      }
    | YieldAndNew =>
      /* yield element from queue, then get a new generator */
      if (Queue.is_empty(state.gens)) {
        state.state = NewGen;
        next();
      } else {
        let gen = Queue.pop(state.gens);
        switch (gen()) {
        | None =>
          state.state = NewGen;
          next();
        | Some(_) as res =>
          Queue.push(gen, state.gens);
          state.state = NewGen;
          res;
        };
      }
    };
  next;
};

/*$T
    merge (of_list [of_list [1;3;5]; of_list [2;4;6]; of_list [7;8;9]]) \
      |> to_list |> List.sort Pervasives.compare = [1;2;3;4;5;6;7;8;9]
  */

/*$R
    let e = of_list [1--3; 4--6; 7--9] in
    let e' = merge e in
    OUnit.assert_equal [1;2;3;4;5;6;7;8;9]
      (to_list e' |> List.sort Pervasives.compare);
  */

let intersection = (~cmp=Pervasives.compare, gen1, gen2) => {
  let x1 = ref(gen1());
  let x2 = ref(gen2());
  let rec next = () =>
    switch (x1^, x2^) {
    | (Some(y1), Some(y2)) =>
      let c = cmp(y1, y2);
      if (c == 0) {
        /* equal elements, yield! */
        x1 := gen1();
        x2 := gen2();
        Some(y1);
      } else if (c < 0) {
        /* drop y1 */
        x1 := gen1();
        next();
      } else {
        /* drop y2 */
        x2 := gen2();
        next();
      };
    | _ => None
    };
  next;
};

/*$T
    intersection (of_list [1;1;2;3;4;8]) (of_list [1;2;4;5;6;7;8;9]) \
      |> to_list = [1;2;4;8]
  */

let sorted_merge = (~cmp=Pervasives.compare, gen1, gen2) => {
  let x1 = ref(gen1());
  let x2 = ref(gen2());
  () =>
    switch (x1^, x2^) {
    | (None, None) => None
    | (Some(y1) as r1, Some(y2) as r2) =>
      if (cmp(y1, y2) <= 0) {
        x1 := gen1();
        r1;
      } else {
        x2 := gen2();
        r2;
      }
    | (Some(_) as r, None) =>
      x1 := gen1();
      r;
    | (None, Some(_) as r) =>
      x2 := gen2();
      r;
    };
};

/*$T
    sorted_merge (of_list [1;2;2;3;5;10;100]) (of_list [2;4;5;6;11]) \
      |> to_list = [1;2;2;2;3;4;5;5;6;10;11;100]
  */

/*$R
    [Gen.of_list [1;3;5]; Gen.of_list [0;1;1;3;4;6;10]; Gen.of_list [2;2;11]]
      |> Gen.sorted_merge_n ?cmp:None
      |> Gen.to_list
      |> OUnit.assert_equal ~printer:pilist [0;1;1;1;2;2;3;3;4;5;6;10;11]
  */

/** {4 Mutable heap (taken from heap.ml to avoid dependencies)} */
module Heap = {
  /** A pairing tree heap with the given comparison function */

  type t('a) = {
    mutable tree: tree('a),
    cmp: ('a, 'a) => int,
  }

  and tree('a) =
    | Empty
    | Node('a, tree('a), tree('a));

  let empty = (~cmp) => {tree: Empty, cmp};

  let is_empty = h =>
    switch (h.tree) {
    | Empty => true
    | Node(_) => false
    };

  let rec union = (~cmp, t1, t2) =>
    switch (t1, t2) {
    | (Empty, _) => t2
    | (_, Empty) => t1
    | (
        [@implicit_arity] Node(x1, l1, r1),
        [@implicit_arity] Node(x2, l2, r2),
      ) =>
      if (cmp(x1, x2) <= 0) {
        [@implicit_arity] Node(x1, union(~cmp, t2, r1), l1);
      } else {
        [@implicit_arity] Node(x2, union(~cmp, t1, r2), l2);
      }
    };

  let insert = (h, x) =>
    h.tree =
      union(~cmp=h.cmp, [@implicit_arity] Node(x, Empty, Empty), h.tree);

  let pop = h =>
    switch (h.tree) {
    | Empty => raise(Not_found)
    | [@implicit_arity] Node(x, l, r) =>
      h.tree = union(~cmp=h.cmp, l, r);
      x;
    };
};

let sorted_merge_n = (~cmp=Pervasives.compare, l) => {
  /* make a heap of (value, generator) */
  let cmp = ((v1, _), (v2, _)) => cmp(v1, v2);
  let heap = Heap.empty(~cmp);
  /* add initial values */
  List.iter(
    gen' =>
      switch (gen'()) {
      | Some(x) => Heap.insert(heap, (x, gen'))
      | None => ()
      },
    l,
  );
  () =>
    if (Heap.is_empty(heap)) {
      None;
    } else {
      let (x, gen) = Heap.pop(heap);
      switch (gen()) {
      | Some(y) =>
        Heap.insert(heap, (y, gen)); /* insert next value */
        Some(x);
      | None => Some(x)
      }; /* gen empty, drop it */
    };
};

/*$T
    sorted_merge_n [of_list [1;2;2;3;5;10;100]; of_list [2;4;5;6;11]; (6--10)] \
      |> to_list = [1;2;2;2;3;4;5;5;6;6;7;8;9;10;10;11;100]
  */

let round_robin = (~n=2, gen) => {
  /* array of queues, together with their index */
  let qs = Array.init(n, _ => Queue.create());
  let cur = ref(0);
  /* get next element for the i-th queue */
  let rec next = i => {
    let q = qs[i];
    if (Queue.is_empty(q)) {
      update_to_i(i);
    } else {
      /* consume generator */
      Some(Queue.pop(q));
    };
  }
  /* consume [gen] until some element for [i]-th generator is
     available. */
  and update_to_i = i =>
    switch (gen()) {
    | None => None
    | Some(x) =>
      let j = cur^;
      cur := (j + 1) mod n; /* move cursor to next generator */
      let q = qs[j];
      if (j == i) {
        assert(Queue.is_empty(q));
        Some(x); /* return the element */
      } else {
        Queue.push(x, q);
        update_to_i(i); /* continue consuming [gen] */
      };
    };

  /* generators */
  let l = Array.mapi((i, _, ()) => next(i), qs);
  Array.to_list(l);
};

/*$T
    round_robin ~n:3 (1--12) |> List.map to_list = \
      [[1;4;7;10]; [2;5;8;11]; [3;6;9;12]]
  */

/*$R
    let e = Restart.round_robin ~n:2 Restart.(1--10) in
    match e with
    | [a;b] ->
      OUnit.assert_equal [1;3;5;7;9] (Gen.to_list a);
      OUnit.assert_equal [2;4;6;8;10] (Gen.to_list b)
    | _ -> OUnit.assert_failure "wrong list lenght"
  */

/*$R
    let e = Restart.round_robin ~n:3 Restart.(1 -- 999) in
    let l = List.map Gen.length e in
    OUnit.assert_equal [333;333;333] l;
  */

/* Duplicate the enum into [n] generators (default 2). The generators
   share the same underlying instance of the enum, so the optimal case is
   when they are consumed evenly */
let tee = (~n=2, gen) => {
  /* array of queues, together with their index */
  let qs = Array.init(n, _ => Queue.create());
  let finished = ref(false); /* is [gen] exhausted? */
  /* get next element for the i-th queue */
  let rec next = i =>
    if (Queue.is_empty(qs[i])) {
      if (finished^) {
        None;
      } else {
        get_next(i);
      };
    } else {
      /* consume generator */
      Queue.pop(qs[i]);
    }
  /* consume one more element */
  and get_next = i =>
    switch (gen()) {
    | Some(_) as res =>
      for (j in 0 to n - 1) {
        if (j != i) {
          Queue.push(res, qs[j]);
        };
      };
      res;
    | None =>
      finished := true;
      None;
    };

  /* generators */
  let l = Array.mapi((i, _, ()) => next(i), qs);
  Array.to_list(l);
};

/*$T
    tee ~n:3 (1--12) |> List.map to_list = \
      [to_list (1--12); to_list (1--12); to_list (1--12)]
  */

module InterleaveState = {
  type t('a) =
    | Only(gen('a))
    | Both(gen('a), gen('a), ref(bool))
    | Stop;
};

/* Yield elements from a and b alternatively */
let interleave = (gen_a, gen_b) => {
  open InterleaveState;
  let state = ref([@implicit_arity] Both(gen_a, gen_b, ref(true)));
  let rec next = () =>
    switch (state^) {
    | Stop => None
    | Only(g) =>
      switch (g()) {
      | None =>
        state := Stop;
        None;
      | Some(_) as res => res
      }
    | [@implicit_arity] Both(g1, g2, r) =>
      switch (
        if (r^) {
          g1();
        } else {
          g2();
        }
      ) {
      | None =>
        state :=
          (
            if (r^) {
              Only(g2);
            } else {
              Only(g1);
            }
          );
        next();
      | Some(_) as res =>
        r := ! r^; /* swap */
        res;
      }
    };
  next;
};

/*$T
    interleave (repeat 0) (1--5) |> take 10 |> to_list = \
      [0;1;0;2;0;3;0;4;0;5]
  */

/*$R
    let e1 = Gen.of_list [1;3;5;7;9] in
    let e2 = Gen.of_list [2;4;6;8;10] in
    let e = Gen.interleave e1 e2 in
    OUnit.assert_equal [1;2;3;4;5;6;7;8;9;10] (Gen.to_list e);
  */

module IntersperseState = {
  type t('a) =
    | Start
    | YieldElem(option('a))
    | YieldSep(option('a)) /* next val */
    | Stop;
};

/* Put [x] between elements of [enum] */
let intersperse = (x, gen) => {
  open IntersperseState;
  let state = ref(Start);
  let rec next = () =>
    switch (state^) {
    | Stop => None
    | YieldElem(res) =>
      switch (gen()) {
      | None => state := Stop
      | Some(_) as res' => state := YieldSep(res')
      };
      res;
    | YieldSep(res) =>
      state := YieldElem(res);
      Some(x);
    | Start =>
      switch (gen()) {
      | None =>
        state := Stop;
        None;
      | Some(_) as res =>
        state := YieldElem(res);
        next();
      }
    };
  next;
};

/*$T
    intersperse 0 (1--5) |> to_list = [1;0;2;0;3;0;4;0;5]
  */

/*$R
    let e = 1 -- 5 in
    let e' = Gen.intersperse 0 e in
    OUnit.assert_equal [1;0;2;0;3;0;4;0;5] (Gen.to_list e');
  */

/* Cartesian product */
let product = (gena, genb) => {
  let all_a = ref([]);
  let all_b = ref([]);
  /* cur: current state, i.e., what we have to do next. Can be stop,
     getLeft/getRight (to obtain next element from first/second generator),
     or prodLeft/prodRIght to compute the product of an element with a list
     of already met elements */
  let cur = ref(`GetLeft);
  let rec next = () =>
    switch (cur^) {
    | `Stop => None
    | `GetLeft =>
      switch (gena()) {
      | None => cur := `GetRightOrStop
      | Some(a) =>
        all_a := [a, ...all_a^];
        cur := `ProdLeft((a, all_b^));
      };
      next();
    | `GetRight
    | `GetRightOrStop =>
      /* TODO: test */
      switch (genb()) {
      | None when cur^ == `GetRightOrStop => cur := `Stop
      | None => cur := `GetLeft
      | Some(b) =>
        all_b := [b, ...all_b^];
        cur := `ProdRight((b, all_a^));
      };
      next();
    | `ProdLeft(_, []) =>
      cur := `GetRight;
      next();
    | `ProdLeft(x, [y, ...l]) =>
      cur := `ProdLeft((x, l));
      Some((x, y));
    | `ProdRight(_, []) =>
      cur := `GetLeft;
      next();
    | `ProdRight(y, [x, ...l]) =>
      cur := `ProdRight((y, l));
      Some((x, y));
    };

  next;
};

/*$T
    product (1--3) (of_list ["a"; "b"]) |> to_list \
      |> List.sort Pervasives.compare = \
        [1, "a"; 1, "b"; 2, "a"; 2, "b"; 3, "a"; 3, "b"]
  */

/*$R
    let printer = pi2list in
    let e = Gen.product (1--3) (4--5) in
    OUnit.assert_equal ~printer [1,4; 1,5; 2,4; 2,5; 3,4; 3,5]
      (List.sort Pervasives.compare (Gen.to_list e));
  */

/* Group equal consecutive elements together. */
let group = (~eq=(==), gen) =>
  switch (gen()) {
  | None => (() => None)
  | Some(x) =>
    let cur = ref([x]);
    let rec next = () => {
      /* try to get an element */
      let next_x =
        if (cur^ == []) {
          None;
        } else {
          gen();
        };
      switch (next_x, cur^) {
      | (None, []) => None
      | (None, l) =>
        cur := []; /* stop */
        Some(l);
      | (Some(x), [y, ..._]) when eq(x, y) =>
        cur := [x, ...cur^];
        next(); /* same group */
      | (Some(x), l) =>
        cur := [x];
        Some(l);
      };
    };
    next;
  };

/*$T
    group (of_list [0;0;0;1;0;2;2;3;4;5;5;5;5;10]) |> to_list = \
      [[0;0;0];[1];[0];[2;2];[3];[4];[5;5;5;5];[10]]
  */

let uniq = (~eq=(==), gen) => {
  open RunState;
  let state = ref(Init);
  let rec next = () =>
    switch (state^) {
    | Stop => None
    | Init =>
      switch (gen()) {
      | None =>
        state := Stop;
        None;
      | Some(x) as res =>
        state := Run(x);
        res;
      }
    | Run(x) =>
      switch (gen()) {
      | None =>
        state := Stop;
        None;
      | Some(y) as res =>
        if (eq(x, y)) {
          next();
        } else {
          /* ignore duplicate */
          state := Run(y);
          res;
        }
      }
    };
  next;
};

/*$T
    uniq (of_list [0;0;0;1;0;2;2;3;4;5;5;5;5;10]) |> to_list = \
      [0;1;0;2;3;4;5;10]
  */

let sort = (~cmp=Pervasives.compare, gen) => {
  /* build heap */
  let h = Heap.empty(~cmp);
  iter(Heap.insert(h), gen);
  () =>
    if (Heap.is_empty(h)) {
      None;
    } else {
      Some(Heap.pop(h));
    };
};
/*$T
    sort (of_list [0;0;0;1;0;2;2;3;4;5;5;5;-42;5;10]) |> to_list = \
      [-42;0;0;0;0;1;2;2;3;4;5;5;5;5;10]
  */

/* NOTE: using a set is not really possible, because once we have built the
   set there is no simple way to iterate on it */
let sort_uniq = (~cmp=Pervasives.compare, gen) =>
  uniq(~eq=(x, y) => cmp(x, y) == 0, sort(~cmp, gen));

/*$T
    sort_uniq (of_list [0;0;0;1;0;2;2;3;4;5;42;5;5;42;5;10]) |> to_list = \
      [0;1;2;3;4;5;10;42]
  */

let chunks = (n, e) => {
  let rec next = () =>
    switch (e()) {
    | None => None
    | Some(x) =>
      let a = Array.make(n, x);
      fill(a, 1);
    }

  and fill = (a, i) =>
    /* fill the array. [i]: current index to fill */
    if (i == n) {
      Some(a);
    } else {
      switch (e()) {
      | None => Some(Array.sub(a, 0, i)) /* last array is not full */
      | Some(x) =>
        a[i] = x;
        fill(a, i + 1);
      };
    };

  next;
};

/*$T
    chunks 25 (0--100) |> map Array.to_list |> to_list = \
      List.map to_list [(0--24); (25--49);(50--74);(75--99);(100--100)]
  */

/*$Q
    Q.(list int) (fun l -> \
      of_list l |> chunks 25 |> flat_map of_array |> to_list = l)
  */

/* state of the permutation machine. One machine manages one element [x],
      and depends on a deeper machine [g] that generates permutations of the
      list minus this element (down to the empty list).
      The machine can do two things:
       - insert the element in the current list of [g], at any position
       - obtain the next list of [g]
   */

/* TODO: check https://en.wikipedia.org/wiki/Heap's_algorithm , might be better */

module PermState = {
  type state('a) =
    | Done
    | Base /* bottom machine, yield [] */
    | Insert(insert_state('a))
  and insert_state('a) = {
    x: 'a,
    mutable l: list('a),
    mutable n: int, /* idx for insertion */
    len: int, /* len of [l] */
    sub: t('a),
  }
  and t('a) = {mutable st: state('a)};
};

let permutations = g => {
  open PermState;
  /* make a machine for n elements. Invariant: n=len(l) */
  let rec make_machine = (n, l) =>
    switch (l) {
    | [] =>
      assert(n == 0);
      {st: Base};
    | [x, ...tail] =>
      let sub = make_machine(n - 1, tail);
      let st =
        switch (next(sub, ())) {
        | None => Done
        | Some(l) => Insert({x, n: 0, l, len: n, sub})
        };

      {st: st};
    }
  /* next element of the machine */
  and next = (m, ()) =>
    switch (m.st) {
    | Done => None
    | Base =>
      m.st = Done;
      Some([]);
    | Insert({x, len, n, l, sub} as state) =>
      if (n == len) {
        switch (next(sub, ())) {
        | None =>
          m.st = Done;
          None;
        | Some(l) =>
          state.l = l;
          state.n = 0;
          next(m, ());
        };
      } else {
        state.n = state.n + 1;
        Some(insert(x, n, l));
      }
    }
  and insert = (x, n, l) =>
    switch (n, l) {
    | (0, _) => [x, ...l]
    | (_, []) => assert(false)
    | (_, [y, ...tail]) => [y, ...insert(x, n - 1, tail)]
    };

  let l = fold((acc, x) => [x, ...acc], [], g);
  next(make_machine(List.length(l), l));
};

/*$T permutations
    permutations (1--3) |> to_list |> List.sort Pervasives.compare = \
      [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]]
    permutations empty |> to_list = [[]]
    permutations (singleton 1) |> to_list = [[1]]
  */

/*
 Credits to Bernardo Freitas Paulo da Costa for [permutations_heap]!

 B.R.Heap's algorithm for permutations,
 cf http://en.wikipedia.org/wiki/Heap%27s_algorithm.

 Continuation-based recursive formula, model for the state manipulations
 below:
    {[
      let rec heap_perm k a n =
        match n with
        | 0 -> k a
        | n ->
            for i = 0 to n-1 do
              heap_perm k a (n-1);
              let j = (if n mod 2 = 1 then 0 else i) in
              let t = a.(j) in
              a.(j) <- a.(n-1);
              a.(n-1) <- t
            done
    ]}
 */

/* The state of the permutation machine, containing
      - the array [a] we're permuting, in the "current permutation";
      - the level of recursion [n]: we can permute elements with index < [n]
      - the stack of values of indices to permute [i] in the list [is]
      The permutation stops when we have no more elements in the stack [is].
   */
module HeapPermState = {
  type state('a) = {
    elts: array('a),
    mutable n: int,
    mutable is: list(int),
  };
};

let permutations_heap = g => {
  open HeapPermState;
  let l = fold((acc, x) => [x, ...acc], [], g);
  let a = Array.of_list(l);
  let rec next = (st, ()) =>
    switch (st.n) {
    | 0 =>
      switch (st.is) {
      | []
      | [_] => assert(false)
      | [0, i, ...is'] =>
        /* "Pop state" before returning next element */
        st.is = [i + 1, ...is'];
        st.n = 1;
        Some(Array.copy(a));
      | [_, _, ..._] => assert(false)
      }
    | n =>
      switch (st.is) {
      | [] => None
      | [i, ...is'] when i == n =>
        /* Pop state at end of loop */
        st.is = is';
        st.n = n + 1;
        switch (st.is) {
        | [] => None /* last loop */
        | [i, ...is'] =>
          let j =
            if (st.n mod 2 == 1) {
              0;
            } else {
              i;
            };
          let tmp = st.elts[j];
          st.elts[j] = st.elts[n];
          st.elts[n] = tmp;
          st.is = [i + 1, ...is'];
          next(st, ());
        };
      | [_, ..._] =>
        /* Recurse down and start new loop */
        st.n = n - 1;
        st.is = [0, ...st.is];
        next(st, ());
      }
    };

  let n = Array.length(a);
  if (n == 0) {
    empty;
  } else {
    next({elts: a, n, is: [0]});
  };
};

/*$T permutations_heap
    permutations_heap (1--3) |> to_list |> List.sort Pervasives.compare = \
      [[|1;2;3|]; [|1;3;2|]; [|2;1;3|]; [|2;3;1|]; [|3;1;2|]; [|3;2;1|]]
    permutations_heap empty |> to_list = []
    permutations_heap (singleton 1) |> to_list = [[|1|]]
  */

module CombState = {
  type state('a) =
    | Done
    | Base
    | Add('a, t('a), t('a)) /* add x at beginning of first; then switch to second */
    | Follow(t('a)) /* just forward */
  and t('a) = {mutable st: state('a)};
};

let combinations = (n, g) => {
  open CombState;
  assert(n >= 0);
  let rec make_state = (n, l) =>
    switch (n, l) {
    | (0, _) => {st: Base}
    | (_, []) => {st: Done}
    | (_, [x, ...tail]) =>
      let m1 = make_state(n - 1, tail);
      let m2 = make_state(n, tail);
      {st: [@implicit_arity] Add(x, m1, m2)};
    }
  and next = (m, ()) =>
    switch (m.st) {
    | Done => None
    | Base =>
      m.st = Done;
      Some([]);
    | Follow(m) =>
      switch (next(m, ())) {
      | None =>
        m.st = Done;
        None;
      | Some(_) as res => res
      }
    | [@implicit_arity] Add(x, m1, m2) =>
      switch (next(m1, ())) {
      | None =>
        m.st = Follow(m2);
        next(m, ());
      | Some(l) => Some([x, ...l])
      }
    };

  let l = fold((acc, x) => [x, ...acc], [], g);
  next(make_state(n, l));
};

/*$T
    combinations 2 (1--4) |> map (List.sort Pervasives.compare) \
      |> to_list |> List.sort Pervasives.compare = \
      [[1;2]; [1;3]; [1;4]; [2;3]; [2;4]; [3;4]]
    combinations 0 (1--4) |> to_list = [[]]
    combinations 1 (singleton 1) |> to_list = [[1]]
  */

module PowerSetState = {
  type state('a) =
    | Done
    | Base
    | Add('a, t('a)) /* add x before any result of m */
    | AddTo(list('a), 'a, t('a)) /* yield x::list, then back to Add(x,m) */
  and t('a) = {mutable st: state('a)};
};

let power_set = g => {
  open PowerSetState;
  let rec make_state = l =>
    switch (l) {
    | [] => {st: Base}
    | [x, ...tail] =>
      let m = make_state(tail);
      {st: [@implicit_arity] Add(x, m)};
    }
  and next = (m, ()) =>
    switch (m.st) {
    | Done => None
    | Base =>
      m.st = Done;
      Some([]);
    | [@implicit_arity] Add(x, m') =>
      switch (next(m', ())) {
      | None =>
        m.st = Done;
        None;
      | Some(l) as res =>
        m.st = [@implicit_arity] AddTo(l, x, m');
        res;
      }
    | [@implicit_arity] AddTo(l, x, m') =>
      m.st = [@implicit_arity] Add(x, m');
      Some([x, ...l]);
    };

  let l = fold((acc, x) => [x, ...acc], [], g);
  next(make_state(l));
};

/*$T
    power_set (1--3) |> map (List.sort Pervasives.compare) \
      |> to_list |> List.sort Pervasives.compare = \
      [[]; [1]; [1;2]; [1;2;3]; [1;3]; [2]; [2;3]; [3]]
    power_set empty |> to_list = [[]]
    power_set (singleton 1) |> map (List.sort Pervasives.compare) \
      |> to_list |> List.sort Pervasives.compare = [[]; [1]]
  */

/** {3 Conversion} */;

let of_list = l => {
  let l = ref(l);
  () =>
    switch (l^) {
    | [] => None
    | [x, ...l'] =>
      l := l';
      Some(x);
    };
};

let to_rev_list = gen => fold((acc, x) => [x, ...acc], [], gen);

/*$Q
    (Q.list Q.small_int) (fun l -> \
      to_rev_list (of_list l) = List.rev l)
  */

let to_list = gen => List.rev(to_rev_list(gen));

let to_array = gen => {
  let l = to_rev_list(gen);
  switch (l) {
  | [] => [||]
  | _ =>
    let a = Array.of_list(l);
    let n = Array.length(a);
    /* reverse array */
    for (i in 0 to (n - 1) / 2) {
      let tmp = a[i];
      a[i] = a[n - i - 1];
      a[n - i - 1] = tmp;
    };
    a;
  };
};

let of_array = (~start=0, ~len=?, a) => {
  let len =
    switch (len) {
    | None => Array.length(a) - start
    | Some(n) =>
      assert(n + start < Array.length(a));
      n;
    };
  let i = ref(start);
  () =>
    if (i^ >= start + len) {
      None;
    } else {
      let x = a[i^];
      incr(i);
      Some(x);
    };
};

/*$Q
    (Q.array Q.small_int) (fun a -> \
      of_array a |> to_array = a)
  */

let of_string = (~start=0, ~len=?, s) => {
  let len =
    switch (len) {
    | None => String.length(s) - start
    | Some(n) =>
      assert(n + start < String.length(s));
      n;
    };
  let i = ref(start);
  () =>
    if (i^ >= start + len) {
      None;
    } else {
      let x = s.[i^];
      incr(i);
      Some(x);
    };
};

let to_buffer = (buf, g) => iter(Buffer.add_char(buf), g);

let to_string = s => {
  let buf = Buffer.create(16);
  to_buffer(buf, s);
  Buffer.contents(buf);
};

let rand_int = i => repeatedly(() => Random.int(i));

let int_range = (~step=1, i, j) => {
  if (step == 0) {
    raise(Invalid_argument("Gen.int_range"));
  };
  let (>) =
    if (step > 0) {
      (>);
    } else {
      (<);
    };
  let r = ref(i);
  () => {
    let x = r^;
    if (x > j) {
      None;
    } else {
      r := r^ + step;
      Some(x);
    };
  };
};

/*$= & ~printer:Q.Print.(list int)
    [1;2;3;4] (int_range 1 4 |> to_list)
    [4;3;2;1] (int_range ~step:~-1 4 1 |> to_list)
    [6;4;2] (int_range 6 1 ~step:~-2 |> to_list)
    [] (int_range 4 1 |> to_list)
  */

let lines = g => {
  let buf = Buffer.create(32);
  let stop = ref(false);
  let rec next = () =>
    if (stop^) {
      None;
    } else {
      switch (g()) {
      | None =>
        stop := true;
        /* only return a non-empty line */
        if (Buffer.length(buf) == 0) {
          None;
        } else {
          Some(Buffer.contents(buf));
        };
      | Some('\n') =>
        let s = Buffer.contents(buf);
        Buffer.clear(buf);
        Some(s);
      | Some(c) =>
        Buffer.add_char(buf, c);
        next();
      };
    };

  next;
};

/*$= & ~printer:Q.Print.(list string)
    ["abc"; "de"; ""] (lines (of_string "abc\nde\n\n") |> to_list)
  */

let unlines = g => {
  let st = ref(`Next);
  () =>
    switch (st^) {
    | `Stop => None
    | `Next =>
      switch (g()) {
      | None =>
        st := `Stop;
        None;
      | Some("") => Some('\n') /* empty line */
      | Some(s) =>
        st := `Consume((s, 1));
        Some(s.[0]);
      }
    | `Consume(s, i) when i == String.length(s) =>
      st := `Next;
      Some('\n');
    | `Consume(s, i) =>
      st := `Consume((s, i + 1));
      Some(s.[i]);
    };
};

/*$Q
    Q.printable_string (fun s -> \
      of_string s |> lines |> unlines |> to_string |> String.trim = String.trim s)
  */

let pp =
    (
      ~start="",
      ~stop="",
      ~sep=",",
      ~horizontal=false,
      pp_elem,
      formatter,
      gen,
    ) => {
  if (horizontal) {
    Format.pp_open_hbox(formatter, ());
  } else {
    Format.pp_open_hvbox(formatter, 0);
  };
  Format.pp_print_string(formatter, start);
  let rec next = is_first =>
    switch (gen()) {
    | Some(x) =>
      if (!is_first) {
        Format.pp_print_string(formatter, sep);
        Format.pp_print_space(formatter, ());
        pp_elem(formatter, x);
      } else {
        pp_elem(formatter, x);
      };
      next(false);
    | None => ()
    };

  next(true);
  Format.pp_print_string(formatter, stop);
  Format.pp_close_box(formatter, ());
};

module Infix = {
  let (--) = int_range(~step=1);

  let (>>=) = (x, f) => flat_map(f, x);
  let (>>|) = (x, f) => map(f, x);
  let (>|=) = (x, f) => map(f, x);
};

include Infix;

module Restart = {
  type t('a) = unit => gen('a);

  type restartable('a) = t('a);

  let lift = (f, e) => f(e());
  let lift2 = (f, e1, e2) => f(e1(), e2());

  let empty = () => empty;

  let singleton = (x, ()) => singleton(x);

  let return = singleton;

  let iterate = (x, f, ()) => iterate(x, f);

  let repeat = (x, ()) => repeat(x);

  let unfold = (f, acc, ()) => unfold(f, acc);

  let init = (~limit=?, f, ()) => init(~limit?, f);

  let cycle = enum => {
    assert(!is_empty(enum()));
    () => {
      let gen = ref(enum()); /* start cycle */
      let rec next = () =>
        switch (gen^()) {
        | Some(_) as res => res
        | None =>
          gen := enum();
          next();
        };
      next;
    };
  };

  let is_empty = e => is_empty(e());

  let fold = (f, acc, e) => fold(f, acc, e());

  let reduce = (f, e) => reduce(f, e());

  let scan = (f, acc, e, ()) => scan(f, acc, e());

  let unfold_scan = (f, acc, e, ()) => unfold_scan(f, acc, e());

  let iter = (f, e) => iter(f, e());

  let iteri = (f, e) => iteri(f, e());

  let length = e => length(e());

  let map = (f, e, ()) => map(f, e());

  let mapi = (f, e, ()) => mapi(f, e());

  let fold_map = (f, s, e, ()) => fold_map(f, s, e());

  let append = (e1, e2, ()) => append(e1(), e2());

  let flatten = (e, ()) => flatten(e());

  let flat_map = (f, e, ()) => flat_map(f, e());

  let mem = (~eq=?, x, e) => mem(~eq?, x, e());

  let take = (n, e, ()) => take(n, e());

  let drop = (n, e, ()) => drop(n, e());

  let nth = (n, e) => nth(n, e());

  let take_nth = (n, e, ()) => take_nth(n, e());

  let filter = (p, e, ()) => filter(p, e());

  let take_while = (p, e, ()) => take_while(p, e());

  let fold_while = (f, s, e) => fold_while(f, s, e());

  let drop_while = (p, e, ()) => drop_while(p, e());

  let filter_map = (f, e, ()) => filter_map(f, e());

  let zip_with = (f, e1, e2, ()) => zip_with(f, e1(), e2());

  let zip = (e1, e2, ()) => zip(e1(), e2());

  let zip_index = (e, ()) => zip_index(e());

  let unzip = e => (map(fst, e), map(snd, e));

  let partition = (p, e) => (filter(p, e), filter(x => !p(x), e));

  let for_all = (p, e) => for_all(p, e());

  let exists = (p, e) => exists(p, e());

  let for_all2 = (p, e1, e2) => for_all2(p, e1(), e2());

  let exists2 = (p, e1, e2) => exists2(p, e1(), e2());

  let map2 = (f, e1, e2, ()) => map2(f, e1(), e2());

  let iter2 = (f, e1, e2) => iter2(f, e1(), e2());

  let fold2 = (f, acc, e1, e2) => fold2(f, acc, e1(), e2());

  let min = (~lt=?, e) => min(~lt?, e());

  let max = (~lt=?, e) => max(~lt?, e());

  let ___eq = eq;
  let eq = (~eq=?, e1, e2) => ___eq(~eq?, e1(), e2());

  let lexico = (~cmp=?, e1, e2) => lexico(~cmp?, e1(), e2());

  let compare = (~cmp=?, e1, e2) => compare(~cmp?, e1(), e2());

  let sum = e => sum(e());

  let find = (f, e) => find(f, e());

  let merge = (e, ()) => merge(e());

  let intersection = (~cmp=?, e1, e2, ()) => intersection(~cmp?, e1(), e2());

  let sorted_merge = (~cmp=?, e1, e2, ()) => sorted_merge(~cmp?, e1(), e2());

  let sorted_merge_n = (~cmp=?, l, ()) =>
    sorted_merge_n(~cmp?, List.map(g => g(), l));

  let tee = (~n=?, e) => tee(~n?, e());

  let round_robin = (~n=?, e) => round_robin(~n?, e());

  let interleave = (e1, e2, ()) => interleave(e1(), e2());

  let intersperse = (x, e, ()) => intersperse(x, e());

  let product = (e1, e2, ()) => product(e1(), e2());

  let group = (~eq=?, e, ()) => group(~eq?, e());

  let uniq = (~eq=?, e, ()) => uniq(~eq?, e());

  let sort = (~cmp=Pervasives.compare, enum, ()) => sort(~cmp, enum());

  let sort_uniq = (~cmp=Pervasives.compare, e) => {
    let e' = sort(~cmp, e);
    uniq(~eq=(x, y) => cmp(x, y) == 0, e');
  };

  let chunks = (n, e, ()) => chunks(n, e());

  let permutations = (g, ()) => permutations(g());

  let permutations_heap = (g, ()) => permutations_heap(g());

  let combinations = (n, g, ()) => combinations(n, g());

  let power_set = (g, ()) => power_set(g());

  let of_list = (l, ()) => of_list(l);

  let to_rev_list = e => to_rev_list(e());

  let to_list = e => to_list(e());

  let to_array = e => to_array(e());

  let of_array = (~start=?, ~len=?, a, ()) => of_array(~start?, ~len?, a);

  let of_string = (~start=?, ~len=?, s, ()) => of_string(~start?, ~len?, s);

  let to_string = s => to_string(s());

  let to_buffer = (buf, s) => to_buffer(buf, s());

  let rand_int = (i, ()) => rand_int(i);

  let int_range = (~step=?, i, j, ()) => int_range(~step?, i, j);

  let lines = (g, ()) => lines(g());
  let unlines = (g, ()) => unlines(g());

  module Infix = {
    let (--) = int_range(~step=1);

    let (>>=) = (x, f) => flat_map(f, x);
    let (>>|) = (x, f) => map(f, x);
    let (>|=) = (x, f) => map(f, x);
  };

  include Infix;

  let pp = (~start=?, ~stop=?, ~sep=?, ~horizontal=?, pp_elem, fmt, e) =>
    pp(~start?, ~stop?, ~sep?, ~horizontal?, pp_elem, fmt, e());

  let of_gen = (~caching=?, ~max_chunk_size=?, g) => {
    let cached = ref(None);
    () =>
      switch (cached^) {
      | Some(mlist) => GenMList.to_gen(mlist)
      | None =>
        let mlist = GenMList.of_gen_lazy(~caching?, ~max_chunk_size?, g);
        cached := Some(mlist);
        GenMList.to_gen(mlist);
      };
  };
};

/** {2 Generator functions} */;

let start = g => g();

/** Store content of the generator in an enum */

let persistent = gen => {
  let l = GenMList.of_gen(gen);
  () => GenMList.to_gen(l);
};

/*$T
    let g = 1--10 in let g' = persistent g in \
      Restart.to_list g' = Restart.to_list g'
    let g = 1--10 in let g' = persistent g in \
      Restart.to_list g' = [1;2;3;4;5;6;7;8;9;10]
  */

/*$R
    let i = ref 0 in
    let gen () =
      let j = !i in
      if j > 5 then None else (incr i; Some j)
    in
    let e = Gen.persistent gen in
    OUnit.assert_equal [0;1;2;3;4;5] (Restart.to_list e);
    OUnit.assert_equal [0;1;2;3;4;5] (Restart.to_list e);
    OUnit.assert_equal [0;1;2;3;4;5] (Restart.to_list e);
  */

let persistent_lazy = (~caching=?, ~max_chunk_size=?, gen) => {
  let l = GenMList.of_gen_lazy(~caching?, ~max_chunk_size?, gen);
  () => GenMList.to_gen(l);
};

/*$T
    let g = 1--1_000_000_000 in let g' = persistent_lazy g in \
      (g' () |> take 100 |> to_list = (1--100 |> to_list)) && \
      (g' () |> take 200 |> to_list = (1--200 |> to_list))
  */

let peek = g => {
  let state = ref(`Start);
  let rec next = () =>
    switch (state^) {
    | `Stop => None
    | `At(x) =>
      switch (g()) {
      | None =>
        state := `Stop;
        Some((x, None));
      | Some(y) as res =>
        state := `At(y);
        Some((x, res));
      }
    | `Start =>
      switch (g()) {
      | None =>
        state := `Stop;
        None;
      | Some(x) =>
        state := `At(x);
        next();
      }
    };

  next;
};

/*$= & ~printer:Q.Print.(list (pair int (option int)))
    [] (peek (of_list []) |> to_list)
    [1, Some 2; 2, Some 3; 3, Some 4; 4, None] (peek (1 -- 4) |> to_list)
  */

/*$Q
  Q.(list int) (fun l -> \
    l = [] || (of_list l |> peek |> filter_map snd |> to_list = List.tl l))
  */

let queue_to_array_ = q =>
  if (Queue.is_empty(q)) {
    [||];
  } else {
    let x = Queue.peek(q);
    let a = Array.make(Queue.length(q), x);
    let i = ref(0);
    Queue.iter(
      x => {
        a[i^] = x;
        incr(i);
      },
      q,
    );
    a;
  };

let peek_n = (n, g) => {
  if (n < 1) {
    invalid_arg("peek_n");
  };
  let state = ref(`Start);
  let q = Queue.create();
  let rec next = () =>
    switch (state^) {
    | `Start =>
      fill(n);
      state :=
        (
          if (Queue.is_empty(q)) {
            `Stop;
          } else {
            `Continue;
          }
        );
      next();
    | `Continue =>
      assert(!Queue.is_empty(q));
      let x = Queue.pop(q);
      fill(1);
      state :=
        (
          if (Queue.is_empty(q)) {
            `Stop;
          } else {
            `Continue;
          }
        );
      Some((x, queue_to_array_(q)));
    | `Stop => None
    }
  /* add [n] elements to [f] if possible */
  and fill = i => {
    assert(i + Queue.length(q) <= n);
    if (i > 0) {
      switch (g()) {
      | None => ()
      | Some(x) =>
        Queue.push(x, q);
        fill(i - 1);
      };
    };
  };

  next;
};

/*$= & ~printer:Q.Print.(list (pair int (array int)))
    [] (peek_n 1 (of_list []) |> to_list)
    [1, [|2;3|]; 2, [|3;4|]; 3, [|4|]; 4, [||]] (peek_n 2 (1 -- 4) |> to_list)
    [1, [|2;3;4|]; 2, [|3;4;5|]; 3, [|4;5|]; 4, [|5|]; 5,[||]] \
      (peek_n 3 (1 -- 5) |> to_list)
  */

/*$QR
    Q.(list small_int)
      (fun l ->
        let l' =
          of_list l
          |> peek_n 10
          |> filter_map (fun (_,a) -> if a=[||] then None else Some a.(0))
          |> to_list
        in
        l = [] || l' = List.tl l)
  */

/** {2 Basic IO} */;

module IO = {
  let with_file_in = (~mode=0o644, ~flags=[], filename, f) => {
    let ic = open_in_gen(flags, mode, filename);
    try({
      let x = f(ic);
      close_in_noerr(ic);
      x;
    }) {
    | e =>
      close_in_noerr(ic);
      raise(e);
    };
  };

  let with_in = (~mode=?, ~flags=?, filename, f) =>
    with_file_in(
      ~mode?,
      ~flags?,
      filename,
      ic => {
        let next = () =>
          try(Some(input_char(ic))) {
          | End_of_file => None
          };

        f(next);
      },
    );

  let with_lines = (~mode=?, ~flags=?, filename, f) =>
    with_file_in(
      ~mode?,
      ~flags?,
      filename,
      ic => {
        let next = () =>
          try(Some(input_line(ic))) {
          | End_of_file => None
          };

        f(next);
      },
    );

  let with_file_out =
      (~mode=0o644, ~flags=[Open_creat, Open_wronly], filename, f) => {
    let oc = open_out_gen(flags, mode, filename);
    try({
      let x = f(oc);
      close_out(oc);
      x;
    }) {
    | e =>
      close_out_noerr(oc);
      raise(e);
    };
  };

  let write_str = (~mode=?, ~flags=?, ~sep="", filename, g) =>
    with_file_out(~mode?, ~flags?, filename, oc =>
      iteri(
        (i, s) => {
          if (i > 0) {
            output_string(oc, sep);
          };
          output_string(oc, s);
        },
        g,
      )
    );

  let write = (~mode=?, ~flags=?, filename, g) =>
    with_file_out(~mode?, ~flags?, filename, oc =>
      iter(c => output_char(oc, c), g)
    );

  let write_lines = (~mode=?, ~flags=?, filename, g) =>
    with_file_out(~mode?, ~flags?, filename, oc =>
      iter(
        s => {
          output_string(oc, s);
          output_char(oc, '\n');
        },
        g,
      )
    );
};
