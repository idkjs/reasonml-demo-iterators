/* This file is free software, part of iter. See file "license" for more details. */

/* {1 Simple and Efficient Iterators} */

/* The iterators are designed to allow easy transfer (mappings) between data
       structures, without defining [n^2] conversions between the [n] types. The
       implementation relies on the assumption that an iterator can be iterated
       on as many times as needed; this choice allows for high performance
       of many combinators. However, for transient iterators, the {!persistent}
       function is provided, storing elements of a transient iterator
       in memory; the iterator can then be used several times (See further).

       Note that some combinators also return iterators (e.g. {!group}). The
       transformation is computed on the fly every time one iterates over
       the resulting iterator. If a transformation performs heavy computation,
       {!persistent} can also be used as intermediate storage.

       Most functions are {b lazy}, i.e. they do not actually use their arguments
       until their result is iterated on. For instance, if one calls {!map}
       on an iterator, one gets a new iterator, but nothing else happens until
       this new iterator is used (by folding or iterating on it).

       If an iterator is built from an iteration function that is {b repeatable}
       (i.e. calling it several times always iterates on the same set of
       elements, for instance List.iter or Map.iter), then
       the resulting {!t} object is also repeatable. For {b one-time iter functions}
       such as iteration on a file descriptor or a {!Stream},
       the {!persistent} function can be used to iterate and store elements in
       a memory structure; the result is an iterator that iterates on the elements
       of this memory structure, cheaply and repeatably.

   */

type t(+'a) = ('a => unit) => unit;
/* An iterator of values of type ['a]. If you give it a function ['a -> unit]
   it will be applied to every element of the iterator successively. */

type iter(+'a) = t('a);

/* {b NOTE} Type [('a, 'b) t2 = ('a -> 'b -> unit) -> unit]
       has been removed and subsumed by [('a * 'b) t]
       @since 1.0
   */

type equal('a) = ('a, 'a) => bool;
type hash('a) = 'a => int;

/* {1 Creation} */

let from_iter: (('a => unit) => unit) => t('a);
/* Build an iterator from a iter function */

let from_labelled_iter: ((~f: 'a => unit) => unit) => t('a);
/* Build an iterator from a labelled iter function
   @since 1.2 */

let from_fun: (unit => option('a)) => t('a);
/* Call the function repeatedly until it returns None. This
   iterator is transient, use {!persistent} if needed! */

let empty: t('a);
/* Empty iterator. It contains no element. */

let singleton: 'a => t('a);
/* Singleton iterator, with exactly one element. */

let doubleton: ('a, 'a) => t('a);
/* Iterator with exactly two elements */

let init: (int => 'a) => t('a);
/* [init f] is the infinite iterator [f 0; f 1; f 2; …].
   @since 0.9 */

let cons: ('a, t('a)) => t('a);
/* [cons x l] yields [x], then yields from [l].
   Same as [append (singleton x) l] */

let snoc: (t('a), 'a) => t('a);
/* Same as {!cons} but yields the element after iterating on [l] */

let return: 'a => t('a);
/* Synonym to {!singleton} */

let pure: 'a => t('a);
/* Synonym to {!singleton} */

let repeat: 'a => t('a);
/* Infinite iterator of the same element. You may want to look
   at {!take} and the likes if you iterate on it. */

let iterate: ('a => 'a, 'a) => t('a);
/* [iterate f x] is the infinite iterator [x, f(x), f(f(x)), ...] */

let forever: (unit => 'b) => t('b);
/* Iterator that calls the given function to produce elements.
   The iterator may be transient (depending on the function), and definitely
   is infinite. You may want to use {!take} and {!persistent}. */

let cycle: t('a) => t('a);
/* Cycle forever through the given iterator. Assume the given iterator can
   be traversed any amount of times (not transient).  This yields an
   infinite iterator, you should use something like {!take} not to loop
   forever. */

let unfoldr: ('b => option(('a, 'b)), 'b) => t('a);
/* [unfoldr f b] will apply [f] to [b]. If it
   yields [Some (x,b')] then [x] is returned
   and unfoldr recurses with [b']. */

let scan: (('b, 'a) => 'b, 'b, t('a)) => t('b);
/* Iterator of intermediate results */

/* {1 Consumption} */

let iter: ('a => unit, t('a)) => unit;
/* Consume the iterator, passing all its arguments to the function.
   Basically [iter f seq] is just [seq f]. */

let iteri: ((int, 'a) => unit, t('a)) => unit;
/* Iterate on elements and their index in the iterator */

let fold: (('a, 'b) => 'a, 'a, t('b)) => 'a;
/* Fold over elements of the iterator, consuming it */

let foldi: (('a, int, 'b) => 'a, 'a, t('b)) => 'a;
/* Fold over elements of the iterator and their index, consuming it */

let fold_map: (('acc, 'a) => ('acc, 'b), 'acc, t('a)) => t('b);
/* [fold_map f acc l] is like {!map}, but it carries some state as in
   {!fold}. The state is not returned, it is just used to thread some
   information to the map function.
   @since 0.9 */

let fold_filter_map:
  (('acc, 'a) => ('acc, option('b)), 'acc, t('a)) => t('b);
/* [fold_filter_map f acc l] is a {!fold_map}-like function, but the
   function can choose to skip an element by retuning [None].
   @since 0.9 */

let map: ('a => 'b, t('a)) => t('b);
/* Map objects of the iterator into other elements, lazily */

let mapi: ((int, 'a) => 'b, t('a)) => t('b);
/* Map objects, along with their index in the iterator */

let map_by_2: (('a, 'a) => 'a, t('a)) => t('a);
/* Map objects two by two. lazily.
   The last element is kept in the iterator if the count is odd.
   @since 0.7 */

let for_all: ('a => bool, t('a)) => bool;
/* Do all elements satisfy the predicate? */

let exists: ('a => bool, t('a)) => bool;
/* Exists there some element satisfying the predicate? */

let mem: (~eq: ('a, 'a) => bool=?, 'a, t('a)) => bool;
/* Is the value a member of the iterator?
   @param eq the equality predicate to use (default [(=)])
   @since 0.5 */

let find: ('a => option('b), t('a)) => option('b);
/* Find the first element on which the function doesn't return [None]
   @since 0.5 */

let find_map: ('a => option('b), t('a)) => option('b);
/* Alias to {!find}
   @since 0.10 */

let findi: ((int, 'a) => option('b), t('a)) => option('b);
/* Indexed version of {!find}
   @since 0.9 */

let find_mapi: ((int, 'a) => option('b), t('a)) => option('b);
/* Alias to {!findi}
   @since 0.10 */

let find_pred: ('a => bool, t('a)) => option('a);
/* [find_pred p l] finds the first element of [l] that satisfies [p],
   or returns [None] if no element satisfies [p]
   @since 0.9 */

let find_pred_exn: ('a => bool, t('a)) => 'a;
/* Unsafe version of {!find_pred}
   @raise Not_found if no such element is found
   @since 0.9 */

let length: t('a) => int;
/* How long is the iterator? Forces the iterator. */

let is_empty: t('a) => bool;
/* Is the iterator empty? Forces the iterator. */

/* {1 Transformation} */

let filter: ('a => bool, t('a)) => t('a);
/* Filter on elements of the iterator */

let append: (t('a), t('a)) => t('a);
/* Append two iterators. Iterating on the result is like iterating
   on the first, then on the second. */

let append_l: list(t('a)) => t('a);
/* Append iterators. Iterating on the result is like iterating
   on the each iterator of the list in order.
   @since 0.11 */

let concat: t(t('a)) => t('a);
/* Concatenate an iterator of iterators into one iterator. */

let flatten: t(t('a)) => t('a);
/* Alias for {!concat} */

let flat_map: ('a => t('b), t('a)) => t('b);
/* Monadic bind. Intuitively, it applies the function to every
   element of the initial iterator, and calls {!concat}.
   Formerly [flatMap]
   @since 0.5 */

let flat_map_l: ('a => list('b), t('a)) => t('b);
/* Convenience function combining {!flat_map} and {!of_list}
   @since 0.9 */

let seq_list: list(t('a)) => t(list('a));
/* [seq_list l] returns all the ways to pick one element in each sub-iterator
   in [l]. Assumes the sub-iterators can be iterated on several times.
   @since 0.11 */

let seq_list_map: ('a => t('b), list('a)) => t(list('b));
/* [seq_list_map f l] maps [f] over every element of [l],
   then calls {!seq_list}
   @since 0.11 */

let filter_map: ('a => option('b), t('a)) => t('b);
/* Map and only keep non-[None] elements
   Formerly [fmap]
   @since 0.5 */

let filter_mapi: ((int, 'a) => option('b), t('a)) => t('b);
/* Map with indices, and only keep non-[None] elements
   @since 0.11 */

let filter_count: ('a => bool, t('a)) => int;
/* Count how many elements satisfy the given predicate
   @since 1.0 */

let intersperse: ('a, t('a)) => t('a);
/* Insert the single element between every element of the iterator */

let keep_some: t(option('a)) => t('a);
/* [filter_some l] retains only elements of the form [Some x].
   Same as [filter_map (fun x->x)]
   @since 1.0 */

let keep_ok: t(Result.result('a, _)) => t('a);
/* [keep_ok l] retains only elements of the form [Ok x].
   @since 1.0 */

let keep_error: t(Result.result(_, 'e)) => t('e);
/* [keep_error l] retains only elements of the form [Error x].
   @since 1.0 */

/* {1 Caching} */

let persistent: t('a) => t('a);
/* Iterate on the iterator, storing elements in an efficient internal structure..
   The resulting iterator can be iterated on as many times as needed.
   {b Note}: calling persistent on an already persistent iterator
   will still make a new copy of the iterator! */

let persistent_lazy: t('a) => t('a);
/* Lazy version of {!persistent}. When calling [persistent_lazy s],
   a new iterator [s'] is immediately returned (without actually consuming
   [s]) in constant time; the first time [s'] is iterated on,
   it also consumes [s] and caches its content into a inner data
   structure that will back [s'] for future iterations.

   {b warning}: on the first traversal of [s'], if the traversal
   is interrupted prematurely ({!take}, etc.) then [s'] will not be
   memorized, and the next call to [s'] will traverse [s] again. */

/* {1 Misc} */

let sort: (~cmp: ('a, 'a) => int=?, t('a)) => t('a);
/* Sort the iterator. Eager, O(n) ram and O(n ln(n)) time.
   It iterates on elements of the argument iterator immediately,
   before it sorts them. */

let sort_uniq: (~cmp: ('a, 'a) => int=?, t('a)) => t('a);
/* Sort the iterator and remove duplicates. Eager, same as [sort] */

let sorted: (~cmp: ('a, 'a) => int=?, t('a)) => bool;
/* Checks whether the iterator is sorted. Eager, same as {!sort}.
   @since 0.9 */

let group_succ_by: (~eq: ('a, 'a) => bool=?, t('a)) => t(list('a));
/* Group equal consecutive elements. Linear time.
   Formerly synonym to [group].
   {b note}: Order of items in each list is unspecified.
   @since 0.6 */

let group_by:
  (~hash: 'a => int=?, ~eq: ('a, 'a) => bool=?, t('a)) => t(list('a));
/* Group equal elements, disregarding their order of appearance.
   precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
   {b note}: Order of items in each list is unspecified.
   @since 0.6 */

let count:
  (~hash: 'a => int=?, ~eq: ('a, 'a) => bool=?, t('a)) => t(('a, int));
/* Map each distinct element to its number of occurrences in the whole seq.
   Similar to [group_by seq |> map (fun l->List.hd l, List.length l)]
   precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
   @since 0.10 */

let uniq: (~eq: ('a, 'a) => bool=?, t('a)) => t('a);
/* Remove consecutive duplicate elements. Basically this is
   like [fun seq -> map List.hd (group seq)]. */

let product: (t('a), t('b)) => t(('a, 'b));
/* Cartesian product of iterators. When calling [product a b],
   the caller {b MUST} ensure that [b] can be traversed as many times
   as required (several times), possibly by calling {!persistent} on it
   beforehand. */

let diagonal_l: list('a) => t(('a, 'a));
/* All pairs of distinct positions of the list. [diagonal l] will
   return the iterator of all [List.nth i l, List.nth j l] if [i < j].
   @since 0.9 */

let diagonal: t('a) => t(('a, 'a));
/* All pairs of distinct positions of the iterator.
   Iterates only once on the iterator, which must be finite.
   @since 0.9 */

let join: (~join_row: ('a, 'b) => option('c), t('a), t('b)) => t('c);
/* [join ~join_row a b] combines every element of [a] with every
   element of [b] using [join_row]. If [join_row] returns None, then
   the two elements do not combine. Assume that [b] allows for multiple
   iterations. */

let join_by:
  (
    ~eq: equal('key)=?,
    ~hash: hash('key)=?,
    'a => 'key,
    'b => 'key,
    ~merge: ('key, 'a, 'b) => option('c),
    t('a),
    t('b)
  ) =>
  t('c);
/* [join key1 key2 ~merge] is a binary operation
   that takes two iterators [a] and [b], projects their
   elements resp. with [key1] and [key2], and combine
   values [(x,y)] from [(a,b)] with the same [key]
   using [merge]. If [merge] returns [None], the combination
   of values is discarded.
   precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
   @since 0.10 */

let join_all_by:
  (
    ~eq: equal('key)=?,
    ~hash: hash('key)=?,
    'a => 'key,
    'b => 'key,
    ~merge: ('key, list('a), list('b)) => option('c),
    t('a),
    t('b)
  ) =>
  t('c);
/* [join_all_by key1 key2 ~merge] is a binary operation
   that takes two iterators [a] and [b], projects their
   elements resp. with [key1] and [key2], and, for each key [k]
   occurring in at least one of them:
   - compute the list [l1] of elements of [a] that map to [k]
   - compute the list [l2] of elements of [b] that map to [k]
   - call [merge k l1 l2]. If [merge] returns [None], the combination
     of values is discarded, otherwise it returns [Some c]
     and [c] is inserted in the result.
   @since 0.10 */

let group_join_by:
  (~eq: equal('a)=?, ~hash: hash('a)=?, 'b => 'a, t('a), t('b)) =>
  t(('a, list('b)));
/* [group_join_by key2] associates to every element [x] of
   the first iterator, all the elements [y] of the second
   iterator such that [eq x (key y)]. Elements of the first
   iterators without corresponding values in the second one
   are mapped to [[]]
   precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
   @since 0.10 */

/* {2 Set-like} */

let inter: (~eq: equal('a)=?, ~hash: hash('a)=?, t('a), t('a)) => t('a);
/* Intersection of two collections. Each element will occur at most once
   in the result. Eager.
   precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
   @since 0.10 */

/*$=
    [2;4;5;6] (inter (1--6) (cons 2 (4--10)) |> sort |> to_list)
    [] (inter (0--5) (6--10) |> to_list)
  */

let union: (~eq: equal('a)=?, ~hash: hash('a)=?, t('a), t('a)) => t('a);
/* Union of two collections. Each element will occur at most once
   in the result. Eager.
   precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
   @since 0.10 */

/*$=
    [2;4;5;6] (union (4--6) (cons 2 (4--5)) |> sort |> to_list)
  */

let diff: (~eq: equal('a)=?, ~hash: hash('a)=?, t('a), t('a)) => t('a);
/* Set difference. Eager.
   @since 0.10 */

/*$=
    [1;2;8;9;10] (diff (1--10) (3--7) |> to_list)
  */

let subset: (~eq: equal('a)=?, ~hash: hash('a)=?, t('a), t('a)) => bool;
/* [subset a b] returns [true] if all elements of [a] belong to [b]. Eager.
   precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
   @since 0.10 */

/*$T
    subset (2 -- 4) (1 -- 4)
    not (subset (1 -- 4) (2 -- 10))
  */

/* {2 Arithmetic} */

let max: (~lt: ('a, 'a) => bool=?, t('a)) => option('a);
/* Max element of the iterator, using the given comparison function.
   @return None if the iterator is empty, Some [m] where [m] is the maximal
   element otherwise */

let max_exn: (~lt: ('a, 'a) => bool=?, t('a)) => 'a;
/* Unsafe version of {!max}
   @raise Not_found if the iterator is empty
   @since 0.10 */

let min: (~lt: ('a, 'a) => bool=?, t('a)) => option('a);
/* Min element of the iterator, using the given comparison function.
   see {!max} for more details. */

let min_exn: (~lt: ('a, 'a) => bool=?, t('a)) => 'a;
/* Unsafe version of {!min}
   @raise Not_found if the iterator is empty
   @since 0.10 */

let sum: t(int) => int;
/* Sum of elements
   @since 0.11 */

let sumf: t(float) => float;
/* Sum of elements, using Kahan summation
   @since 0.11 */

/* {2 List-like} */

let head: t('a) => option('a);
/* First element, if any, otherwise [None]
   @since 0.5.1 */

let head_exn: t('a) => 'a;
/* First element, if any, fails
   @raise Invalid_argument if the iterator is empty
   @since 0.5.1 */

let take: (int, t('a)) => t('a);
/* Take at most [n] elements from the iterator. Works on infinite
   iterators. */

let take_while: ('a => bool, t('a)) => t('a);
/* Take elements while they satisfy the predicate, then stops iterating.
   Will work on an infinite iterator [s] if the predicate is false for at
   least one element of [s]. */

let fold_while: (('a, 'b) => ('a, [ | `Stop | `Continue]), 'a, t('b)) => 'a;
/* Folds over elements of the iterator, stopping early if the accumulator
   returns [('a, `Stop)]
   @since 0.5.5 */

let drop: (int, t('a)) => t('a);
/* Drop the [n] first elements of the iterator. Lazy. */

let drop_while: ('a => bool, t('a)) => t('a);
/* Predicate version of {!drop} */

let rev: t('a) => t('a);
/* Reverse the iterator. O(n) memory and time, needs the
   iterator to be finite. The result is persistent and does
   not depend on the input being repeatable. */

let zip_i: t('a) => t((int, 'a));
/* Zip elements of the iterator with their index in the iterator.
   @since 1.0 Changed type to just give an iterator of pairs */

/* {2 Pair iterators} */

let fold2: (('c, 'a, 'b) => 'c, 'c, t(('a, 'b))) => 'c;

let iter2: (('a, 'b) => unit, t(('a, 'b))) => unit;

let map2: (('a, 'b) => 'c, t(('a, 'b))) => t('c);

let map2_2: (('a, 'b) => 'c, ('a, 'b) => 'd, t(('a, 'b))) => t(('c, 'd));
/* [map2_2 f g seq2] maps each [x, y] of seq2 into [f x y, g x y] */

/* {1 Data structures converters} */

let to_list: t('a) => list('a);
/* Convert the iterator into a list. Preserves order of elements.
   This function is tail-recursive, but consumes 2*n memory.
   If order doesn't matter to you, consider {!to_rev_list}. */

let to_rev_list: t('a) => list('a);
/* Get the list of the reversed iterator (more efficient than {!to_list}) */

let of_list: list('a) => t('a);

let on_list: (t('a) => t('b), list('a)) => list('b);
/* [on_list f l] is equivalent to [to_list @@ f @@ of_list l].
       @since 0.5.2
   */

let pair_with_idx: t('a) => t((int, 'a));
/* Similar to {!zip_i} but returns a normal iterator of tuples
   @since 0.11 */

let to_opt: t('a) => option('a);
/* Alias to {!head}
   @since 0.5.1 */

let to_array: t('a) => array('a);
/* Convert to an array. Currently not very efficient because
   an intermediate list is used. */

let of_array: array('a) => t('a);

let of_array_i: array('a) => t((int, 'a));
/* Elements of the array, with their index */

let array_slice: (array('a), int, int) => t('a);
/* [array_slice a i j] Iterator of elements whose indexes range
   from [i] to [j] */

let of_opt: option('a) => t('a);
/* Iterate on 0 or 1 values.
   @since 0.5.1 */

let of_stream: Stream.t('a) => t('a);
/* Iterator of elements of a stream (usable only once) */

let to_stream: t('a) => Stream.t('a);
/* Convert to a stream. linear in memory and time (a copy is made in memory) */

let to_stack: (Stack.t('a), t('a)) => unit;
/* Push elements of the iterator on the stack */

let of_stack: Stack.t('a) => t('a);
/* Iterator of elements of the stack (same order as [Stack.iter]) */

let to_queue: (Queue.t('a), t('a)) => unit;
/* Push elements of the iterator into the queue */

let of_queue: Queue.t('a) => t('a);
/* Iterator of elements contained in the queue, FIFO order */

let hashtbl_add: (Hashtbl.t('a, 'b), t(('a, 'b))) => unit;
/* Add elements of the iterator to the hashtable, with
   Hashtbl.add */

let hashtbl_replace: (Hashtbl.t('a, 'b), t(('a, 'b))) => unit;
/* Add elements of the iterator to the hashtable, with
   Hashtbl.replace (erases conflicting bindings) */

let to_hashtbl: t(('a, 'b)) => Hashtbl.t('a, 'b);
/* Build a hashtable from an iterator of key/value pairs */

let of_hashtbl: Hashtbl.t('a, 'b) => t(('a, 'b));
/* Iterator of key/value pairs from the hashtable */

let hashtbl_keys: Hashtbl.t('a, 'b) => t('a);
let hashtbl_values: Hashtbl.t('a, 'b) => t('b);

let of_str: string => t(char);
let to_str: t(char) => string;

let concat_str: t(string) => string;
/* Concatenate strings together, eagerly.
   Also see {!intersperse} to add a separator.
   @since 0.5 */

exception OneShotSequence;
/* Raised when the user tries to iterate several times on
   a transient iterator */

let of_in_channel: in_channel => t(char);
/* Iterates on characters of the input (can block when one
   iterates over the iterator). If you need to iterate
   several times on this iterator, use {!persistent}.
   @raise OneShotIterator when used more than once. */

let to_buffer: (t(char), Buffer.t) => unit;
/* Copy content of the iterator into the buffer */

let int_range: (~start: int, ~stop: int) => t(int);
/* Iterator on integers in [start...stop] by steps 1. Also see
   {!(--)} for an infix version. */

let int_range_dec: (~start: int, ~stop: int) => t(int);
/* Iterator on decreasing integers in [stop...start] by steps -1.
   See {!(--^)} for an infix version */

let int_range_by: (~step: int, int, int) => t(int);
/* [int_range_by ~step i j] is the range starting at [i], including [j],
   where the difference between successive elements is [step].
   use a negative [step] for a decreasing iterator.
   @raise Invalid_argument if [step=0] */

let bools: t(bool);
/* Iterates on [true] and [false]
   @since 0.7 */

let of_set: ((module Set.S with type elt = 'a and type t = 'b), 'b) => t('a);
/* Convert the given set to an iterator. The set module must be provided. */

let to_set: ((module Set.S with type elt = 'a and type t = 'b), t('a)) => 'b;
/* Convert the iterator to a set, given the proper set module */

type gen('a) = unit => option('a);
type klist('a) = unit => [ | `Nil | `Cons('a, klist('a))];

let of_gen: gen('a) => t('a);
/* Traverse eagerly the generator and build an iterator from it */

let to_gen: t('a) => gen('a);
/* Make the iterator persistent (O(n)) and then iterate on it. Eager. */

let of_klist: klist('a) => t('a);
/* Iterate on the lazy list */

let to_klist: t('a) => klist('a);
/* Make the iterator persistent and then iterate on it. Eager. */

/* {2 Sets} */

module Set: {
  module type S = {
    include Set.S;
    let of_iter: iter(elt) => t;
    let to_iter: t => iter(elt);
    let to_list: t => list(elt);
    let of_list: list(elt) => t;

    let of_seq: iter(elt) => t;
    /* @deprecated use {!of_iter} instead */

    let to_seq: t => iter(elt);
    /* @deprecated use {!to_iter} instead */
  };

  /* Create an enriched Set module from the given one */
  module Adapt: (X: Set.S) => S with type elt = X.elt and type t = X.t;

  /* Functor to build an extended Set module from an ordered type */
  module Make: (X: Set.OrderedType) => S with type elt = X.t;
};

/* {2 Maps} */

module Map: {
  module type S = {
    include Map.S;
    let to_iter: t('a) => iter((key, 'a));
    let of_iter: iter((key, 'a)) => t('a);
    let keys: t('a) => iter(key);
    let values: t('a) => iter('a);
    let to_list: t('a) => list((key, 'a));
    let of_list: list((key, 'a)) => t('a);

    let to_seq: t('a) => iter((key, 'a));
    /* @deprecated use {!to_iter} instead */

    let of_seq: iter((key, 'a)) => t('a);
    /* @deprecated use {!of_iter} instead */
  };

  /* Adapt a pre-existing Map module to make it iterator-aware */
  module Adapt:
    (M: Map.S) => S with type key = M.key and type t('a) = M.t('a);

  /* Create an enriched Map module, with iterator-aware functions */
  module Make: (V: Map.OrderedType) => S with type key = V.t;
};

/* {1 Random iterators} */

let random_int: int => t(int);
/* Infinite iterator of random integers between 0 and
   the given higher bound (see Random.int) */

let random_bool: t(bool);
/* Infinite iterator of random bool values */

let random_float: float => t(float);

let random_array: array('a) => t('a);
/* Iterator of choices of an element in the array */

let random_list: list('a) => t('a);
/* Infinite iterator of random elements of the list. Basically the
   same as {!random_array}. */

let shuffle: t('a) => t('a);
/* [shuffle seq] returns a perfect shuffle of [seq].
   Uses O(length seq) memory and time. Eager.
   @since 0.7 */

let shuffle_buffer: (int, t('a)) => t('a);
/* [shuffle_buffer n seq] returns an iterator of element of [seq] in random
   order. The shuffling is *not* uniform. Uses O(n) memory.

   The first [n] elements of the iterator are consumed immediately. The
   rest is consumed lazily.
   @since 0.7 */

/* {2 Sampling} */

let sample: (int, t('a)) => array('a);
/* [sample n seq] returns k samples of [seq], with uniform probability.
   It will consume the iterator and use O(n) memory.

   It returns an array of size [min (length seq) n].
   @since 0.7 */

/* {1 Infix functions} */

module Infix: {
  let (--): (int, int) => t(int);
  /* [a -- b] is the range of integers from [a] to [b], both included,
     in increasing order. It will therefore be empty if [a > b]. */

  let (--^): (int, int) => t(int);
  /* [a --^ b] is the range of integers from [b] to [a], both included,
     in decreasing order (starts from [a]).
     It will therefore be empty if [a < b]. */

  let (>>=): (t('a), 'a => t('b)) => t('b);
  /* Monadic bind (infix version of {!flat_map}
     @since 0.5 */

  let (>|=): (t('a), 'a => 'b) => t('b);
  /* Infix version of {!map}
     @since 0.5 */

  let (<*>): (t('a => 'b), t('a)) => t('b);
  /* Applicative operator (product+application)
     @since 0.5 */

  let (<+>): (t('a), t('a)) => t('a);
  /* Concatenation of iterators
     @since 0.5 */
};

include (module type of Infix);

/* {1 Pretty printing} */

let pp_seq:
  (~sep: string=?, (Format.formatter, 'a) => unit, Format.formatter, t('a)) =>
  unit;
/* Pretty print an iterator of ['a], using the given pretty printer
   to print each elements. An optional separator string can be provided. */

let pp_buf: (~sep: string=?, (Buffer.t, 'a) => unit, Buffer.t, t('a)) => unit;
/* Print into a buffer */

let to_string: (~sep: string=?, 'a => string, t('a)) => string;
/* Print into a string */

/* {1 Basic IO}

   Very basic interface to manipulate files as iterator of chunks/lines. The
   iterators take care of opening and closing files properly; every time
   one iterates over an iterator, the file is opened/closed again.

   Example: copy a file ["a"] into file ["b"], removing blank lines:

   {[
     Iterator.(IO.lines_of "a" |> filter (fun l-> l<> "") |> IO.write_lines "b");;
   ]}

   By chunks of [4096] bytes:

   {[
     Iterator.IO.(chunks_of ~size:4096 "a" |> write_to "b");;
   ]}

   Read the lines of a file into a list:

   {[
     Iterator.IO.lines "a" |> Iterator.to_list
   ]}

   @since 0.5.1 */

module IO: {
  let lines_of:
    (~mode: int=?, ~flags: list(open_flag)=?, string) => t(string);
  /* [lines_of filename] reads all lines of the given file. It raises the
     same exception as would opening the file and read from it, except
     from [End_of_file] (which is caught). The file is {b always} properly
     closed.
     Every time the iterator is iterated on, the file is opened again, so
     different iterations might return different results
     @param mode default [0o644]
     @param flags default: [[Open_rdonly]] */

  let chunks_of:
    (~mode: int=?, ~flags: list(open_flag)=?, ~size: int=?, string) =>
    t(string);
  /* Read chunks of the given [size] from the file. The last chunk might be
     smaller. Behaves like {!lines_of} regarding errors and options.
     Every time the iterator is iterated on, the file is opened again, so
     different iterations might return different results */

  let write_to:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(string)) => unit;
  /* [write_to filename seq] writes all strings from [seq] into the given
     file. It takes care of opening and closing the file.
     @param mode default [0o644]
     @param flags used by [open_out_gen]. Default: [[Open_creat;Open_wronly]]. */

  let write_bytes_to:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(Bytes.t)) => unit;
  /* @since 0.5.4 */

  let write_lines:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(string)) => unit;
  /* Same as {!write_to}, but intercales ['\n'] between each string */

  let write_bytes_lines:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(Bytes.t)) => unit;
  /* @since 0.5.4 */
};
