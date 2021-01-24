/* This file is free software, part of sequence. See file "license" for more details. */

/** {1 Simple and Efficient Iterators} */;

/** The iterators are designed to allow easy transfer (mappings) between data
    structures, without defining [n^2] conversions between the [n] types. The
    implementation relies on the assumption that a sequence can be iterated
    on as many times as needed; this choice allows for high performance
    of many combinators. However, for transient iterators, the {!persistent}
    function is provided, storing elements of a transient iterator
    in memory; the iterator can then be used several times (See further).

    Note that some combinators also return sequences (e.g. {!group}). The
    transformation is computed on the fly every time one iterates over
    the resulting sequence. If a transformation performs heavy computation,
    {!persistent} can also be used as intermediate storage.

    Most functions are {b lazy}, i.e. they do not actually use their arguments
    until their result is iterated on. For instance, if one calls {!map}
    on a sequence, one gets a new sequence, but nothing else happens until
    this new sequence is used (by folding or iterating on it).

    If a sequence is built from an iteration function that is {b repeatable}
    (i.e. calling it several times always iterates on the same set of
    elements, for instance List.iter or Map.iter), then
    the resulting {!t} object is also repeatable. For {b one-time iter functions}
    such as iteration on a file descriptor or a {!Stream},
    the {!persistent} function can be used to iterate and store elements in
    a memory structure; the result is a sequence that iterates on the elements
    of this memory structure, cheaply and repeatably. */;

/** A sequence of values of type ['a]. If you give it a function ['a -> unit]
    it will be applied to every element of the sequence successively. */

type t(+'a) = ('a => unit) => unit;

type sequence(+'a) = t('a);

/** {b NOTE} Type [('a, 'b) t2 = ('a -> 'b -> unit) -> unit]
    has been removed and subsumed by [('a * 'b) t] @since 1.0 */;

type equal('a) = ('a, 'a) => bool;
type hash('a) = 'a => int;

/** {2 Build a sequence} */;

/** Build a sequence from a iter function */

let from_iter: (('a => unit) => unit) => t('a);

/** Call the function repeatedly until it returns None. This
    sequence is transient, use {!persistent} if needed! */

let from_fun: (unit => option('a)) => t('a);

/** Empty sequence. It contains no element. */

let empty: t('a);

/** Singleton sequence, with exactly one element. */

let singleton: 'a => t('a);

/** Sequence with exactly two elements */

let doubleton: ('a, 'a) => t('a);

/** [init f] is the infinite sequence [f 0; f 1; f 2; …].
    @since 0.9 */

let init: (int => 'a) => t('a);

/** [cons x l] yields [x], then yields from [l].
    Same as [append (singleton x) l] */

let cons: ('a, t('a)) => t('a);

/** Same as {!cons} but yields the element after iterating on [l] */

let snoc: (t('a), 'a) => t('a);

/** Synonym to {!singleton} */

let return: 'a => t('a);

/** Synonym to {!singleton} */

let pure: 'a => t('a);

/** Infinite sequence of the same element. You may want to look
    at {!take} and the likes if you iterate on it. */

let repeat: 'a => t('a);

/** [iterate f x] is the infinite sequence [x, f(x), f(f(x)), ...] */

let iterate: ('a => 'a, 'a) => t('a);

/** Sequence that calls the given function to produce elements.
    The sequence may be transient (depending on the function), and definitely
    is infinite. You may want to use {!take} and {!persistent}. */

let forever: (unit => 'b) => t('b);

/** Cycle forever through the given sequence. Assume the given sequence can
    be traversed any amount of times (not transient).  This yields an
    infinite sequence, you should use something like {!take} not to loop
    forever. */

let cycle: t('a) => t('a);

/** {2 Consume a sequence} */;

/** Consume the sequence, passing all its arguments to the function.
    Basically [iter f seq] is just [seq f]. */

let iter: ('a => unit, t('a)) => unit;

/** Iterate on elements and their index in the sequence */

let iteri: ((int, 'a) => unit, t('a)) => unit;

/** Fold over elements of the sequence, consuming it */

let fold: (('a, 'b) => 'a, 'a, t('b)) => 'a;

/** Fold over elements of the sequence and their index, consuming it */

let foldi: (('a, int, 'b) => 'a, 'a, t('b)) => 'a;

/** [fold_map f acc l] is like {!map}, but it carries some state as in
    {!fold}. The state is not returned, it is just used to thread some
    information to the map function.
    @since 0.9 */

let fold_map: (('acc, 'a) => ('acc, 'b), 'acc, t('a)) => t('b);

/** [fold_filter_map f acc l] is a {!fold_map}-like function, but the
    function can choose to skip an element by retuning [None].
    @since 0.9 */

let fold_filter_map:
  (('acc, 'a) => ('acc, option('b)), 'acc, t('a)) => t('b);

/** Map objects of the sequence into other elements, lazily */

let map: ('a => 'b, t('a)) => t('b);

/** Map objects, along with their index in the sequence */

let mapi: ((int, 'a) => 'b, t('a)) => t('b);

/** Map objects two by two. lazily.
      The last element is kept in the sequence if the count is odd.
      @since 0.7 */

let map_by_2: (('a, 'a) => 'a, t('a)) => t('a);

/** Do all elements satisfy the predicate? */

let for_all: ('a => bool, t('a)) => bool;

/** Exists there some element satisfying the predicate? */

let exists: ('a => bool, t('a)) => bool;

/** Is the value a member of the sequence?
    @param eq the equality predicate to use (default [(=)])
    @since 0.5 */

let mem: (~eq: ('a, 'a) => bool=?, 'a, t('a)) => bool;

/** Find the first element on which the function doesn't return [None]
    @since 0.5 */

let find: ('a => option('b), t('a)) => option('b);

/** Alias to {!find}
    @since 0.10 */

let find_map: ('a => option('b), t('a)) => option('b);

/** Indexed version of {!find}
    @since 0.9 */

let findi: ((int, 'a) => option('b), t('a)) => option('b);

/** Alias to {!findi}
    @since 0.10 */

let find_mapi: ((int, 'a) => option('b), t('a)) => option('b);

/** [find_pred p l] finds the first element of [l] that satisfies [p],
    or returns [None] if no element satisfies [p]
    @since 0.9 */

let find_pred: ('a => bool, t('a)) => option('a);

/** Unsafe version of {!find_pred}
    @raise Not_found if no such element is found
    @since 0.9 */

let find_pred_exn: ('a => bool, t('a)) => 'a;

/** How long is the sequence? Forces the sequence. */

let length: t('a) => int;

/** Is the sequence empty? Forces the sequence. */

let is_empty: t('a) => bool;

/** {2 Transform a sequence} */;

/** Filter on elements of the sequence */

let filter: ('a => bool, t('a)) => t('a);

/** Append two sequences. Iterating on the result is like iterating
    on the first, then on the second. */

let append: (t('a), t('a)) => t('a);

/** Append sequences. Iterating on the result is like iterating
    on the each sequence of the list in order.
    @since 0.11 */

let append_l: list(t('a)) => t('a);

/** Concatenate a sequence of sequences into one sequence. */

let concat: t(t('a)) => t('a);

/** Alias for {!concat} */

let flatten: t(t('a)) => t('a);

/** Monadic bind. Intuitively, it applies the function to every
    element of the initial sequence, and calls {!concat}.
    Formerly [flatMap]
    @since 0.5 */

let flat_map: ('a => t('b), t('a)) => t('b);

/** Convenience function combining {!flat_map} and {!of_list}
    @since 0.9 */

let flat_map_l: ('a => list('b), t('a)) => t('b);

/** [seq_list l] returns all the ways to pick one element in each sub-sequence
    in [l]. Assumes the sub-sequences can be iterated on several times.
    @since 0.11 */

let seq_list: list(t('a)) => t(list('a));

/** [seq_list_map f l] maps [f] over every element of [l],
    then calls {!seq_list}
    @since 0.11 */

let seq_list_map: ('a => t('b), list('a)) => t(list('b));

/** Map and only keep non-[None] elements
    Formerly [fmap]
    @since 0.5 */

let filter_map: ('a => option('b), t('a)) => t('b);

/** Map with indices, and only keep non-[None] elements
    @since 0.11 */

let filter_mapi: ((int, 'a) => option('b), t('a)) => t('b);

/** Count how many elements satisfy the given predicate
    @since 1.0 */

let filter_count: ('a => bool, t('a)) => int;

/** Insert the single element between every element of the sequence */

let intersperse: ('a, t('a)) => t('a);

/** [filter_some l] retains only elements of the form [Some x].
    Same as [filter_map (fun x->x)]
    @since 1.0 */

let keep_some: t(option('a)) => t('a);

/** [keep_ok l] retains only elements of the form [Ok x].
    @since 1.0 */

let keep_ok: t(Result.result('a, _)) => t('a);

/** [keep_error l] retains only elements of the form [Error x].
    @since 1.0 */

let keep_error: t(Result.result(_, 'e)) => t('e);

/** {2 Caching} */;

/** Iterate on the sequence, storing elements in an efficient internal structure..
    The resulting sequence can be iterated on as many times as needed.
    {b Note}: calling persistent on an already persistent sequence
    will still make a new copy of the sequence! */

let persistent: t('a) => t('a);

/** Lazy version of {!persistent}. When calling [persistent_lazy s],
    a new sequence [s'] is immediately returned (without actually consuming
    [s]) in constant time; the first time [s'] is iterated on,
    it also consumes [s] and caches its content into a inner data
    structure that will back [s'] for future iterations.

    {b warning}: on the first traversal of [s'], if the traversal
    is interrupted prematurely ({!take}, etc.) then [s'] will not be
    memorized, and the next call to [s'] will traverse [s] again. */

let persistent_lazy: t('a) => t('a);

/** {2 Misc} */;

/** Sort the sequence. Eager, O(n) ram and O(n ln(n)) time.
    It iterates on elements of the argument sequence immediately,
    before it sorts them. */

let sort: (~cmp: ('a, 'a) => int=?, t('a)) => t('a);

/** Sort the sequence and remove duplicates. Eager, same as [sort] */

let sort_uniq: (~cmp: ('a, 'a) => int=?, t('a)) => t('a);

/** Checks whether the sequence is sorted. Eager, same as {!sort}.
    @since 0.9 */

let sorted: (~cmp: ('a, 'a) => int=?, t('a)) => bool;

/** Group equal consecutive elements. Linear time.
    Formerly synonym to [group].
    @since 0.6 */

let group_succ_by: (~eq: ('a, 'a) => bool=?, t('a)) => t(list('a));

/** Group equal elements, disregarding their order of appearance.
    The result sequence is traversable as many times as required.
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
    @since 0.6 */

let group_by:
  (~hash: 'a => int=?, ~eq: ('a, 'a) => bool=?, t('a)) => t(list('a));

/** Map each distinct element to its number of occurrences in the whole seq.
    Similar to [group_by seq |> map (fun l->List.hd l, List.length l)]
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
    @since 0.10 */

let count:
  (~hash: 'a => int=?, ~eq: ('a, 'a) => bool=?, t('a)) => t(('a, int));

/** Remove consecutive duplicate elements. Basically this is
    like [fun seq -> map List.hd (group seq)]. */

let uniq: (~eq: ('a, 'a) => bool=?, t('a)) => t('a);

/** Cartesian product of the sequences. When calling [product a b],
    the caller {b MUST} ensure that [b] can be traversed as many times
    as required (several times), possibly by calling {!persistent} on it
    beforehand. */

let product: (t('a), t('b)) => t(('a, 'b));

/** All pairs of distinct positions of the list. [diagonal l] will
    return the sequence of all [List.nth i l, List.nth j l] if [i < j].
    @since 0.9 */

let diagonal_l: list('a) => t(('a, 'a));

/** All pairs of distinct positions of the sequence.
    Iterates only once on the sequence, which must be finite.
    @since 0.9 */

let diagonal: t('a) => t(('a, 'a));

/** [join ~join_row a b] combines every element of [a] with every
    element of [b] using [join_row]. If [join_row] returns None, then
    the two elements do not combine. Assume that [b] allows for multiple
    iterations. */

let join: (~join_row: ('a, 'b) => option('c), t('a), t('b)) => t('c);

/** [join key1 key2 ~merge] is a binary operation
    that takes two sequences [a] and [b], projects their
    elements resp. with [key1] and [key2], and combine
    values [(x,y)] from [(a,b)] with the same [key]
    using [merge]. If [merge] returns [None], the combination
    of values is discarded.
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
    @since 0.10 */

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

/** [join_all_by key1 key2 ~merge] is a binary operation
    that takes two sequences [a] and [b], projects their
    elements resp. with [key1] and [key2], and, for each key [k]
    occurring in at least one of them:
    - compute the list [l1] of elements of [a] that map to [k]
    - compute the list [l2] of elements of [b] that map to [k]
    - call [merge k l1 l2]. If [merge] returns [None], the combination
      of values is discarded, otherwise it returns [Some c]
      and [c] is inserted in the result.
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

/** [group_join_by key2] associates to every element [x] of
    the first sequence, all the elements [y] of the second
    sequence such that [eq x (key y)]. Elements of the first
    sequences without corresponding values in the second one
    are mapped to [[]]
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
    @since 0.10 */

let group_join_by:
  (~eq: equal('a)=?, ~hash: hash('a)=?, 'b => 'a, t('a), t('b)) =>
  t(('a, list('b)));

/** Intersection of two collections. Each element will occur at most once
    in the result. Eager.
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
    @since 0.10 */

let inter: (~eq: equal('a)=?, ~hash: hash('a)=?, t('a), t('a)) => t('a);

/*$=
    [2;4;5;6] (inter (1--6) (cons 2 (4--10)) |> sort |> to_list)
    [] (inter (0--5) (6--10) |> to_list)
  */

/** Union of two collections. Each element will occur at most once
    in the result. Eager.
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
    @since 0.10 */

let union: (~eq: equal('a)=?, ~hash: hash('a)=?, t('a), t('a)) => t('a);

/*$=
    [2;4;5;6] (union (4--6) (cons 2 (4--5)) |> sort |> to_list)
  */

/** Set difference. Eager.
    @since 0.10 */

let diff: (~eq: equal('a)=?, ~hash: hash('a)=?, t('a), t('a)) => t('a);

/*$=
    [1;2;8;9;10] (diff (1--10) (3--7) |> to_list)
  */

/** [subset a b] returns [true] if all elements of [a] belong to [b]. Eager.
    precondition: for any [x] and [y], if [eq x y] then [hash x=hash y] must hold.
    @since 0.10 */

let subset: (~eq: equal('a)=?, ~hash: hash('a)=?, t('a), t('a)) => bool;

/*$T
    subset (2 -- 4) (1 -- 4)
    not (subset (1 -- 4) (2 -- 10))
  */

/** [unfoldr f b] will apply [f] to [b]. If it
    yields [Some (x,b')] then [x] is returned
    and unfoldr recurses with [b']. */

let unfoldr: ('b => option(('a, 'b)), 'b) => t('a);

/** Sequence of intermediate results */

let scan: (('b, 'a) => 'b, 'b, t('a)) => t('b);

/** Max element of the sequence, using the given comparison function.
    @return None if the sequence is empty, Some [m] where [m] is the maximal
    element otherwise */

let max: (~lt: ('a, 'a) => bool=?, t('a)) => option('a);

/** Unsafe version of {!max}
    @raise Not_found if the sequence is empty
    @since 0.10 */

let max_exn: (~lt: ('a, 'a) => bool=?, t('a)) => 'a;

/** Min element of the sequence, using the given comparison function.
    see {!max} for more details. */

let min: (~lt: ('a, 'a) => bool=?, t('a)) => option('a);

/** Unsafe version of {!min}
    @raise Not_found if the sequence is empty
    @since 0.10 */

let min_exn: (~lt: ('a, 'a) => bool=?, t('a)) => 'a;

/** Sum of elements
    @since 0.11 */

let sum: t(int) => int;

/** Sum of elements, using Kahan summation
    @since 0.11 */

let sumf: t(float) => float;

/** First element, if any, otherwise [None]
    @since 0.5.1 */

let head: t('a) => option('a);

/** First element, if any, fails
    @raise Invalid_argument if the sequence is empty
    @since 0.5.1 */

let head_exn: t('a) => 'a;

/** Take at most [n] elements from the sequence. Works on infinite
    sequences. */

let take: (int, t('a)) => t('a);

/** Take elements while they satisfy the predicate, then stops iterating.
    Will work on an infinite sequence [s] if the predicate is false for at
    least one element of [s]. */

let take_while: ('a => bool, t('a)) => t('a);

/** Folds over elements of the sequence, stopping early if the accumulator
    returns [('a, `Stop)]
    @since 0.5.5 */

let fold_while: (('a, 'b) => ('a, [ | `Stop | `Continue]), 'a, t('b)) => 'a;

/** Drop the [n] first elements of the sequence. Lazy. */

let drop: (int, t('a)) => t('a);

/** Predicate version of {!drop} */

let drop_while: ('a => bool, t('a)) => t('a);

/** Reverse the sequence. O(n) memory and time, needs the
    sequence to be finite. The result is persistent and does
    not depend on the input being repeatable. */

let rev: t('a) => t('a);

/** Zip elements of the sequence with their index in the sequence.
    Changed type @since 1.0 to just give a sequence of pairs */

let zip_i: t('a) => t((int, 'a));

let fold2: (('c, 'a, 'b) => 'c, 'c, t(('a, 'b))) => 'c;

let iter2: (('a, 'b) => unit, t(('a, 'b))) => unit;

let map2: (('a, 'b) => 'c, t(('a, 'b))) => t('c);

/** [map2_2 f g seq2] maps each [x, y] of seq2 into [f x y, g x y] */

let map2_2: (('a, 'b) => 'c, ('a, 'b) => 'd, t(('a, 'b))) => t(('c, 'd));

/** {2 Basic data structures converters} */;

/** Convert the sequence into a list. Preserves order of elements.
    This function is tail-recursive, but consumes 2*n memory.
    If order doesn't matter to you, consider {!to_rev_list}. */

let to_list: t('a) => list('a);

/** Get the list of the reversed sequence (more efficient than {!to_list}) */

let to_rev_list: t('a) => list('a);

let of_list: list('a) => t('a);

/** [on_list f l] is equivalent to [to_list @@ f @@ of_list l].
    @since 0.5.2
*/

let on_list: (t('a) => t('b), list('a)) => list('b);

/** Similar to {!zip_i} but returns a normal sequence of tuples
    @since 0.11 */

let pair_with_idx: t('a) => t((int, 'a));

/** Alias to {!head}
    @since 0.5.1 */

let to_opt: t('a) => option('a);

/** Convert to an array. Currently not very efficient because
    an intermediate list is used. */

let to_array: t('a) => array('a);

let of_array: array('a) => t('a);

/** Elements of the array, with their index */

let of_array_i: array('a) => t((int, 'a));

/** [array_slice a i j] Sequence of elements whose indexes range
    from [i] to [j] */

let array_slice: (array('a), int, int) => t('a);

/** Iterate on 0 or 1 values.
    @since 0.5.1 */

let of_opt: option('a) => t('a);

/** Sequence of elements of a stream (usable only once) */

let of_stream: Stream.t('a) => t('a);

/** Convert to a stream. linear in memory and time (a copy is made in memory) */

let to_stream: t('a) => Stream.t('a);

/** Push elements of the sequence on the stack */

let to_stack: (Stack.t('a), t('a)) => unit;

/** Sequence of elements of the stack (same order as [Stack.iter]) */

let of_stack: Stack.t('a) => t('a);

/** Push elements of the sequence into the queue */

let to_queue: (Queue.t('a), t('a)) => unit;

/** Sequence of elements contained in the queue, FIFO order */

let of_queue: Queue.t('a) => t('a);

/** Add elements of the sequence to the hashtable, with
    Hashtbl.add */

let hashtbl_add: (Hashtbl.t('a, 'b), t(('a, 'b))) => unit;

/** Add elements of the sequence to the hashtable, with
    Hashtbl.replace (erases conflicting bindings) */

let hashtbl_replace: (Hashtbl.t('a, 'b), t(('a, 'b))) => unit;

/** Build a hashtable from a sequence of key/value pairs */

let to_hashtbl: t(('a, 'b)) => Hashtbl.t('a, 'b);

/** Sequence of key/value pairs from the hashtable */

let of_hashtbl: Hashtbl.t('a, 'b) => t(('a, 'b));

let hashtbl_keys: Hashtbl.t('a, 'b) => t('a);
let hashtbl_values: Hashtbl.t('a, 'b) => t('b);

let of_str: string => t(char);
let to_str: t(char) => string;

/** Concatenate strings together, eagerly.
    Also see {!intersperse} to add a separator.
    @since 0.5 */

let concat_str: t(string) => string;

/** Raised when the user tries to iterate several times on
    a transient iterator */

exception OneShotSequence;

/** Iterates on characters of the input (can block when one
    iterates over the sequence). If you need to iterate
    several times on this sequence, use {!persistent}.
    @raise OneShotSequence when used more than once. */

let of_in_channel: in_channel => t(char);

/** Copy content of the sequence into the buffer */

let to_buffer: (t(char), Buffer.t) => unit;

/** Iterator on integers in [start...stop] by steps 1. Also see
    {!(--)} for an infix version. */

let int_range: (~start: int, ~stop: int) => t(int);

/** Iterator on decreasing integers in [stop...start] by steps -1.
    See {!(--^)} for an infix version */

let int_range_dec: (~start: int, ~stop: int) => t(int);

/** [int_range_by ~step i j] is the range starting at [i], including [j],
    where the difference between successive elements is [step].
    use a negative [step] for a decreasing sequence.
    @raise Invalid_argument if [step=0] */

let int_range_by: (~step: int, int, int) => t(int);

/** Iterates on [true] and [false]
    @since 0.7 */

let bools: t(bool);

/** Convert the given set to a sequence. The set module must be provided. */

let of_set: ((module Set.S with type elt = 'a and type t = 'b), 'b) => t('a);

/** Convert the sequence to a set, given the proper set module */

let to_set: ((module Set.S with type elt = 'a and type t = 'b), t('a)) => 'b;

type gen('a) = unit => option('a);
type klist('a) = unit => [ | `Nil | `Cons('a, klist('a))];

/** Traverse eagerly the generator and build a sequence from it */

let of_gen: gen('a) => t('a);

/** Make the sequence persistent (O(n)) and then iterate on it. Eager. */

let to_gen: t('a) => gen('a);

/** Iterate on the lazy list */

let of_klist: klist('a) => t('a);

/** Make the sequence persistent and then iterate on it. Eager. */

let to_klist: t('a) => klist('a);

/** {2 Functorial conversions between sets and sequences} */;

module Set: {
  module type S = {
    include Set.S;
    let of_seq: sequence(elt) => t;
    let to_seq: t => sequence(elt);
    let to_list: t => list(elt);
    let of_list: list(elt) => t;
  };

  /** Create an enriched Set module from the given one */

  module Adapt: (X: Set.S) => S with type elt = X.elt and type t = X.t;

  /** Functor to build an extended Set module from an ordered type */

  module Make: (X: Set.OrderedType) => S with type elt = X.t;
};

/** {2 Conversion between maps and sequences.} */;

module Map: {
  module type S = {
    include Map.S;
    let to_seq: t('a) => sequence((key, 'a));
    let of_seq: sequence((key, 'a)) => t('a);
    let keys: t('a) => sequence(key);
    let values: t('a) => sequence('a);
    let to_list: t('a) => list((key, 'a));
    let of_list: list((key, 'a)) => t('a);
  };

  /** Adapt a pre-existing Map module to make it sequence-aware */

  module Adapt:
    (M: Map.S) => S with type key = M.key and type t('a) = M.t('a);

  /** Create an enriched Map module, with sequence-aware functions */

  module Make: (V: Map.OrderedType) => S with type key = V.t;
};

/** {2 Infinite sequences of random values} */;

/** Infinite sequence of random integers between 0 and
    the given higher bound (see Random.int) */

let random_int: int => t(int);

/** Infinite sequence of random bool values */

let random_bool: t(bool);

let random_float: float => t(float);

/** Sequence of choices of an element in the array */

let random_array: array('a) => t('a);

/** Infinite sequence of random elements of the list. Basically the
    same as {!random_array}. */

let random_list: list('a) => t('a);

/** [shuffle seq] returns a perfect shuffle of [seq].
    Uses O(length seq) memory and time. Eager.
    @since 0.7 */

let shuffle: t('a) => t('a);

/** [shuffle_buffer n seq] returns a sequence of element of [seq] in random
    order. The shuffling is *not* uniform. Uses O(n) memory.

    The first [n] elements of the sequence are consumed immediately. The
    rest is consumed lazily.
    @since 0.7 */

let shuffle_buffer: (int, t('a)) => t('a);

/** {2 Sampling} */;

/** [sample n seq] returns k samples of [seq], with uniform probability.
      It will consume the sequence and use O(n) memory.

      It returns an array of size [min (length seq) n].
      @since 0.7 */

let sample: (int, t('a)) => array('a);

/** {2 Infix functions} */;

module Infix: {
  /** [a -- b] is the range of integers from [a] to [b], both included,
      in increasing order. It will therefore be empty if [a > b]. */

  let (--): (int, int) => t(int);

  /** [a --^ b] is the range of integers from [b] to [a], both included,
      in decreasing order (starts from [a]).
      It will therefore be empty if [a < b]. */

  let (--^): (int, int) => t(int);

  /** Monadic bind (infix version of {!flat_map}
      @since 0.5 */

  let (>>=): (t('a), 'a => t('b)) => t('b);

  /** Infix version of {!map}
      @since 0.5 */

  let (>|=): (t('a), 'a => 'b) => t('b);

  /** Applicative operator (product+application)
      @since 0.5 */

  let (<*>): (t('a => 'b), t('a)) => t('b);

  /** Concatenation of sequences
      @since 0.5 */

  let (<+>): (t('a), t('a)) => t('a);
};

include (module type of Infix);

/** {2 Pretty printing of sequences} */;

/** Pretty print a sequence of ['a], using the given pretty printer
    to print each elements. An optional separator string can be provided. */

let pp_seq:
  (~sep: string=?, (Format.formatter, 'a) => unit, Format.formatter, t('a)) =>
  unit;

/** Print into a buffer */

let pp_buf: (~sep: string=?, (Buffer.t, 'a) => unit, Buffer.t, t('a)) => unit;

/** Print into a string */

let to_string: (~sep: string=?, 'a => string, t('a)) => string;

/** {2 Basic IO}

    Very basic interface to manipulate files as sequence of chunks/lines. The
    sequences take care of opening and closing files properly; every time
    one iterates over a sequence, the file is opened/closed again.

    Example: copy a file ["a"] into file ["b"], removing blank lines:

    {[
      Sequence.(IO.lines_of "a" |> filter (fun l-> l<> "") |> IO.write_lines "b");;
    ]}

    By chunks of [4096] bytes:

    {[
      Sequence.IO.(chunks_of ~size:4096 "a" |> write_to "b");;
    ]}

    Read the lines of a file into a list:

    {[
      Sequence.IO.lines "a" |> Sequence.to_list
    ]}

    @since 0.5.1 */;

module IO: {
  /** [lines_of filename] reads all lines of the given file. It raises the
      same exception as would opening the file and read from it, except
      from [End_of_file] (which is caught). The file is {b always} properly
      closed.
      Every time the sequence is iterated on, the file is opened again, so
      different iterations might return different results
      @param mode default [0o644]
      @param flags default: [[Open_rdonly]] */

  let lines_of:
    (~mode: int=?, ~flags: list(open_flag)=?, string) => t(string);

  /** Read chunks of the given [size] from the file. The last chunk might be
      smaller. Behaves like {!lines_of} regarding errors and options.
      Every time the sequence is iterated on, the file is opened again, so
      different iterations might return different results */

  let chunks_of:
    (~mode: int=?, ~flags: list(open_flag)=?, ~size: int=?, string) =>
    t(string);

  /** [write_to filename seq] writes all strings from [seq] into the given
      file. It takes care of opening and closing the file.
      @param mode default [0o644]
      @param flags used by [open_out_gen]. Default: [[Open_creat;Open_wronly]]. */

  let write_to:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(string)) => unit;

  /** @since 0.5.4 */

  let write_bytes_to:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(Bytes.t)) => unit;

  /** Same as {!write_to}, but intercales ['\n'] between each string */

  let write_lines:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(string)) => unit;

  /** @since 0.5.4 */

  let write_bytes_lines:
    (~mode: int=?, ~flags: list(open_flag)=?, string, t(Bytes.t)) => unit;
};
