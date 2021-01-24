/* This file is free software, part of gen. See file "license" for more details. */

/** {1 Common signature for transient and restartable generators}

    The signature {!S} abstracts on a type ['a t], where the [t] can be
    the type of transient or restartable generators. Some functions specify
    explicitely that they use ['a gen] (transient generators). */;

type gen('a) = unit => option('a);

module type S = {
  type t('a);

  /** Empty generator, with no elements */

  let empty: t('a);

  /** One-element generator */

  let singleton: 'a => t('a);

  /** Alias to {!singleton}
      @since 0.3 */

  let return: 'a => t('a);

  /** Repeat same element endlessly */

  let repeat: 'a => t('a);

  /** [iterate x f] is [[x; f x; f (f x); f (f (f x)); ...]] */

  let iterate: ('a, 'a => 'a) => t('a);

  /** Dual of {!fold}, with a deconstructing operation. It keeps on
      unfolding the ['b] value into a new ['b], and a ['a] which is yielded,
      until [None] is returned. */

  let unfold: ('b => option(('a, 'b)), 'b) => t('a);

  /** Calls the function, starting from 0, on increasing indices.
      If [limit] is provided and is a positive int, iteration will
      stop at the limit (excluded).
      For instance [init ~limit:4 id] will yield 0, 1, 2, and 3. */

  let init: (~limit: int=?, int => 'a) => t('a);

  /** {2 Basic combinators}

      {b Note}: those combinators, applied to generators (not restartable
      generators) {i consume} their argument. Sometimes they consume it lazily,
      sometimes eagerly, but in any case once [f gen] has been called (with [f] a
      combinator), [gen] shouldn't be used anymore. */;

  /** Check whether the gen is empty. Pops an element, if any */

  let is_empty: t(_) => bool;

  /** Fold on the generator, tail-recursively. Consumes the generator. */

  let fold: (('b, 'a) => 'b, 'b, t('a)) => 'b;

  /** Fold on non-empty sequences. Consumes the generator.
      @raise Invalid_argument on an empty gen */

  let reduce: (('a, 'a) => 'a, t('a)) => 'a;

  /** Like {!fold}, but keeping successive values of the accumulator.
      Consumes the generator. */

  let scan: (('b, 'a) => 'b, 'b, t('a)) => t('b);

  /** A mix of {!unfold} and {!scan}. The current state is combined with
      the current element to produce a new state, and an output value
      of type 'c.
      @since 0.2.2 */

  let unfold_scan: (('b, 'a) => ('b, 'c), 'b, t('a)) => t('c);

  /** Iterate on the gen, consumes it. */

  let iter: ('a => unit, t('a)) => unit;

  /** Iterate on elements with their index in the gen, from 0, consuming it. */

  let iteri: ((int, 'a) => unit, t('a)) => unit;

  /** Length of an gen (linear time), consuming it */

  let length: t(_) => int;

  /** Lazy map. No iteration is performed now, the function will be called
      when the result is traversed. */

  let map: ('a => 'b, t('a)) => t('b);

  /** Lazy map with indexing starting from 0. No iteration is performed now,
      the function will be called when the result is traversed.
      @since 0.5 */

  let mapi: ((int, 'a) => 'b, t('a)) => t('b);

  /** Lazy fold and map. No iteration is performed now, the function will be
      called when the result is traversed. The result is
      an iterator over the successive states of the fold.
      @since 0.2.4 */

  let fold_map: (('b, 'a) => 'b, 'b, t('a)) => t('b);

  /** Append the two gens; the result contains the elements of the first,
      then the elements of the second gen. */

  let append: (t('a), t('a)) => t('a);

  /** Flatten the generator of generators */

  let flatten: t(gen('a)) => t('a);

  /** Monadic bind; each element is transformed to a sub-gen
      which is then iterated on, before the next element is processed,
      and so on. */

  let flat_map: ('a => gen('b), t('a)) => t('b);

  /** Is the given element, member of the gen? */

  let mem: (~eq: ('a, 'a) => bool=?, 'a, t('a)) => bool;

  /** Take at most n elements */

  let take: (int, t('a)) => t('a);

  /** Drop n elements */

  let drop: (int, t('a)) => t('a);

  /** n-th element, or Not_found
      @raise Not_found if the generator contains less than [n] arguments */

  let nth: (int, t('a)) => 'a;

  /** [take_nth n g] returns every element of [g] whose index
      is a multiple of [n]. For instance [take_nth 2 (1--10) |> to_list]
      will return [1;3;5;7;9] */

  let take_nth: (int, t('a)) => t('a);

  /** Filter out elements that do not satisfy the predicate.  */

  let filter: ('a => bool, t('a)) => t('a);

  /** Take elements while they satisfy the predicate. The initial generator
      itself is not to be used anymore after this. */

  let take_while: ('a => bool, t('a)) => t('a);

  /** Fold elements until (['a, `Stop]) is indicated by the accumulator.
      @since 0.2.4 */

  let fold_while:
    (('a, 'b) => ('a, [ | `Stop | `Continue]), 'a, t('b)) => 'a;

  /** Drop elements while they satisfy the predicate. The initial generator
      itself should not be used anymore, only the result of [drop_while]. */

  let drop_while: ('a => bool, t('a)) => t('a);

  /** Maps some elements to 'b, drop the other ones */

  let filter_map: ('a => option('b), t('a)) => t('b);

  /** Zip elements with their index in the gen */

  let zip_index: t('a) => t((int, 'a));

  /** Unzip into two sequences, splitting each pair */

  let unzip: t(('a, 'b)) => (t('a), t('b));

  /** [partition p l] returns the elements that satisfy [p],
      and the elements that do not satisfy [p] */

  let partition: ('a => bool, t('a)) => (t('a), t('a));

  /** Is the predicate true for all elements? */

  let for_all: ('a => bool, t('a)) => bool;

  /** Is the predicate true for at least one element? */

  let exists: ('a => bool, t('a)) => bool;

  /** Minimum element, according to the given comparison function.
      @raise Invalid_argument if the generator is empty */

  let min: (~lt: ('a, 'a) => bool=?, t('a)) => 'a;

  /** Maximum element, see {!min}
      @raise Invalid_argument if the generator is empty */

  let max: (~lt: ('a, 'a) => bool=?, t('a)) => 'a;

  /** Equality of generators. */

  let eq: (~eq: ('a, 'a) => bool=?, t('a), t('a)) => bool;

  /** Lexicographic comparison of generators. If a generator is a prefix
      of the other one, it is considered smaller. */

  let lexico: (~cmp: ('a, 'a) => int=?, t('a), t('a)) => int;

  /** Synonym for {! lexico} */

  let compare: (~cmp: ('a, 'a) => int=?, t('a), t('a)) => int;

  /** [find p e] returns the first element of [e] to satisfy [p],
      or None. */

  let find: ('a => bool, t('a)) => option('a);

  /** Sum of all elements */

  let sum: t(int) => int;

  /** {2 Multiple iterators} */;

  /** Map on the two sequences. Stops once one of them is exhausted.*/

  let map2: (('a, 'b) => 'c, t('a), t('b)) => t('c);

  /** Iterate on the two sequences. Stops once one of them is exhausted.*/

  let iter2: (('a, 'b) => unit, t('a), t('b)) => unit;

  /** Fold the common prefix of the two iterators */

  let fold2: (('acc, 'a, 'b) => 'acc, 'acc, t('a), t('b)) => 'acc;

  /** Succeeds if all pairs of elements satisfy the predicate.
      Ignores elements of an iterator if the other runs dry. */

  let for_all2: (('a, 'b) => bool, t('a), t('b)) => bool;

  /** Succeeds if some pair of elements satisfy the predicate.
      Ignores elements of an iterator if the other runs dry. */

  let exists2: (('a, 'b) => bool, t('a), t('b)) => bool;

  /** Combine common part of the gens (stops when one is exhausted) */

  let zip_with: (('a, 'b) => 'c, t('a), t('b)) => t('c);

  /** Zip together the common part of the gens */

  let zip: (t('a), t('b)) => t(('a, 'b));

  /** {2 Complex combinators} */;

  /** Pick elements fairly in each sub-generator. The merge of gens
      [e1, e2, ... ] picks elements in [e1], [e2],
      in [e3], [e1], [e2] .... Once a generator is empty, it is skipped;
      when they are all empty, and none remains in the input,
      their merge is also empty.
      For instance, [merge [1;3;5] [2;4;6]] will be, in disorder, [1;2;3;4;5;6]. */

  let merge: t(gen('a)) => t('a);

  /** Intersection of two sorted sequences. Only elements that occur in both
      inputs appear in the output */

  let intersection: (~cmp: ('a, 'a) => int=?, t('a), t('a)) => t('a);

  /** Merge two sorted sequences into a sorted sequence */

  let sorted_merge: (~cmp: ('a, 'a) => int=?, t('a), t('a)) => t('a);

  /** Sorted merge of multiple sorted sequences */

  let sorted_merge_n: (~cmp: ('a, 'a) => int=?, list(t('a))) => t('a);

  /** Duplicate the gen into [n] generators (default 2). The generators
      share the same underlying instance of the gen, so the optimal case is
      when they are consumed evenly */

  let tee: (~n: int=?, t('a)) => list(gen('a));

  /** Split the gen into [n] generators in a fair way. Elements with
      [index = k mod n] with go to the k-th gen. [n] default value
      is 2. */

  let round_robin: (~n: int=?, t('a)) => list(gen('a));

  /** [interleave a b] yields an element of [a], then an element of [b],
      and so on. When a generator is exhausted, this behaves like the
      other generator. */

  let interleave: (t('a), t('a)) => t('a);

  /** Put the separator element between all elements of the given gen */

  let intersperse: ('a, t('a)) => t('a);

  /** Cartesian product, in no predictable order. Works even if some of the
      arguments are infinite. */

  let product: (t('a), t('b)) => t(('a, 'b));

  /** Group equal consecutive elements together. */

  let group: (~eq: ('a, 'a) => bool=?, t('a)) => t(list('a));

  /** Remove consecutive duplicate elements. Basically this is
      like [fun e -> map List.hd (group e)]. */

  let uniq: (~eq: ('a, 'a) => bool=?, t('a)) => t('a);

  /** Sort according to the given comparison function. The gen must be finite. */

  let sort: (~cmp: ('a, 'a) => int=?, t('a)) => t('a);

  /** Sort and remove duplicates. The gen must be finite. */

  let sort_uniq: (~cmp: ('a, 'a) => int=?, t('a)) => t('a);

  /** [chunks n e] returns a generator of arrays of length [n], composed
      of successive elements of [e]. The last array may be smaller
      than [n] */

  let chunks: (int, t('a)) => t(array('a));

  /** Permutations of the gen.
      @since 0.2.2 */

  let permutations: t('a) => t(list('a));

  /** Permutations of the gen, using Heap's algorithm.
      @since 0.2.3 */

  let permutations_heap: t('a) => t(array('a));

  /** Combinations of given length. The ordering of the elements within
      each combination is unspecified.
      Example (ignoring ordering):
        [combinations 2 (1--3) |> to_list = [[1;2]; [1;3]; [2;3]]]
      @since 0.2.2 */

  let combinations: (int, t('a)) => t(list('a));

  /** All subsets of the gen (in no particular order). The ordering of
      the elements within each subset is unspecified.
      @since 0.2.2 */

  let power_set: t('a) => t(list('a));

  /** {2 Basic conversion functions} */;

  /** Enumerate elements of the list */

  let of_list: list('a) => t('a);

  /** non tail-call trasnformation to list, in the same order */

  let to_list: t('a) => list('a);

  /** Tail call conversion to list, in reverse order (more efficient) */

  let to_rev_list: t('a) => list('a);

  /** Convert the gen to an array (not very efficient) */

  let to_array: t('a) => array('a);

  /** Iterate on (a slice of) the given array */

  let of_array: (~start: int=?, ~len: int=?, array('a)) => t('a);

  /** Iterate on bytes of the string */

  let of_string: (~start: int=?, ~len: int=?, string) => t(char);

  /** Convert into a string */

  let to_string: t(char) => string;

  /** Consumes the iterator and writes to the buffer */

  let to_buffer: (Buffer.t, t(char)) => unit;

  /** Random ints in the given range. */

  let rand_int: int => t(int);

  /** [int_range ~step a b] generates integers between [a] and [b], included,
      with steps of length [step] (1 if omitted). [a] is assumed to be smaller
      than [b], otherwise the result will be empty.
      @raise Invalid_argument if [step=0]
      @param step step between two numbers; must not be zero,
        but it can be negative for decreasing ranges (@since 0.5). */

  let int_range: (~step: int=?, int, int) => t(int);

  /** Group together chars belonging to the same line
      @since 0.3 */

  let lines: t(char) => t(string);

  /** Explode lines into their chars, adding a ['\n'] after each one
      @since 0.3 */

  let unlines: t(string) => t(char);

  module Infix: {
    /** Synonym for {! int_range ~by:1} */

    let (--): (int, int) => t(int);

    /** Monadic bind operator */

    let (>>=): (t('a), 'a => gen('b)) => t('b);

    /** Infix map operator
        @since 0.2.3 */

    let (>>|): (t('a), 'a => 'b) => t('b);

    /** Infix map operator
        @since 0.2.3 */

    let (>|=): (t('a), 'a => 'b) => t('b);
  };

  /** Synonym for {! int_range ~by:1} */

  let (--): (int, int) => t(int);

  /** Monadic bind operator */

  let (>>=): (t('a), 'a => gen('b)) => t('b);

  /** Infix map operator
      @since 0.2.3 */

  let (>>|): (t('a), 'a => 'b) => t('b);

  /** Infix map operator
      @since 0.2.3 */

  let (>|=): (t('a), 'a => 'b) => t('b);

  /** Pretty print the content of the generator on a formatter. */

  let pp:
    (
      ~start: string=?,
      ~stop: string=?,
      ~sep: string=?,
      ~horizontal: bool=?,
      (Format.formatter, 'a) => unit,
      Format.formatter,
      t('a)
    ) =>
    unit;
};
