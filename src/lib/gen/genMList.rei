/* This file is free software, part of gen. See file "license" for more details. */

/** {1 Efficient Mutable Lists}

    Unrolled lists, append-only, used for storing the content of a generator.

    Example:
    {[
      let g = 1 -- 1000 ;;
      val g : int t = <fun>

      let c = g |> MList.of_gen_lazy |> MList.to_clonable;;
      val c : int clonable = <obj>

                c#next |> take 500 |> to_list;;
      - : int list = [1; 2; 3; .....; 500]

      let c' = c#clone ;;
      val c' : int clonable = <obj>

                 c |> to_list;;
      - : int list = [501; 502; ....; 1000]

        c'#gen |> to_list;;   (* c consumed, but not c' *)
      - : int list = [501; 502; ....; 1000]

        c#gen |> to_list;;
      - : int list = []
    ]}

    @since 0.2.3 */;

type gen('a) = unit => option('a);
type clonable('a) = {
  .
  /** Generator of values tied to this copy */ gen: gen('a),
  /** Clone the internal state */ clone: clonable('a),
};

/** An internal append-only storage of elements of type 'a, produced from
    a generator */

type t('a);

/** [of_gen g] consumes [g] to build a mlist */

let of_gen: gen('a) => t('a);

/** [of_gen_lazy g] makes a mlist that will read from [g] as required,
    until [g] is exhausted. Do not use [g] directly after this, or
    some elements will be absent from the mlist!
    @param caching if true or absent, values are read from the generator
      by chunks of increasing size. If false, values are read one by one.
    @param max_chunk_size if provided and [caching = true],
      sets the (maximal) size of the internal chunks */

let of_gen_lazy:
  (~max_chunk_size: int=?, ~caching: bool=?, gen('a)) => t('a);

/** Iterate on the mlist. This function can be called many times without
    any problem, the mlist isn't consumable! */

let to_gen: t('a) => gen('a);

let to_clonable: t('a) => clonable('a);
