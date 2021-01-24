https://github.com/rauschma/reasonml-demo-iterators

[ParseChunkTest.re](./__tests__/ParseChunkTest.re) is broken.

```bash
./__tests__/ParseChunksTest.re:36:44-51

  34 │ test("linesToChunks", () => {
  35 │   let linesGen = Gen.of_array(lineArray);
  36 │   let chunkGen = ParseChunks.linesToChunks(linesGen);
  37 │   expect(Gen.to_list(chunkGen))
  38 │     |> toEqual(chunkList);

  This has type: unit => option(option(Js.String.t))
  Somewhere wanted:
    ReasonmlDemoIterators.Gen.t(string) (defined as unit => option(string))

  The incompatible parts:
    option(Js.String.t) vs string

FAILED: cannot make progress due to previous errors.
>>>> Finish compiling(exit: 1)
```

and

```bash
We've found a bug for you!
./__tests__/ParseChunksTest.re:43:47-54

  41 │ test("linesToChunksImp", () => {
  42 │   let linesGen = Gen.of_array(lineArray);
  43 │   let chunkGen = ParseChunks.linesToChunksImp(linesGen);
  44 │   expect(Gen.to_list(chunkGen))
  45 │     |> toEqual(chunkList);

  This has type: unit => option(option(Js.String.t))
  Somewhere wanted:
    ReasonmlDemoIterators.Gen.t(string) (defined as unit => option(string))

  The incompatible parts:
    option(Js.String.t) vs string

FAILED: cannot make progress due to previous errors.
>>>> Finish compiling(exit: 1)
```
