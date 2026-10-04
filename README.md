# tandem-ml

OCaml implementation of [Tandem8x32](https://github.com/tandem-rng/spec), a noncryptographic
pseudorandom number generator built to be fast on CPUs and GPUs alike. The package is `tandem`.
It produces the stream the specification defines, bit for bit.

- Pure OCaml 5.3 or newer, no C stubs, one dependency for the tests (`alcotest`).
- A generator `Tandem.t` is a value: its transport form (128-bit key, 64-bit bit position,
  chunk length `K`) plus a cache of the current 1024-bit row that its copies share. Every draw
  returns the value and the successor generator.
- Scalar draws: `bool`, `u32`, `u64`, `float` (53 bits), `float32` (24 bits, held in a float).
- Bounded integers (`below32`, `below64`, `below`, `between`) and standard normals and
  exponentials, scalar and as fills. They are not in the specification. They follow the shared
  device core in `tandem-cuda`, so every port returns the same integers and values. A bound of 0
  returns 0 after one draw.
- `fill_below32` and `fill_below64` take one draw of the plain fill per element and consume
  exactly one draw per element. A rejected draw retries on `split (purpose key P) g` of the key,
  `g` being the global draw index, so a fill cut anywhere equals the whole.
- A Box-Muller pair uses two uniform draws. `normal` returns its cos half and `normal2` the
  `(cos, sin)` pair. `fill_normal` fills pairs from draws `2j` and `2j + 1`, so an odd length
  uses the cos half of its last pair and consumes both draws.
- Fills write into `Bigarray.Array1` (`int32`, `int64`, `float32`, `float64`, C layout) and, for
  the float kinds, into `Float.Array` through `Tandem.Float_array`. Each takes `?off` and `?len`.
- `split`, `fork` and `purpose` derive children. `position` and `seek` move a generator in
  constant time.
- `Tandem.State` wraps a mutable generator in the shape of `Random.State`.

## Use

```ocaml
let () =
  let g = Tandem.seed 42 in
  let x, g = Tandem.float g in                      (* a draw and the next generator *)
  let n, g = Tandem.below g 1000 in                 (* uniform in [0, 1000) *)
  let z, g = Tandem.normal g in
  let a = Bigarray.(Array1.create float64 c_layout 1_000_000) in
  let g = Tandem.fill_normal g a in
  let worker = Tandem.split g 7 in                  (* by index, from the key alone *)
  let g, kids = Tandem.fork g 4 in                  (* from the current block *)
  ignore (x, n, z, worker, g, kids)
```

A fill at an offset reproduces part of a larger fill, so ranks and domains need no
coordination:

```ocaml
let part key ~first a =                             (* elements first.. of one global fill *)
  Tandem.fill_float (Tandem.seek (Tandem.of_key key) (Int64.of_int (64 * first))) a
```

A generator and its copies share a cache of the current row. The cache changes no value, but do
not use copies of one generator from two domains at once. Give each domain its own generator
through `split`, `fork` or `purpose`.

## Single precision

OCaml has no `float32`. The `_f32` functions hold single precision values in doubles and round
to single after every operation, so every `float32` normal and exponential is bit for bit what
tandem-c computes. A sum, product, quotient or root rounded once from a double is the single
result. A fused multiply-add is rounded twice, to double and then to single, which differs from
a single rounding only when the double result lands on a midpoint of two singles. No difference
shows in the tests below.

All multiply-adds of the normals and exponentials are `Float.fma`.

## Tests

`dune build @all @runtest` runs three suites with alcotest.

- `vectors`: every vector of the specification.
- `streams`: long dumps from the Julia implementation, as fills and as scalar draws.
- `derived`: the bounded, normal and exponential fixtures of tandem-c, exact in every bit, fills
  against scalar draws, fills cut at arbitrary elements, empty fills, `Float.Array` against
  bigarray fills, and the 1e6-pair normal and exponential dumps against tandem-c's hashes.

The fixtures are generated from tandem-c's headers at b049384:

```
python3 tools/gen_derived.py ../tandem-c/tests > test/derived_data.ml
python3 tools/gen_vectors.py ../tandem-spec/vectors.json > test/vectors_data.ml
dune exec tools/dump.exe -- normals | shasum -a 256
```

The dump of the normals has the SHA-256 `cfae418807a7d5f91ecd3e42c33a00943690c6e4b888ee39206738783efe9ded`
and the dump of the exponentials `5c035a4ef1368231d25a9c2f9201be2df3224e28a14549a50625d0db3770ef4e`,
the same bytes as tandem-c's `tools/dump_normals.c` and `tools/dump_exponentials.c`.

## Speed

Apple M4, one core, OCaml 5.5.1 with flambda, `dune exec --release bench/bench.exe`. Fills of
2^22 elements, best of five.

| Fill | Melem/s | GB/s |
|---|---|---|
| `fill_u32` | 364 | 1.46 |
| `fill_float` | 191 | 1.53 |
| `Float_array.fill_float` | 194 | 1.56 |
| `fill_below32`, range 1000 | 239 | 0.96 |
| `fill_normal` | 82 | 0.66 |
| `fill_normal32` | 57 | 0.23 |
| `fill_exponential` | 89 | 0.71 |
| `fill_exponential32` | 77 | 0.31 |

| Scalar draw | ns |
|---|---|
| `Tandem.u32` | 5.4 |
| `Tandem.float` | 7.6 |
| `Tandem.below32` 1000 | 6.5 |
| `Tandem.normal` | 44 |
| `Tandem.State.bits64` | 7.4 |
| `Random.State.bits` (LXM, 30 bits) | 3.5 |
| `Random.State.bits64` | 3.5 |
| `Random.State.float 1.` | 3.5 |
| `Random.State.int 1000` | 3.6 |

The standard library has no fills and no normals. `Random.State.float` at 3.5 ns per element is
about 290 Melem/s, against 191 for `fill_float`. A scalar draw of `Tandem` allocates its
successor generator. The fills are plain scalar OCaml, with no vector instructions.

## AI assistance

This port was written with the help of large language models under human
direction. The design and the specification are human work, as is much of the
Julia implementation. The code is tested bit for bit against every vector of
the specification and against long stream dumps from the Julia implementation,
and every value must match. The output does not depend on who or what wrote the
code.

## License

Apache License 2.0. See `LICENSE` and `NOTICE`.
