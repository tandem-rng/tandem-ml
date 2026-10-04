# Notes

## Install

```
git clone git@github.com:tandem-rng/tandem-ml && cd tandem-ml
opam install . --deps-only --with-test && dune build
```

Needs OCaml 5.3 or newer and a C compiler. The fills run a vendored copy of tandem-c at
`b049384`. Not published to opam.

## What it provides

- `Tandem.t`: a value generator. `seed`, `seed_u128`, `of_key`, `position`, `seek`.
- Scalar draws: `bool`, `u32`, `u64`, `float`.
- Bounded integers: `below32`, `below64`, `below`, `between`, `fill_below32`, `fill_below64`.
- Normals: `normal`, `normal2`, `fill_normal`.
- Exponentials: `exponential`, `fill_exponential`.
- Fills into `Bigarray.Array1` of `int32`, `int64`, `float64`, with `?off` and `?len`.
- Fills into `Float.Array`: `Tandem.Float_array`.
- `Tandem.Pure`: the same fills in OCaml alone, the reference for the C fills.
- `split`, `fork`, `purpose` and their `_u64` forms.
- `Tandem.State`: the shape of `Random.State` on a mutable generator.
- Normals and exponentials equal tandem-c bit for bit.

```ocaml
let g = Tandem.seed 42
let x, g = Tandem.float g
let n, g = Tandem.below g 1000
let z, g = Tandem.normal g
let a = Bigarray.(Array1.create float64 c_layout 1_000_000)
let g = Tandem.fill_normal g a
let worker = Tandem.split g 7
let g, kids = Tandem.fork g 4
```

- A generator `Tandem.t` is its transport form (128-bit key, 64-bit bit position, chunk length
  `K`) plus a cache of the current 1024-bit row that its copies share. Every draw returns the
  value and the successor generator.
- `fill_below32` and `fill_below64` take one draw of the plain fill per element and consume exactly
  one draw per element. A rejected draw retries on `split (purpose key P) g` of the key, `g` being
  the global draw index, so a fill cut anywhere equals the whole.
- A Box-Muller pair uses two uniform draws. `normal` returns its cos half and `normal2` the
  `(cos, sin)` pair. `fill_normal` fills pairs from draws `2j` and `2j + 1`, so an odd length uses
  the cos half of its last pair and consumes both draws.
- A bound of 0 returns 0 after one draw.
- A plain fill of 0 elements aligns the position. Bounded, normal and exponential fills of 0
  elements move nothing.
- `Tandem.State` has the shape of `Random.State`. `State.make` takes any int array: the first
  integer is the seed and each further integer `i` takes `split i`.

## Single precision

OCaml has no single precision float type. The library has no `Float32` draws, fills or
normals. Every float is a double from a 64-bit draw.

## C fills

The fills run the vendored `src/tandem.c` and `src/tandem.h`, a copy of tandem-c at `b049384`,
built by dune with `-O2 -ffp-contract=off` and OCaml's C compiler (clang on macOS). The stubs
in `src/tandem_stubs.c` read and write the generator's row cache, so a fill after a scalar draw,
or a scalar draw after a fill, does not reseed. OCaml checks every range and position before
the call, and the stubs neither allocate nor raise. A fill holds its domain until it returns.

`Tandem.Pure` holds the same fills in OCaml alone. The tests check that both give the same
values and end positions at unaligned starts, across rows and groups, with the cache in the
same row, an earlier row or another group.

To update the vendored copy:

```
cp ../tandem-c/tandem.c ../tandem-c/tandem.h src/
```

## Use

A fill at an offset reproduces part of a larger fill, so ranks and domains need no coordination:

```ocaml
let part key ~first a =
  Tandem.fill_float (Tandem.seek (Tandem.of_key key) (Int64.of_int (64 * first))) a
```

A generator and its copies share a cache of the current row. The cache changes no value, but do
not use copies of one generator from two domains at once. Give each domain its own generator
through `split`, `fork` or `purpose`.

## Tests

```
dune build @all @runtest
```

- Every spec vector, and long stream dumps from the Julia implementation.
- The `cross_below`, `cross_fill_below`, `cross_normal` and `cross_exponential` fixtures of
  tandem-c b049384, exact in every bit.
- Fills against scalar draws, cut fills, empty fills.
- C fills against `Tandem.Pure`, bit for bit.
- Hashes of 10^7 normals and 5 * 10^6 exponentials from tandem-c, moments and
  Kolmogorov-Smirnov tests on 10^7 normals and exponentials.

`dune build @all @runtest` runs three suites: `vectors` (every spec vector), `streams` (Julia
dumps as fills and scalar draws) and `derived` (tandem-c fixtures, fills against scalar draws,
cut fills, empty fills, C fills against `Tandem.Pure`, the hashes of the long normal and
exponential fills, and the moments and Kolmogorov-Smirnov distance of 10^7 normals and
exponentials).

Fixtures come from tandem-c b049384:

```
python3 tools/gen_derived.py ../tandem-c/tests > test/derived_data.ml
python3 tools/gen_vectors.py ../tandem-spec/vectors.json > test/vectors_data.ml
```

The long fill hashes are FNV-1a over the f64 fills of tandem-c's `tools/dump_normals.c` and
`tools/dump_exponentials.c` without their f32 parts: `0x8ea806b43afe58ed` for normals and
`0x8cb6728a73181814` for exponentials. The same tandem-c build gives the full dump hashes
`0x9414e1315e2653be` and `0x47f8f98297d94ee2` that tandem-c records.

## Speed

Apple M4, one core, OCaml 5.5.1 with flambda, `dune exec --release bench/bench.exe`. Fills of
2^22 elements, best of five. tandem-c on the same machine fills u32 at 19.0 GiB/s and f64
normals at 4.9.

| Fill | Melem/s | GiB/s |
|---|---|---|
| `fill_u32` | 4999 | 18.6 |
| `fill_float` | 2205 | 16.4 |
| `Float_array.fill_float` | 2205 | 16.4 |
| `fill_below32`, range 1000 | 2038 | 7.6 |
| `fill_normal` | 650 | 4.8 |
| `fill_exponential` | 811 | 6.0 |
| `Pure.fill_u32` | 349 | 1.3 |
| `Pure.fill_normal` | 66 | 0.49 |

| Scalar draw | ns |
|---|---|
| `Tandem.u32` | 3.8 |
| `Tandem.float` | 6.5 |
| `Tandem.below32` 1000 | 5.4 |
| `Tandem.normal` | 31 |
| `Tandem.State.bits` | 4.6 |
| `Tandem.State.bits64` | 6.7 |
| `Random.State.bits` (LXM, 30 bits) | 3.5 |
| `Random.State.bits64` | 3.5 |
| `Random.State.float 1.` | 3.5 |
| `Random.State.int 1000` | 3.6 |

The standard library has no fills and no normals. A value draw allocates its successor
generator, four words. The `State` draws allocate nothing.

## AI assistance

This port was written with the help of large language models under human
direction. The design and the specification are human work, as is much of the
Julia implementation. The code is tested bit for bit against every vector of
the specification and against long stream dumps from the Julia implementation,
and every value must match. The output does not depend on who or what wrote the
code.
