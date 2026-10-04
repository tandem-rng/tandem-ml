# Notes

## What it provides

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

## Use

A fill at an offset reproduces part of a larger fill, so ranks and domains need no coordination:

```ocaml
let part key ~first a =
  Tandem.fill_float (Tandem.seek (Tandem.of_key key) (Int64.of_int (64 * first))) a
```

A generator and its copies share a cache of the current row. The cache changes no value, but do
not use copies of one generator from two domains at once. Give each domain its own generator
through `split`, `fork` or `purpose`.

## Single precision

OCaml has no `float32`. The `_f32` functions hold single precision values in doubles and round to
single after every operation, so every `float32` normal and exponential is bit for bit what
tandem-c computes. A sum, product, quotient or root rounded once from a double is the single
result. A fused multiply-add is rounded twice, to double and then to single, which differs from a
single rounding only when the double result lands on a midpoint of two singles. No difference shows
in the tests. All multiply-adds of the normals and exponentials are `Float.fma`.

## Tests

`dune build @all @runtest` runs three suites: `vectors` (every spec vector), `streams` (Julia
dumps as fills and scalar draws) and `derived` (tandem-c fixtures, fills against scalar draws,
cut fills, empty fills, `Float.Array` against bigarray fills, the 1e6-pair hashes).

Fixtures come from tandem-c b049384:

```
python3 tools/gen_derived.py ../tandem-c/tests > test/derived_data.ml
python3 tools/gen_vectors.py ../tandem-spec/vectors.json > test/vectors_data.ml
dune exec tools/dump.exe -- normals | shasum -a 256
```

SHA-256 of the normals dump: `cfae418807a7d5f91ecd3e42c33a00943690c6e4b888ee39206738783efe9ded`.
SHA-256 of the exponentials dump: `5c035a4ef1368231d25a9c2f9201be2df3224e28a14549a50625d0db3770ef4e`.
Both equal tandem-c's `tools/dump_normals.c` and `tools/dump_exponentials.c`.

## Speed

The standard library has no fills and no normals. `Random.State.float` at 3.5 ns per element is
about 290 Melem/s, against 191 for `fill_float`. A scalar draw of `Tandem` allocates its successor
generator. The fills are scalar OCaml, with no vector instructions.
