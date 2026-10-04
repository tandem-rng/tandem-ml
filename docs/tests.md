# Tests

```
dune build @all @runtest
```

## Suite

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

## Fixtures

Fixtures come from tandem-c b049384:

```
python3 tools/gen_derived.py ../tandem-c/tests > test/derived_data.ml
python3 tools/gen_vectors.py ../tandem-spec/vectors.json > test/vectors_data.ml
```

The long fill hashes are FNV-1a over the f64 fills of tandem-c's `tools/dump_normals.c` and
`tools/dump_exponentials.c` without their f32 parts: `0x8ea806b43afe58ed` for normals and
`0x8cb6728a73181814` for exponentials. The same tandem-c build gives the full dump hashes
`0x9414e1315e2653be` and `0x47f8f98297d94ee2` that tandem-c records.

## CI

CI runs `opam exec -- dune build @all @runtest` on ubuntu-latest and macos-latest with
OCaml 5.5 and 5.4.
