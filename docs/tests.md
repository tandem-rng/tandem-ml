# Tests

```
dune build @all @runtest
```

## Suite

- Every spec vector, and long stream dumps from the Julia implementation.
- The `cross_below`, `cross_fill_below`, `cross_normal` and `cross_exponential` fixtures of
  tandem-c 121db59, exact in every bit, through the C fills, `Tandem.Pure`, the scalar draws
  and `Tandem.State`.
- Fills against scalar draws, cut fills, empty fills. Normal fills are cut on and around
  elements that take the ziggurat's fallback stream.
- C fills against `Tandem.Pure`, bit for bit.
- Hashes of 5 * 10^6 normals and 5 * 10^6 exponentials from tandem-c, and of the normals of a
  Python implementation of Appendix A, for both paths. Moments to the fourth order and
  Kolmogorov-Smirnov tests on 10^7 normals from each path and on 10^7 exponentials.

`dune build @all @runtest` runs three suites: `vectors` (every spec vector), `streams` (Julia
dumps as fills and scalar draws) and `derived` (everything else above).

## Fixtures

Fixtures come from tandem-c 121db59, whose `tests/cross_normal.h` has the SHA-256
`3cd7c8f9178711255718288eb712eaccb33a1726d2a185f412f13590398ad3ac`:

```
python3 tools/gen_derived.py ../tandem-c/tests > test/derived_data.ml
python3 tools/gen_vectors.py ../tandem-spec/vectors.json > test/vectors_data.ml
```

The long fill hashes are FNV-1a. The normals of tandem-c's `tools/dump_normals.c`, 10^6 from
each of five starts of `seed_u128 2026L 7L`, hash to `0xa61cfa844c85f7c1`, and their bytes have
the SHA-256 `700ec4d2f4d6b82aaa56c6eff18a4e5919585fdbd093988773383d580ea610d1`. The Python
reference gives `0x0c4059ed409d578d` and `0x30ce40c86b295193` for 2 * 10^5 normals from key
`{1, 2, 3, 4}` at bits 0 and 2373. The f64 part of `tools/dump_exponentials.c` hashes to
`0x8cb6728a73181814`, and the full dump to `0x47f8f98297d94ee2` as tandem-c records.

## CI

CI runs `opam exec -- dune build @all @runtest` on ubuntu-latest and macos-latest with
OCaml 5.5 and 5.4.
