# Tests

```
dune build @all @runtest
```

## Suite

- Every spec vector, and long stream dumps from the Julia implementation.
- The conformance files of the spec, byte copies in `test/conformance`, read by
  `test/conformance.ml`, which demonstrates every item of the spec's `conformance/CHECKLIST.md`
  that the port offers. Every case of the bounded, normal, exponential and weighted choice files
  runs through the C fills, `Tandem.Pure`, both array kinds, the scalar draws and `Tandem.State`,
  and cut at elements 1, 7, 20, 21 and n - 1, bit for bit with its end position. The checks cover
  the choice tables, the global draw index of the fallback, the width from the range, empty fills,
  rejected weights, the choice law, random access, the position bounds, and the SHA-256 and FNV-1a
  of the streams and the `Float64` normal dumps in `hashes.json`. OCaml has no single precision
  float, so the `Float32` cases and the `Float32`, `Bool`, 128-bit, `Char`, binary16 and
  `UInt8` streams are counted and left out, and the fill-end check of section 5 cannot be reached
  with an array that fits in memory.
- Fills against scalar draws, cut fills, empty fills. Normal fills are cut on and around
  elements that take the ziggurat's fallback stream.
- C fills against `Tandem.Pure`, bit for bit.
- The hash of the `Float64` part of 5 * 10^6 exponentials from tandem-c, for both paths. Moments to the fourth order and
  Kolmogorov-Smirnov tests on 10^7 normals from each path and on 10^7 exponentials.

`dune build @all @runtest` runs four suites: `vectors` (every spec vector), `streams` (Julia
dumps as fills and scalar draws), `conformance` and `derived` (everything else above).

## Fixtures

`test/conformance/*.json` are byte copies of tandem-spec f420545 `conformance/*.json`, and CI
compares them with the spec. `python3 tools/gen_vectors.py ../tandem-spec/vectors.json >
test/vectors_data.ml` converts the spec vectors. The f64 part of tandem-c's
`tools/dump_exponentials.c` hashes to `0x8cb6728a73181814`, and the full dump to
`0x47f8f98297d94ee2` as tandem-c records.

## CI

CI runs `opam exec -- dune build @all @runtest` on ubuntu-latest and macos-latest with
OCaml 5.5 and 5.4.
