# Design

## Fills

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

## Bounded integers

`fill_below32` and `fill_below64` take one draw of the plain fill per element and consume
exactly one draw per element. A rejected draw retries on `split (purpose key P) g` of the key,
`g` being the global draw index, so a fill cut anywhere equals the whole.

## Normals and exponentials

A Box-Muller pair uses two uniform draws. `normal` returns its cos half and `normal2` the
`(cos, sin)` pair. `fill_normal` fills pairs from draws `2j` and `2j + 1`, so an odd length
uses the cos half of its last pair and consumes both draws. Normals and exponentials equal
tandem-c bit for bit.
