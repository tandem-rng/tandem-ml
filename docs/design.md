# Design

## Fills

The fills run the vendored `src/tandem.c`, `src/tandem.h` and `src/tandem_normal_tables.h`, a
copy of tandem-c at `121db59`, built by dune with `-O2 -ffp-contract=off` and OCaml's C
compiler (clang on macOS). The stubs
in `src/tandem_stubs.c` read and write the generator's row cache, so a fill after a scalar draw,
or a scalar draw after a fill, does not reseed. OCaml checks every range and position before
the call, and the stubs neither allocate nor raise. A fill holds its domain until it returns.

## Scalar draws

A scalar draw reads a buffer of eight rows, 1 KiB, that one C fill refills, so it costs no
stub call and allocates nothing beyond a value draw's successor generator (four words). Every
generator key carries this buffer and the 512-byte row cache. `Tandem.State` keeps its position
in a `Bytes` block: OCaml 5 on arm64 stores a mutable field with a release store, which
doubled the time of a draw.

## Pure OCaml

`Tandem.Pure` holds the same fills in OCaml alone. The tests check that both give the same
values and end positions at unaligned starts, across rows and groups, with the cache in the
same row, an earlier row or another group.

To update the vendored copy:

```
cp ../tandem-c/tandem.c ../tandem-c/tandem.h ../tandem-c/tandem_normal_tables.h src/
```

## Bounded integers

`fill_below32` and `fill_below64` take one draw of the plain fill per element and consume
exactly one draw per element. A rejected draw retries on `split (purpose key P) g` of the key,
`g` being the global draw index, so a fill cut anywhere equals the whole.

## Normals and exponentials

Normals follow the 1024-layer ziggurat of Appendix A. Element `i` of `fill_normal` comes from
draw `i` of the u64 fill, and `normal` takes one 64-bit draw. A draw outside the inner
rectangles, 0.43 % of them, continues on `split (purpose key 0x4e524d3634) g`, `g` being the
global draw index, so a fill cut anywhere equals the whole and the fallback never moves the
generator. The scalar draws pass such a draw to tandem.c, and `Tandem.Pure` runs it in OCaml,
seeding only the fallback lanes it reads. The OCaml code takes the tables from `src/zig_tables.ml`, which
`tools/gen_zig_tables.py` writes from the spec's `tables/normal_f64_zig1024.json`:

```
python3 tools/gen_zig_tables.py ../tandem-spec/tables/normal_f64_zig1024.json > src/zig_tables.ml
```

The logarithm is the reference one of Appendix A with `Float.fma`, and OCaml fuses no other
operation. Normals and exponentials equal tandem-c bit for bit.
