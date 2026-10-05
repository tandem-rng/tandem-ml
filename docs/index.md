# tandem-ml

OCaml implementation of Tandem8x32. It produces the stream of the
[specification](https://github.com/tandem-rng/spec/blob/main/SPEC.md) bit for bit. Fills run a
vendored tandem-c through C stubs, with a pure OCaml fallback in `Tandem.Pure`.

- [API](api.md): the generator, its draws and fills, `Tandem.State` and parallel use.
- [Design](design.md): the C fills, bounded integers, normals and exponentials.
- [Tests](tests.md): what the suites check, the fixtures, and what CI runs.
- [Speed](speed.md): Apple M4 figures against `Random.State`.

## Install

```
git clone git@github.com:tandem-rng/tandem-ml && cd tandem-ml
opam install . --deps-only --with-test && dune build
```

Needs OCaml 5.3 or newer and a C compiler. The fills run a vendored copy of tandem-c at
`121db59`. Not published to opam.

## AI assistance

This port was written with the help of large language models under human
direction. The design and the specification are human work, as is much of the
Julia implementation. The code is tested bit for bit against every vector of
the specification and against long stream dumps from the Julia implementation,
and every value must match. The output does not depend on who or what wrote the
code.
