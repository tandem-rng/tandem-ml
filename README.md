# tandem-ml

[![CI](https://github.com/tandem-rng/tandem-ml/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/tandem-rng/tandem-ml/actions/workflows/ci.yml)
[![License: Apache 2.0](https://img.shields.io/badge/license-Apache_2.0-blue.svg)](LICENSE)

OCaml implementation of [Tandem8x32](https://github.com/tandem-rng/spec), a noncryptographic
pseudorandom number generator. It produces the specified stream bit for bit. Fills run a vendored
tandem-c through C stubs, with a pure OCaml fallback in `Tandem.Pure`.

Install with opam and dune. It needs OCaml 5.3 or newer and a C compiler. The vendored tandem-c
is at `b049384`. The package `tandem` is not published to opam.

```
git clone git@github.com:tandem-rng/tandem-ml && cd tandem-ml
opam install . --deps-only --with-test && dune build
dune build @all @runtest
```

```ocaml
let g = Tandem.seed 42
let a = Bigarray.(Array1.create float64 c_layout 1_000_000)
let g = Tandem.fill_normal g a                   (* Box-Muller, bit identical to tandem-c *)
let worker = Tandem.split g 7                    (* by index, from the key alone *)
let n, worker = Tandem.below worker 1000         (* uniform on [0, 1000), Lemire's method *)
```

OCaml has no single precision float, so there are no `Float32` draws. See
[docs/notes.md](docs/notes.md) for the API, tests and speed.

Portions of the code were generated with the assistance of LLMs.

[Documentation](docs/notes.md) · [Apache 2.0 license](LICENSE)
