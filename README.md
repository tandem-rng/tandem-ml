<p align="center"><img src="assets/lockup.png" width="560" alt="tandem rng .ml"></p>

# tandem-ml

[![CI](https://github.com/tandem-rng/tandem-ml/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/tandem-rng/tandem-ml/actions/workflows/ci.yml)
[![Docs](https://img.shields.io/badge/docs-tandem--rng.github.io-7fb3ee.svg)](https://tandem-rng.github.io/tandem-ml/)
[![License: Apache 2.0](https://img.shields.io/badge/license-Apache_2.0-blue.svg)](LICENSE)

OCaml implementation of [Tandem8x32](https://github.com/tandem-rng/spec), a noncryptographic
pseudorandom number generator. It produces the specified stream bit for bit. Fills run a vendored
tandem-c through C stubs, with a pure OCaml fallback in `Tandem.Pure`.

## Install

Install with opam and dune.

```
git clone git@github.com:tandem-rng/tandem-ml && cd tandem-ml
opam install . --deps-only --with-test && dune build
dune build @all @runtest
```

## Quick start

```ocaml
let g = Tandem.seed 42
let a = Bigarray.(Array1.create float64 c_layout 1_000_000)
let g = Tandem.fill_normal g a                   (* ziggurat, bit identical to tandem-c *)
let worker = Tandem.split g 7                    (* by index, from the key alone *)
let n, worker = Tandem.below worker 1000         (* uniform on [0, 1000), Lemire's method *)
```

## Requirements

It needs OCaml 5.3 or newer and a C compiler. The vendored tandem-c
is at `121db59`. The package `tandem` is not published to opam.
OCaml has no single precision float, so there are no `Float32` draws.

## Documentation

See [API](docs/api.md) for every draw and fill, and [design](docs/design.md),
[tests](docs/tests.md) and [speed](docs/speed.md) for the rest.

Portions of the code were generated with the assistance of LLMs.

[Docs](https://tandem-rng.github.io/tandem-ml/) · [Specification](https://github.com/tandem-rng/spec/blob/main/SPEC.md) · [Website](https://tandem-rng.github.io/) · [Issues](https://github.com/tandem-rng/tandem-ml/issues) · [License](LICENSE)
