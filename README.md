# tandem-ml

OCaml implementation of [Tandem8x32](https://github.com/tandem-rng/spec), a noncryptographic
pseudorandom number generator. It produces the specified stream bit for bit and is fast on CPUs
and GPUs alike. The package is `tandem`.

## Install

```
git clone git@github.com:tandem-rng/tandem-ml && cd tandem-ml
opam install . --deps-only --with-test && dune build
```

Needs OCaml 5.3 or newer and a C compiler. The fills run a vendored copy of tandem-c at
`b049384`. Not published to opam.

## Use

```ocaml
let g = Tandem.seed 42
let x, g = Tandem.float g
let n, g = Tandem.below g 1000
let z, g = Tandem.normal g
let a = Bigarray.(Array1.create float64 c_layout 1_000_000)
let g = Tandem.fill_normal g a
```

```ocaml
let worker = Tandem.split g 7
let g, kids = Tandem.fork g 4
```

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

OCaml has no single precision float, so there are no `Float32` draws.

More in [docs/notes.md](docs/notes.md).

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

## AI assistance

This port was written with the help of large language models under human
direction. The design and the specification are human work, as is much of the
Julia implementation. The code is tested bit for bit against every vector of
the specification and against long stream dumps from the Julia implementation,
and every value must match. The output does not depend on who or what wrote the
code.

## License

Apache License 2.0. See `LICENSE` and `NOTICE`.
