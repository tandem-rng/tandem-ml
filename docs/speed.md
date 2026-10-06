# Speed

`dune exec --release bench/bench.exe` produces the figures. Every figure is GiB/s of output.

## CPU

Apple M4, one core, OCaml 5.5.1 with flambda. Fills of 2^22 elements, best of five. A scalar draw
counts the bytes of its value: 4 for a 32-bit draw, 8 for a Float64 draw. Each row has a
`Random.State` (LXM) baseline that makes the same element type, a loop into an array of that
type for the fills. The standard library has no normals or exponentials, so those rows compare
against a `Random.State.float` loop.

| Fill | GiB/s | Baseline loop | GiB/s |
|---|---|---|---|
| `fill_u32` | 19.24 | `Random.State.bits32` | 1.06 |
| `fill_float` | 16.66 | `Random.State.float 1.` | 2.09 |
| `Float_array.fill_float` | 16.66 | `Random.State.float 1.` | 2.14 |
| `fill_below32`, range 1000 | 7.75 | `Random.State.int 1000` | 1.04 |
| `fill_normal` | 7.78 | `Random.State.float 1.` | 2.09 |
| `fill_exponential` | 6.07 | `Random.State.float 1.` | 2.09 |
| `Pure.fill_u32` | 1.94 | `Random.State.bits32` | 1.06 |
| `Pure.fill_float` | 1.97 | `Random.State.float 1.` | 2.09 |
| `Pure.fill_normal` | 1.03 | `Random.State.float 1.` | 2.09 |
| `Pure.fill_exponential` | 0.74 | `Random.State.float 1.` | 2.09 |

| Scalar draw | GiB/s | Baseline | GiB/s |
|---|---|---|---|
| `Tandem.u32` | 2.09 | `Random.State.bits32` | 1.08 |
| `Tandem.float` | 3.57 | `Random.State.float 1.` | 2.13 |
| `Tandem.below32 1000` | 1.30 | `Random.State.int 1000` | 1.05 |
| `Tandem.normal` | 2.25 | `Random.State.float 1.` | 2.13 |
| `Tandem.State.bits` | 1.96 | `Random.State.bits` (30 bits) | 1.09 |
| `Tandem.State.bits64` | 3.69 | `Random.State.bits64` | 2.14 |
| `Tandem.State.float 1.` | 3.60 | `Random.State.float 1.` | 2.13 |
| `Tandem.State.int 1000` | 1.48 | `Random.State.int 1000` | 1.05 |

A value draw allocates its successor generator, four words. The `State` draws allocate
nothing. `Pure.fill_exponential` spends most of its time in `Float.fma`, a C call in OCaml.
