# Speed

`dune exec --release bench/bench.exe` produces the figures. Every figure is GiB/s of output.

## CPU

Apple M4, one core, OCaml 5.5.1 with flambda. Fills of 2^22 elements, best of five. A scalar draw
counts the bytes of its value: 4 for a 32-bit draw, 8 for a Float64 draw. tandem-c's own bench on
the same machine in the same session fills u32 at 18.55 GiB/s, f64 at 16.05 and f64 normals at
7.43.

| Fill | GiB/s |
|---|---|
| `fill_u32` | 18.71 |
| `fill_float` | 16.17 |
| `Float_array.fill_float` | 16.13 |
| `fill_below32`, range 1000 | 7.66 |
| `fill_normal` | 7.74 |
| `fill_exponential` | 6.12 |
| `Pure.fill_u32` | 1.31 |
| `Pure.fill_float` | 1.36 |
| `Pure.fill_normal` | 0.61 |
| `Pure.fill_exponential` | 0.62 |

| Scalar draw | GiB/s |
|---|---|
| `Tandem.u32` | 0.93 |
| `Tandem.float` | 1.11 |
| `Tandem.below32` 1000 | 0.66 |
| `Tandem.normal` | 0.50 |
| `Tandem.State.bits` | 0.75 |
| `Tandem.State.bits64` | 1.00 |
| `Tandem.State.float 1.` | 1.01 |
| `Tandem.State.int 1000` | 0.67 |
| `Random.State.bits` (LXM, 30 bits) | 1.04 |
| `Random.State.bits64` | 2.09 |
| `Random.State.float 1.` | 2.01 |
| `Random.State.int 1000` | 0.97 |

The standard library has no fills and no normals. A value draw allocates its successor
generator, four words. The `State` draws allocate nothing.
