# Speed

`dune exec --release bench/bench.exe` produces the figures.

## CPU

Apple M4, one core, OCaml 5.5.1 with flambda, `dune exec --release bench/bench.exe`. Fills of
2^22 elements, best of five. tandem-c on the same machine fills u32 at 19.0 GiB/s and f64
normals at 7.7. The normal rows are not yet measured for the ziggurat.

| Fill | Melem/s | GiB/s |
|---|---|---|
| `fill_u32` | 4999 | 18.6 |
| `fill_float` | 2205 | 16.4 |
| `Float_array.fill_float` | 2205 | 16.4 |
| `fill_below32`, range 1000 | 2038 | 7.6 |
| `fill_normal` | pending | pending |
| `fill_exponential` | 811 | 6.0 |
| `Pure.fill_u32` | 349 | 1.3 |
| `Pure.fill_normal` | pending | pending |

| Scalar draw | ns |
|---|---|
| `Tandem.u32` | 3.8 |
| `Tandem.float` | 6.5 |
| `Tandem.below32` 1000 | 5.4 |
| `Tandem.normal` | pending |
| `Tandem.State.bits` | 4.6 |
| `Tandem.State.bits64` | 6.7 |
| `Random.State.bits` (LXM, 30 bits) | 3.5 |
| `Random.State.bits64` | 3.5 |
| `Random.State.float 1.` | 3.5 |
| `Random.State.int 1000` | 3.6 |

The standard library has no fills and no normals. A value draw allocates its successor
generator, four words. The `State` draws allocate nothing.
