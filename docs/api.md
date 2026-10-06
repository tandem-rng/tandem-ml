# API

## Use

```ocaml
let g = Tandem.seed 42
let x, g = Tandem.float g
let n, g = Tandem.below g 1000
let z, g = Tandem.normal g
let a = Bigarray.(Array1.create float64 c_layout 1_000_000)
let g = Tandem.fill_normal g a
let worker = Tandem.split g 7
let g, kids = Tandem.fork g 4
```

## Reference

- `Tandem.t`: a value generator. `seed`, `seed_u128`, `of_key`, `position`, `seek`.
- Scalar draws: `bool`, `u32`, `u64`, `float`.
- Bounded integers: `below32`, `below64`, `below`, `between`, `fill_below32`, `fill_below64`.
- Normals: `normal`, `fill_normal`.
- Exponentials: `exponential`, `fill_exponential`.
- Weighted choice, Appendix C: `Choice.create` builds the alias table of a `float array` in exact
  integers and raises `Invalid_argument` unless the weights are finite, not negative and not all
  zero. `choice` and `fill_choice` return indices, one 64-bit draw each with no retry, so a fill cut
  anywhere equals the whole fill. `Choice.size`, `capacity`, `cut` and `alias` expose the table.
  The fills run over the C or the pure 64-bit fill.
- Fills into `Bigarray.Array1` of `int32`, `int64`, `float64`, with `?off` and `?len`.
- Fills into `Float.Array`: `Tandem.Float_array`.
- `Tandem.Pure`: the same fills in OCaml alone, the reference for the C fills.
- `split`, `fork`, `purpose` and their `_u64` forms.
- `Tandem.State`: the shape of `Random.State` on a mutable generator.
- A generator `Tandem.t` is its transport form (128-bit key, 64-bit bit position, chunk length
  `K`) plus a cache of the current 1024-bit row that its copies share. Every draw returns the
  value and the successor generator.
- A bound of 0 returns 0 after one draw.
- A plain, normal or choice fill of 0 elements aligns the position. Bounded and exponential fills of 0
  elements move nothing.
- `Tandem.State` has the shape of `Random.State`. `State.make` takes any int array: the first
  integer is the seed and each further integer `i` takes `split i`.

## Single precision

OCaml has no single precision float type. The library has no `Float32` draws, fills or
normals. Every float is a double from a 64-bit draw.

## Parallel use

A fill at an offset reproduces part of a larger fill, so ranks and domains need no coordination:

```ocaml
let part key ~first a =
  Tandem.fill_float (Tandem.seek (Tandem.of_key key) (Int64.of_int (64 * first))) a
```

A generator and its copies share a cache of the current row. The cache changes no value, but do
not use copies of one generator from two domains at once. Give each domain its own generator
through `split`, `fork` or `purpose`.
