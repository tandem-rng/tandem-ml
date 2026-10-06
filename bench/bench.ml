(* Fills at 2^22 elements and scalar draws, each beside a Random.State (LXM in OCaml 5) loop that
   makes the same element type. Each figure is GiB/s of output, the best of five runs after one
   warm-up. Run on a quiet machine:
     dune exec --release bench/bench.exe *)

module A1 = Bigarray.Array1

let n = 1 lsl 22
let draws = 20_000_000

let best f =
  f ();
  let t = ref infinity in
  for _ = 1 to 5 do
    let t0 = Unix.gettimeofday () in
    f ();
    t := Float.min !t (Unix.gettimeofday () -. t0)
  done;
  !t

let gib ~count ~bytes f = float_of_int (count * bytes) /. best f /. 1073741824.
let row name x base_name base = Printf.printf "| `%s` | %.2f | `%s` | %.2f |\n%!" name x base_name base
let sink = ref 0

let () =
  let g = Tandem.seed 42 in
  let rs = Random.State.make [| 42 |] in
  let u32 = A1.create Bigarray.int32 Bigarray.c_layout n in
  let f64 = A1.create Bigarray.float64 Bigarray.c_layout n in
  let fa = Float.Array.create n in
  let fill ~bytes f = gib ~count:n ~bytes f in
  (* The baselines: a Random.State loop into an array of the same element type. *)
  let b_u32 =
    fill ~bytes:4 (fun () ->
        for i = 0 to n - 1 do
          A1.unsafe_set u32 i (Random.State.bits32 rs)
        done)
  in
  let b_float =
    fill ~bytes:8 (fun () ->
        for i = 0 to n - 1 do
          A1.unsafe_set f64 i (Random.State.float rs 1.)
        done)
  in
  let b_fa =
    fill ~bytes:8 (fun () ->
        for i = 0 to n - 1 do
          Float.Array.unsafe_set fa i (Random.State.float rs 1.)
        done)
  in
  let b_int =
    fill ~bytes:4 (fun () ->
        for i = 0 to n - 1 do
          A1.unsafe_set u32 i (Int32.of_int (Random.State.int rs 1000))
        done)
  in
  let fill_row name ~bytes f (base_name, base) = row name (fill ~bytes f) base_name base in
  let bits32_loop = ("Random.State.bits32", b_u32) in
  let float_loop = ("Random.State.float 1.", b_float) in
  let int_loop = ("Random.State.int 1000", b_int) in
  print_endline "| Fill | GiB/s | Baseline loop | GiB/s |\n|---|---|---|---|";
  fill_row "fill_u32" ~bytes:4 (fun () -> ignore (Tandem.fill_u32 g u32)) bits32_loop;
  fill_row "fill_float" ~bytes:8 (fun () -> ignore (Tandem.fill_float g f64)) float_loop;
  fill_row "Float_array.fill_float" ~bytes:8
    (fun () -> ignore (Tandem.Float_array.fill_float g fa))
    ("Random.State.float 1.", b_fa);
  fill_row "fill_below32, range 1000" ~bytes:4 (fun () -> ignore (Tandem.fill_below32 g ~range:1000 u32)) int_loop;
  fill_row "fill_normal" ~bytes:8 (fun () -> ignore (Tandem.fill_normal g f64)) float_loop;
  fill_row "fill_exponential" ~bytes:8 (fun () -> ignore (Tandem.fill_exponential g f64)) float_loop;
  fill_row "Pure.fill_u32" ~bytes:4 (fun () -> ignore (Tandem.Pure.fill_u32 g u32)) bits32_loop;
  fill_row "Pure.fill_float" ~bytes:8 (fun () -> ignore (Tandem.Pure.fill_float g f64)) float_loop;
  fill_row "Pure.fill_normal" ~bytes:8 (fun () -> ignore (Tandem.Pure.fill_normal g f64)) float_loop;
  fill_row "Pure.fill_exponential" ~bytes:8 (fun () -> ignore (Tandem.Pure.fill_exponential g f64)) float_loop;
  let scalar ~bytes f = gib ~count:draws ~bytes f in
  let r_bits32 =
    scalar ~bytes:4 (fun () ->
        let s = ref 0 in
        for _ = 1 to draws do
          s := !s + Int32.to_int (Random.State.bits32 rs)
        done;
        sink := !s)
  in
  let r_bits =
    scalar ~bytes:4 (fun () ->
        let s = ref 0 in
        for _ = 1 to draws do
          s := !s + Random.State.bits rs
        done;
        sink := !s)
  in
  let r_bits64 =
    scalar ~bytes:8 (fun () ->
        let s = ref 0L in
        for _ = 1 to draws do
          s := Int64.add !s (Random.State.bits64 rs)
        done;
        sink := Int64.to_int !s)
  in
  let r_float =
    scalar ~bytes:8 (fun () ->
        let s = ref 0. in
        for _ = 1 to draws do
          s := !s +. Random.State.float rs 1.
        done;
        sink := int_of_float !s)
  in
  let r_int =
    scalar ~bytes:4 (fun () ->
        let s = ref 0 in
        for _ = 1 to draws do
          s := !s + Random.State.int rs 1000
        done;
        sink := !s)
  in
  let value_int name draw base =
    row name
      (scalar ~bytes:4 (fun () ->
           let g = ref g and s = ref 0 in
           for _ = 1 to draws do
             let x, g' = draw !g in
             g := g';
             s := !s + x
           done;
           sink := !s))
      (fst base) (snd base)
  in
  let value_float name draw =
    row name
      (scalar ~bytes:8 (fun () ->
           let g = ref g and s = ref 0. in
           for _ = 1 to draws do
             let x, g' = draw !g in
             g := g';
             s := !s +. x
           done;
           sink := int_of_float !s))
      "Random.State.float 1." r_float
  in
  print_endline "\n| Scalar draw | GiB/s | Baseline | GiB/s |\n|---|---|---|---|";
  value_int "Tandem.u32" Tandem.u32 ("Random.State.bits32", r_bits32);
  value_float "Tandem.float" Tandem.float;
  value_int "Tandem.below32 1000" (fun g -> Tandem.below32 g 1000) ("Random.State.int 1000", r_int);
  value_float "Tandem.normal" Tandem.normal;
  let st = Tandem.State.make [| 42 |] in
  row "Tandem.State.bits"
    (scalar ~bytes:4 (fun () ->
         let s = ref 0 in
         for _ = 1 to draws do
           s := !s + Tandem.State.bits st
         done;
         sink := !s))
    "Random.State.bits" r_bits;
  row "Tandem.State.bits64"
    (scalar ~bytes:8 (fun () ->
         let s = ref 0L in
         for _ = 1 to draws do
           s := Int64.add !s (Tandem.State.bits64 st)
         done;
         sink := Int64.to_int !s))
    "Random.State.bits64" r_bits64;
  row "Tandem.State.float 1."
    (scalar ~bytes:8 (fun () ->
         let s = ref 0. in
         for _ = 1 to draws do
           s := !s +. Tandem.State.float st 1.
         done;
         sink := int_of_float !s))
    "Random.State.float 1." r_float;
  row "Tandem.State.int 1000"
    (scalar ~bytes:4 (fun () ->
         let s = ref 0 in
         for _ = 1 to draws do
           s := !s + Tandem.State.int st 1000
         done;
         sink := !s))
    "Random.State.int 1000" r_int
