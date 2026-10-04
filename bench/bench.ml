(* Fills at 2^22 elements and scalar draws against Random (LXM in OCaml 5). Each figure is the
   best of five runs after one warm-up. Run on a quiet machine:
     dune exec --release bench/bench.exe *)

module A1 = Bigarray.Array1

let n = 1 lsl 22
let scalar_draws = 20_000_000

let best f =
  f ();
  let t = ref infinity in
  for _ = 1 to 5 do
    let t0 = Unix.gettimeofday () in
    f ();
    t := Float.min !t (Unix.gettimeofday () -. t0)
  done;
  !t

let fill name ~bytes f =
  let t = best f in
  Printf.printf "| %s | %.0f | %.2f |\n%!" name (float_of_int n /. t /. 1e6) (float_of_int (n * bytes) /. t /. 1e9)

let sink = ref 0

let scalar ?(draws = scalar_draws) name f =
  let t = best f in
  Printf.printf "| %s | %.1f |\n%!" name (t /. float_of_int draws *. 1e9)

let () =
  let g = Tandem.seed 42 in
  let u32 = A1.create Bigarray.int32 Bigarray.c_layout n in
  let f64 = A1.create Bigarray.float64 Bigarray.c_layout n in
  let f32 = A1.create Bigarray.float32 Bigarray.c_layout n in
  let fa = Float.Array.create n in
  print_endline "| fill of 2^22 | Melem/s | GB/s |\n|---|---|---|";
  fill "u32" ~bytes:4 (fun () -> ignore (Tandem.fill_u32 g u32));
  fill "float64" ~bytes:8 (fun () -> ignore (Tandem.fill_float g f64));
  fill "float64, Float.Array" ~bytes:8 (fun () -> ignore (Tandem.Float_array.fill_float g fa));
  fill "bounded 32 (range 1000)" ~bytes:4 (fun () -> ignore (Tandem.fill_below32 g ~range:1000 u32));
  fill "normal float64" ~bytes:8 (fun () -> ignore (Tandem.fill_normal g f64));
  fill "normal float32" ~bytes:4 (fun () -> ignore (Tandem.fill_normal32 g f32));
  fill "exponential float64" ~bytes:8 (fun () -> ignore (Tandem.fill_exponential g f64));
  fill "exponential float32" ~bytes:4 (fun () -> ignore (Tandem.fill_exponential32 g f32));
  print_endline "\n| scalar draw | ns |\n|---|---|";
  scalar "Tandem.u32" (fun () ->
      let g = ref g and s = ref 0 in
      for _ = 1 to scalar_draws do
        let x, g' = Tandem.u32 !g in
        g := g';
        s := !s + x
      done;
      sink := !s);
  scalar "Tandem.float" (fun () ->
      let g = ref g and s = ref 0. in
      for _ = 1 to scalar_draws do
        let x, g' = Tandem.float !g in
        g := g';
        s := !s +. x
      done;
      sink := int_of_float !s);
  scalar "Tandem.below32 1000" (fun () ->
      let g = ref g and s = ref 0 in
      for _ = 1 to scalar_draws do
        let x, g' = Tandem.below32 !g 1000 in
        g := g';
        s := !s + x
      done;
      sink := !s);
  scalar ~draws:(scalar_draws / 10) "Tandem.normal" (fun () ->
      let g = ref g and s = ref 0. in
      for _ = 1 to scalar_draws / 10 do
        let x, g' = Tandem.normal !g in
        g := g';
        s := !s +. x
      done;
      sink := int_of_float !s);
  scalar "Tandem.State.bits64" (fun () ->
      let st = Tandem.State.make_seed 42 and s = ref 0L in
      for _ = 1 to scalar_draws do
        s := Int64.add !s (Tandem.State.bits64 st)
      done;
      sink := Int64.to_int !s);
  let st = Random.State.make [| 42 |] in
  scalar "Random.State.bits (30 bits)" (fun () ->
      let s = ref 0 in
      for _ = 1 to scalar_draws do
        s := !s + Random.State.bits st
      done;
      sink := !s);
  scalar "Random.State.bits64" (fun () ->
      let s = ref 0L in
      for _ = 1 to scalar_draws do
        s := Int64.add !s (Random.State.bits64 st)
      done;
      sink := Int64.to_int !s);
  scalar "Random.State.float 1." (fun () ->
      let s = ref 0. in
      for _ = 1 to scalar_draws do
        s := !s +. Random.State.float st 1.
      done;
      sink := int_of_float !s);
  scalar "Random.State.int 1000" (fun () ->
      let s = ref 0 in
      for _ = 1 to scalar_draws do
        s := !s + Random.State.int st 1000
      done;
      sink := !s)
