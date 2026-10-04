(* Long-stream agreement with TandemRNG.jl. The dumps in data/ are raw little-endian fills
   written by the Julia reference (tandem-c's tools/dump_streams.jl). Each is compared against a
   fill and against scalar draws, which must end at the same position. *)

module A1 = Bigarray.Array1

let dump name = In_channel.with_open_bin ("data/" ^ name) In_channel.input_all

let make name =
  if String.starts_with ~prefix:"k1234_K8_" name then Tandem.of_key ~chunk_length:8 [| 1; 2; 3; 4 |]
  else if String.starts_with ~prefix:"k1234_" name then Tandem.of_key [| 1; 2; 3; 4 |]
  else Tandem.seed 42


let u32_dump name () =
  let bytes = dump name in
  let n = String.length bytes / 4 in
  let want i = Int32.to_int (String.get_int32_le bytes (4 * i)) land 0xffff_ffff in
  let a = A1.create Bigarray.int32 Bigarray.c_layout n in
  let filled = Tandem.fill_u32 (make name) a in
  let g = ref (make name) in
  for i = 0 to n - 1 do
    if Int32.to_int a.{i} land 0xffff_ffff <> want i then Alcotest.failf "%s: fill differs at %d" name i;
    let x, g' = Tandem.u32 !g in
    g := g';
    if x <> want i then Alcotest.failf "%s: draw differs at %d" name i
  done;
  Alcotest.(check int64) "end position" (Tandem.position filled) (Tandem.position !g)

let u64_dump name () =
  let bytes = dump name in
  let n = String.length bytes / 8 in
  let a = A1.create Bigarray.int64 Bigarray.c_layout n in
  let filled = Tandem.fill_u64 (make name) a in
  let g = ref (make name) in
  for i = 0 to n - 1 do
    let want = String.get_int64_le bytes (8 * i) in
    if a.{i} <> want then Alcotest.failf "%s: fill differs at %d" name i;
    let x, g' = Tandem.u64 !g in
    g := g';
    if x <> want then Alcotest.failf "%s: draw differs at %d" name i
  done;
  Alcotest.(check int64) "end position" (Tandem.position filled) (Tandem.position !g)

let f64_dump name () =
  let bytes = dump name in
  let n = String.length bytes / 8 in
  let a = A1.create Bigarray.float64 Bigarray.c_layout n in
  let fa = Float.Array.create n in
  let filled = Tandem.fill_float (make name) a in
  ignore (Tandem.Float_array.fill_float (make name) fa);
  let g = ref (make name) in
  for i = 0 to n - 1 do
    let want = Int64.float_of_bits (String.get_int64_le bytes (8 * i)) in
    if a.{i} <> want || Float.Array.get fa i <> want then Alcotest.failf "%s: fill differs at %d" name i;
    let x, g' = Tandem.float !g in
    g := g';
    if x <> want then Alcotest.failf "%s: draw differs at %d" name i
  done;
  Alcotest.(check int64) "end position" (Tandem.position filled) (Tandem.position !g)

let f32_dump name () =
  let bytes = dump name in
  let n = String.length bytes / 4 in
  let a = A1.create Bigarray.float32 Bigarray.c_layout n in
  let fa = Float.Array.create n in
  let filled = Tandem.fill_float32 (make name) a in
  ignore (Tandem.Float_array.fill_float32 (make name) fa);
  let g = ref (make name) in
  for i = 0 to n - 1 do
    let want = Int32.float_of_bits (String.get_int32_le bytes (4 * i)) in
    if a.{i} <> want || Float.Array.get fa i <> want then Alcotest.failf "%s: fill differs at %d" name i;
    let x, g' = Tandem.float32 !g in
    g := g';
    if x <> want then Alcotest.failf "%s: draw differs at %d" name i
  done;
  Alcotest.(check int64) "end position" (Tandem.position filled) (Tandem.position !g)

let bool_dump name () =
  let bytes = dump name in
  let g = ref (make name) in
  String.iteri
    (fun i c ->
      let b, g' = Tandem.bool !g in
      g := g';
      if b <> (c <> '\000') then Alcotest.failf "%s: draw differs at %d" name i)
    bytes;
  Alcotest.(check int64) "end position" (Int64.of_int (String.length bytes)) (Tandem.position !g)

let () =
  Alcotest.run "streams"
    [
      ( "dumps",
        [
          Alcotest.test_case "u32, key 1 2 3 4, K = 8" `Quick (u32_dump "k1234_K8_u32.bin");
          Alcotest.test_case "u32, key 1 2 3 4, K = 32" `Quick (u32_dump "k1234_K32_u32.bin");
          Alcotest.test_case "u64, key 1 2 3 4, K = 32" `Quick (u64_dump "k1234_K32_u64.bin");
          Alcotest.test_case "f64, seed 42" `Quick (f64_dump "seed42_K32_f64.bin");
          Alcotest.test_case "f32, seed 42" `Quick (f32_dump "seed42_K32_f32.bin");
          Alcotest.test_case "bool, seed 42" `Quick (bool_dump "seed42_K32_bool.bin");
        ] );
    ]
