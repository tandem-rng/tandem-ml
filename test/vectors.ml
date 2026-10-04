(* Every vector of the specification, from vectors_data.ml. *)

open Vectors_data

let words = Alcotest.(array int)
let domain_stream = 0x9e37_79b9
let aux_stream = 0x94d0_49bb
let rng () = Tandem.of_key ~chunk_length:k key

let step () =
  List.iter
    (fun (o, h, o_out, h_out) ->
      let o', h' = Tandem.Spec.step ~o ~h in
      Alcotest.check words "o" o_out o';
      Alcotest.check words "h" h_out h')
    t_vectors

let seeding_function () =
  List.iter
    (fun (counter, o, h) ->
      let o', h' =
        Tandem.Spec.f_keyed key ~counter:(Int64.of_int counter) ~domain:domain_stream ~aux:aux_stream
      in
      Alcotest.check words "o" o o';
      Alcotest.check words "h" h h')
    f_vectors

let stream_words () =
  let at i = Tandem.u32 (Tandem.seek (rng ()) (Int64.of_int (32 * i))) |> fst in
  List.iter
    (fun (first, ws) ->
      Array.iteri (fun i w -> Alcotest.(check int) "word" w (at (first + i))) ws;
      let row = 32 * first / 1024 and lane = 32 * first / 128 land 7 in
      Alcotest.check words "block" ws (Tandem.Spec.block key ~chunk:((8 * (row / k)) + lane) ~j:(row mod k)))
    stream;
  let a = Bigarray.(Array1.create int32 c_layout 64) in
  ignore (Tandem.fill_u32 (rng ()) a);
  List.iter
    (fun (first, ws) ->
      Array.iteri (fun i w -> Alcotest.(check int) "fill" w (Int32.to_int a.{first + i} land 0xffff_ffff)) ws)
    stream

let draws_from_position_0 () =
  let at_bits p = Tandem.seek (rng ()) (Int64.of_int p) in
  List.iter (fun (i, x) -> Alcotest.(check (float 0.)) "f64" x (fst (Tandem.float (at_bits (64 * i))))) f64;
  List.iter (fun (i, b) -> Alcotest.(check bool) "bool" b (fst (Tandem.bool (at_bits i)))) bool

let derived_keys () =
  let r = rng () in
  Alcotest.check words "split 0" split0 (Tandem.key (Tandem.split r 0));
  Alcotest.check words "split 1" split1 (Tandem.key (Tandem.split r 1));
  Alcotest.check words "purpose 7" purpose7 (Tandem.key (Tandem.purpose r 7));
  let parent, kids = Tandem.fork r 2 in
  Alcotest.check words "fork 0" fork0 (Tandem.key kids.(0));
  Alcotest.(check int64) "child position" 0L (Tandem.position kids.(0));
  Alcotest.(check int) "child chunk length" k (Tandem.chunk_length kids.(0));
  Alcotest.(check int64) "parent position" 128L (Tandem.position parent)

let seed_whitening () =
  let r = Tandem.seed ~chunk_length:k seed in
  Alcotest.check words "key" seed_key (Tandem.key r);
  Alcotest.(check bool) "default chunk length" true (Tandem.equal r (Tandem.seed seed));
  List.iter (fun (i, x) -> Alcotest.(check (float 0.)) "f64" x (fst (Tandem.float (Tandem.seek r (Int64.of_int (64 * i)))))) seed_f64;
  List.iter (fun (i, x) -> Alcotest.(check int) "u32" x (fst (Tandem.u32 (Tandem.seek r (Int64.of_int (32 * i)))))) seed_u32

let () =
  Alcotest.run "vectors"
    [
      ( "specification",
        [
          Alcotest.test_case "step T" `Quick step;
          Alcotest.test_case "seeding function F" `Quick seeding_function;
          Alcotest.test_case "stream words" `Quick stream_words;
          Alcotest.test_case "draws from position 0" `Quick draws_from_position_0;
          Alcotest.test_case "derived keys" `Quick derived_keys;
          Alcotest.test_case "seed whitening" `Quick seed_whitening;
        ] );
    ]
