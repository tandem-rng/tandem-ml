(* Writes the normal or exponential fills to stdout as raw little-endian bytes, as tandem-c's
   tools/dump_normals.c and tools/dump_exponentials.c do, for comparing ports byte for byte:
     dune exec tools/dump.exe -- normals | shasum -a 256 *)

module A1 = Bigarray.Array1

let () =
  set_binary_mode_out stdout true;
  let normals = match Sys.argv with [| _; "normals" |] -> true | [| _; "exponentials" |] -> false | _ -> prerr_endline "usage: dump (normals|exponentials)"; exit 2 in
  let count = if normals then 1_999_999 else 1_000_000 in
  let d = A1.create Bigarray.float64 Bigarray.c_layout count in
  let f = A1.create Bigarray.float32 Bigarray.c_layout count in
  let buf = Bytes.create (8 * count) in
  List.iter
    (fun from ->
      let g = Tandem.seek (Tandem.seed_u128 2026L 7L) (Int64.of_int from) in
      let g = if normals then Tandem.fill_normal g d else Tandem.fill_exponential g d in
      for i = 0 to count - 1 do
        Bytes.set_int64_le buf (8 * i) (Int64.bits_of_float d.{i})
      done;
      output_bytes stdout buf;
      ignore (if normals then Tandem.fill_normal32 g f else Tandem.fill_exponential32 g f);
      for i = 0 to count - 1 do
        Bytes.set_int32_le buf (4 * i) (Int32.bits_of_float f.{i})
      done;
      output stdout buf 0 (4 * count))
    [ 0; 1; 77; 12345; 1 lsl 30 ]
