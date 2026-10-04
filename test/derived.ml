(* Bounded integers, normals and exponentials agree bit for bit with tandem-c, which takes its
   fixtures from the shared device core. Fills agree with scalar draws and are cuttable at any
   element. The fixed values come from derived_data.ml. *)

module A1 = Bigarray.Array1
open Derived_data

let single x = Int32.float_of_bits (Int32.bits_of_float x)
let bits = Int64.bits_of_float
let same_float msg want got = Alcotest.(check int64) msg (bits want) (bits got)

(* The bounded and normal fixtures start after one bit draw, which leaves the position
   unaligned. *)
let start () = snd (Tandem.bool (Tandem.seed 42))
let at p = Tandem.seek (Tandem.seed 42) (Int64.of_int p)
let pos = Alcotest.int64

let u32_array n = A1.create Bigarray.int32 Bigarray.c_layout n
let u64_array n = A1.create Bigarray.int64 Bigarray.c_layout n
let f64_array n = A1.create Bigarray.float64 Bigarray.c_layout n
let f32_array n = A1.create Bigarray.float32 Bigarray.c_layout n
let to_ints a = Array.init (A1.dim a) (fun i -> Int32.to_int a.{i} land 0xffff_ffff)
let to_floats a = Array.init (A1.dim a) (fun i -> a.{i})

(* ---- Bounded integers ---- *)

let below_scalar () =
  List.iter
    (fun (range, want, end_pos) ->
      let g = ref (start ()) in
      let got = Array.map (fun _ -> let x, g' = Tandem.below32 !g range in g := g'; x) want in
      Alcotest.(check (array int)) (Printf.sprintf "below32 %d" range) want got;
      Alcotest.check pos "end" (Int64.of_int end_pos) (Tandem.position !g))
    below32;
  List.iter
    (fun (range, want, end_pos) ->
      let g = ref (start ()) in
      let got = Array.map (fun _ -> let x, g' = Tandem.below64 !g range in g := g'; x) want in
      Alcotest.(check (array int64)) (Printf.sprintf "below64 %Lu" range) want got;
      Alcotest.check pos "end" (Int64.of_int end_pos) (Tandem.position !g))
    below64

let below_fills () =
  List.iter
    (fun (from, range, want, end_pos) ->
      let a = u32_array (Array.length want) in
      let g = Tandem.fill_below32 (at from) ~range a in
      Alcotest.(check (array int)) (Printf.sprintf "fill_below32 %d at %d" range from) want (to_ints a);
      Alcotest.check pos "end" (Int64.of_int end_pos) (Tandem.position g))
    fill_below32;
  List.iter
    (fun (from, range, want, end_pos) ->
      let a = u64_array (Array.length want) in
      let g = Tandem.fill_below64 (at from) ~range a in
      Alcotest.(check (array int64)) (Printf.sprintf "fill_below64 %Lu at %d" range from) want
        (Array.init (A1.dim a) (fun i -> a.{i}));
      Alcotest.check pos "end" (Int64.of_int end_pos) (Tandem.position g))
    fill_below64

let bound_zero () =
  let a = Tandem.seed 3 in
  let x, a' = Tandem.below32 a 0 and _, b = Tandem.u32 a in
  Alcotest.(check int) "below32 0" 0 x;
  Alcotest.(check bool) "one draw" true (Tandem.equal a' b);
  let x, a' = Tandem.below64 a 0L and _, b = Tandem.u64 a in
  Alcotest.(check int64) "below64 0" 0L x;
  Alcotest.(check bool) "one draw" true (Tandem.equal a' b)

let width_from_range () =
  let g = Tandem.seed 5 in
  Alcotest.(check bool) "2^32 on 32-bit draws" true
    (let x, g' = Tandem.below g (1 lsl 32) and y, g'' = Tandem.below32 g (1 lsl 32) in
     x = y && Tandem.equal g' g'');
  Alcotest.(check bool) "above 2^32 on 64-bit draws" true
    (let r = (1 lsl 32) + 1 in
     let x, g' = Tandem.below g r and y, g'' = Tandem.below64 g (Int64.of_int r) in
     Int64.of_int x = y && Tandem.equal g' g'');
  let x, _ = Tandem.between g ~lo:(-5) ~hi:5 and y, _ = Tandem.below32 g 10 in
  Alcotest.(check int) "interval adds the low end" (y - 5) x

(* ---- Normals and exponentials ---- *)

let normal_f64 () =
  let want, end_pos = normal64 in
  let n = Array.length want in
  let g = ref (start ()) in
  let got = Array.make n 0. in
  for j = 0 to (n / 2) - 1 do
    let c, s, g' = Tandem.normal2 !g in
    g := g';
    got.(2 * j) <- c;
    got.((2 * j) + 1) <- s
  done;
  Array.iteri (fun i w -> same_float (Printf.sprintf "normal2 %d" i) w got.(i)) want;
  Alcotest.check pos "end" (Int64.of_int end_pos) (Tandem.position !g);
  let a = f64_array n and fa = Float.Array.create n in
  let g1 = Tandem.fill_normal (start ()) a and g2 = Tandem.Float_array.fill_normal (start ()) fa in
  Array.iteri (fun i w -> same_float (Printf.sprintf "fill %d" i) w a.{i}) want;
  Array.iteri (fun i w -> same_float (Printf.sprintf "float array fill %d" i) w (Float.Array.get fa i)) want;
  Alcotest.check pos "fill end" (Int64.of_int end_pos) (Tandem.position g1);
  Alcotest.check pos "float array fill end" (Int64.of_int end_pos) (Tandem.position g2);
  let c, _ = Tandem.normal (start ()) in
  same_float "normal is element 0" want.(0) c

let normal_f32 () =
  let want, end_pos = normal32 in
  let want = Array.map single want in
  let n = Array.length want in
  let g = ref (start ()) in
  let got = Array.make n 0. in
  for j = 0 to (n / 2) - 1 do
    let c, s, g' = Tandem.normal2_f32 !g in
    g := g';
    got.(2 * j) <- c;
    got.((2 * j) + 1) <- s
  done;
  Array.iteri (fun i w -> same_float (Printf.sprintf "normal2_f32 %d" i) w got.(i)) want;
  Alcotest.check pos "end" (Int64.of_int end_pos) (Tandem.position !g);
  let a = f32_array n and fa = Float.Array.create n in
  let g1 = Tandem.fill_normal32 (start ()) a and g2 = Tandem.Float_array.fill_normal32 (start ()) fa in
  Array.iteri (fun i w -> same_float (Printf.sprintf "fill %d" i) w a.{i}) want;
  Array.iteri (fun i w -> same_float (Printf.sprintf "float array fill %d" i) w (Float.Array.get fa i)) want;
  Alcotest.check pos "fill end" (Int64.of_int end_pos) (Tandem.position g1);
  Alcotest.check pos "float array fill end" (Int64.of_int end_pos) (Tandem.position g2);
  let c, _ = Tandem.normal_f32 (start ()) in
  same_float "normal_f32 is element 0" want.(0) c

let exponential_f64 () =
  List.iter
    (fun (from, want, end_pos) ->
      let n = Array.length want in
      let a = f64_array n and fa = Float.Array.create n in
      let g1 = Tandem.fill_exponential (at from) a in
      let g2 = Tandem.Float_array.fill_exponential (at from) fa in
      let g = ref (at from) in
      Array.iteri
        (fun i w ->
          same_float (Printf.sprintf "fill %d at %d" i from) w a.{i};
          same_float (Printf.sprintf "float array fill %d at %d" i from) w (Float.Array.get fa i);
          let e, g' = Tandem.exponential !g in
          g := g';
          same_float (Printf.sprintf "draw %d at %d" i from) w e)
        want;
      List.iter (fun g -> Alcotest.check pos "end" (Int64.of_int end_pos) (Tandem.position g)) [ g1; g2; !g ])
    exponential64

let exponential_f32 () =
  List.iter
    (fun (from, want, end_pos) ->
      let n = Array.length want in
      let a = f32_array n and fa = Float.Array.create n in
      let g1 = Tandem.fill_exponential32 (at from) a in
      let g2 = Tandem.Float_array.fill_exponential32 (at from) fa in
      let g = ref (at from) in
      Array.iteri
        (fun i w ->
          let w = single w in
          same_float (Printf.sprintf "fill %d at %d" i from) w a.{i};
          same_float (Printf.sprintf "float array fill %d at %d" i from) w (Float.Array.get fa i);
          let e, g' = Tandem.exponential_f32 !g in
          g := g';
          same_float (Printf.sprintf "draw %d at %d" i from) w e)
        want;
      List.iter (fun g -> Alcotest.check pos "end" (Int64.of_int end_pos) (Tandem.position g)) [ g1; g2; !g ])
    exponential32

(* The long fills must have the bits of the M4 clang build of tandem-c: the hashes below are
   the ones its tests/test_normal_bits.c and tests/test_exponential_bits.c record. *)
let fnv h x =
  Int64.mul (Int64.logxor h (Int64.of_int x)) 0x100000001b3L

let fnv_bytes h f n =
  let h = ref h in
  for i = 0 to n - 1 do
    h := fnv !h (f i)
  done;
  !h

let hash_f64 h a n =
  fnv_bytes h
    (fun i -> Int64.to_int (Int64.shift_right_logical (bits a.{i / 8}) (8 * (i land 7))) land 0xff)
    (8 * n)

let hash_f32 h a n =
  fnv_bytes h
    (fun i -> Int32.to_int (Int32.shift_right_logical (Int32.bits_of_float a.{i / 4}) (8 * (i land 3))) land 0xff)
    (4 * n)

let long_fill_hash ~count ~fill64 ~fill32 expected () =
  let starts = [ 0; 1; 77; 12345; 1 lsl 30 ] in
  let d = f64_array count and f = f32_array count in
  let h = ref 0xcbf29ce484222325L in
  List.iter
    (fun from ->
      let g = fill64 (Tandem.seek (Tandem.seed_u128 2026L 7L) (Int64.of_int from)) d in
      h := hash_f64 !h d count;
      ignore (fill32 g f);
      h := hash_f32 !h f count)
    starts;
  Alcotest.(check int64) "FNV-1a" expected !h

(* ---- Fills agree with draws and cut anywhere ---- *)

(* A fill of [n] elements equals the fill cut after [cut] elements, from an unaligned start. *)
let cut_fill name ~create ~fill ~get ~equal =
  List.iter
    (fun (from, n, cut) ->
      let g = at from in
      let whole = create n in
      let end_whole = fill ?off:None ?len:None g whole in
      let parts = create n in
      let mid = fill ?off:None ?len:(Some cut) g parts in
      let end_parts = fill ?off:(Some cut) ?len:(Some (n - cut)) mid parts in
      for i = 0 to n - 1 do
        if not (equal (get whole i) (get parts i)) then Alcotest.failf "%s: cut %d differs at %d" name cut i
      done;
      Alcotest.(check bool) (name ^ ": end") true (Tandem.equal end_whole end_parts))
    [ (0, 700, 1); (5, 700, 333); (1001, 2500, 1024); (33, 3000, 2047) ]

let cut_fills () =
  let eq_int = Int32.equal and eq_i64 = Int64.equal in
  let eq_f x y = Int64.equal (bits x) (bits y) in
  cut_fill "u32" ~create:u32_array ~fill:(fun ?off ?len g a -> Tandem.fill_u32 ?off ?len g a) ~get:A1.get ~equal:eq_int;
  cut_fill "u64" ~create:u64_array ~fill:(fun ?off ?len g a -> Tandem.fill_u64 ?off ?len g a) ~get:A1.get ~equal:eq_i64;
  cut_fill "float" ~create:f64_array ~fill:(fun ?off ?len g a -> Tandem.fill_float ?off ?len g a) ~get:A1.get ~equal:eq_f;
  cut_fill "float32" ~create:f32_array ~fill:(fun ?off ?len g a -> Tandem.fill_float32 ?off ?len g a) ~get:A1.get ~equal:eq_f;
  cut_fill "exponential" ~create:f64_array
    ~fill:(fun ?off ?len g a -> Tandem.fill_exponential ?off ?len g a)
    ~get:A1.get ~equal:eq_f;
  cut_fill "exponential32" ~create:f32_array
    ~fill:(fun ?off ?len g a -> Tandem.fill_exponential32 ?off ?len g a)
    ~get:A1.get ~equal:eq_f;
  (* 3 / 4 of the 32-bit range rejects a third of the draws. *)
  cut_fill "below32" ~create:u32_array
    ~fill:(fun ?off ?len g a -> Tandem.fill_below32 ?off ?len g ~range:3221225473 a)
    ~get:A1.get ~equal:eq_int;
  cut_fill "below64" ~create:u64_array
    ~fill:(fun ?off ?len g a -> Tandem.fill_below64 ?off ?len g ~range:(-4611686018427387903L) a)
    ~get:A1.get ~equal:eq_i64

(* Normals pair up draws, so a cut falls on an even element. *)
let cut_normal_fills () =
  let eq_f x y = Int64.equal (bits x) (bits y) in
  List.iter
    (fun (from, n, cut) ->
      let check name create fill get =
        let g = at from in
        let whole = create n and parts = create n in
        let end_whole = fill ?off:None ?len:None g whole in
        let mid = fill ?off:None ?len:(Some cut) g parts in
        let end_parts = fill ?off:(Some cut) ?len:(Some (n - cut)) mid parts in
        for i = 0 to n - 1 do
          if not (eq_f (get whole i) (get parts i)) then Alcotest.failf "%s: cut %d differs at %d" name cut i
        done;
        Alcotest.(check bool) (name ^ ": end") true (Tandem.equal end_whole end_parts)
      in
      check "normal" f64_array (fun ?off ?len g a -> Tandem.fill_normal ?off ?len g a) A1.get;
      check "normal32" f32_array (fun ?off ?len g a -> Tandem.fill_normal32 ?off ?len g a) A1.get)
    [ (0, 701, 2); (5, 3001, 1024); (1001, 2500, 2046) ]

(* A normal or exponential fill is the flattened scalar sequence, across the block size of the
   fill, with an odd length that keeps the cosine half of the last pair. *)
let fills_are_draws () =
  let n = 2051 in
  let a = f64_array n and f = f32_array n in
  let g = Tandem.fill_normal (at 9) a and g32 = Tandem.fill_normal32 (at 9) f in
  let s = ref (at 9) and s32 = ref (at 9) in
  for j = 0 to n / 2 do
    let c, z, s' = Tandem.normal2 !s and c32, z32, s32' = Tandem.normal2_f32 !s32 in
    s := s';
    s32 := s32';
    same_float "cos" c a.{2 * j};
    same_float "cos f32" c32 f.{2 * j};
    if 2 * j + 1 < n then begin
      same_float "sin" z a.{(2 * j) + 1};
      same_float "sin f32" z32 f.{(2 * j) + 1}
    end
  done;
  Alcotest.(check bool) "both uniforms of the last pair are consumed" true
    (Tandem.equal g !s && Tandem.equal g32 !s32);
  let e = f64_array n in
  let g = Tandem.fill_exponential (at 9) e in
  let s = ref (at 9) in
  for i = 0 to n - 1 do
    let x, s' = Tandem.exponential !s in
    s := s';
    same_float "exponential" x e.{i}
  done;
  Alcotest.(check bool) "end" true (Tandem.equal g !s)

let float_array_equals_bigarray () =
  let n = 1500 in
  let g = at 3 in
  let check name bigarray floats =
    for i = 0 to n - 1 do
      same_float (Printf.sprintf "%s %d" name i) bigarray.(i) (Float.Array.get floats i)
    done
  in
  let fa = Float.Array.create n in
  let a = f64_array n in
  ignore (Tandem.fill_float g a);
  ignore (Tandem.Float_array.fill_float g fa);
  check "float" (to_floats a) fa;
  let a = f32_array n in
  ignore (Tandem.fill_float32 g a);
  ignore (Tandem.Float_array.fill_float32 g fa);
  check "float32" (to_floats a) fa;
  let a = f64_array n in
  ignore (Tandem.fill_normal g a);
  ignore (Tandem.Float_array.fill_normal g fa);
  check "normal" (to_floats a) fa;
  let a = f32_array n in
  ignore (Tandem.fill_normal32 g a);
  ignore (Tandem.Float_array.fill_normal32 g fa);
  check "normal32" (to_floats a) fa;
  let a = f64_array n in
  ignore (Tandem.fill_exponential g a);
  ignore (Tandem.Float_array.fill_exponential g fa);
  check "exponential" (to_floats a) fa;
  let a = f32_array n in
  ignore (Tandem.fill_exponential32 g a);
  ignore (Tandem.Float_array.fill_exponential32 g fa);
  check "exponential32" (to_floats a) fa

(* A fill of one kind from a position is the scalar draws of that kind. *)
let uniform_fills_are_draws () =
  let n = 300 in
  let u = u32_array n and w = u64_array n and f = f64_array n and f32 = f32_array n in
  let ends =
    [ Tandem.fill_u32 (at 7) u; Tandem.fill_u64 (at 7) w; Tandem.fill_float (at 7) f; Tandem.fill_float32 (at 7) f32 ]
  in
  let gu = ref (at 7) and gw = ref (at 7) and gf = ref (at 7) and g32 = ref (at 7) in
  for i = 0 to n - 1 do
    let x, g = Tandem.u32 !gu in gu := g; Alcotest.(check int) "u32" x (Int32.to_int u.{i} land 0xffff_ffff);
    let x, g = Tandem.u64 !gw in gw := g; Alcotest.(check int64) "u64" x w.{i};
    let x, g = Tandem.float !gf in gf := g; same_float "float" x f.{i};
    let x, g = Tandem.float32 !g32 in g32 := g; same_float "float32" x f32.{i}
  done;
  List.iter2 (fun a b -> Alcotest.(check bool) "end" true (Tandem.equal a b)) ends [ !gu; !gw; !gf; !g32 ]

(* ---- Positions ---- *)

let empty_fills () =
  List.iter
    (fun from ->
      let g = at from in
      let moved g' = not (Tandem.equal g g') in
      Alcotest.(check bool) "bounded 32" false (moved (Tandem.fill_below32 g ~range:10 (u32_array 0)));
      Alcotest.(check bool) "bounded 64" false (moved (Tandem.fill_below64 g ~range:10L (u64_array 0)));
      Alcotest.(check bool) "normal" false (moved (Tandem.fill_normal g (f64_array 0)));
      Alcotest.(check bool) "normal32" false (moved (Tandem.fill_normal32 g (f32_array 0)));
      Alcotest.(check bool) "exponential" false (moved (Tandem.fill_exponential g (f64_array 0)));
      Alcotest.(check bool) "exponential32" false (moved (Tandem.fill_exponential32 g (f32_array 0)));
      Alcotest.(check bool) "float array normal" false (moved (Tandem.Float_array.fill_normal g (Float.Array.create 0)));
      (* A plain fill aligns the position, as the specification says. *)
      Alcotest.check pos "u32 aligns" (Int64.of_int ((from + 31) land lnot 31)) (Tandem.position (Tandem.fill_u32 g (u32_array 0)));
      Alcotest.check pos "f64 aligns" (Int64.of_int ((from + 63) land lnot 63)) (Tandem.position (Tandem.fill_float g (f64_array 0))))
    [ 0; 1; 5; 33; 65; 1001 ]

let seek_bounds () =
  let g = Tandem.seed 1 in
  let top = Tandem.seek g 0x7fff_ffff_ffff_ffffL in
  Alcotest.check pos "position round trip" 0x7fff_ffff_ffff_ffffL (Tandem.position top);
  Alcotest.check_raises "seek to 2^63" (Invalid_argument "Tandem: a position is below 2^63") (fun () ->
      ignore (Tandem.seek g Int64.min_int));
  (* The last aligned 32-bit draw below 2^63 still works and moves to 2^63. *)
  let _, g' = Tandem.u32 (Tandem.seek g 0x7fff_ffff_ffff_ffe0L) in
  Alcotest.check pos "carry into the high limb" Int64.min_int (Tandem.position g')

(* ---- The stateful wrapper ---- *)

let state_wrapper () =
  let s = Tandem.State.make [| 42 |] and g = ref (Tandem.seed 42) in
  let next f = let x, g' = f !g in g := g'; x in
  Alcotest.(check int64) "bits64" (next Tandem.u64) (Tandem.State.bits64 s);
  Alcotest.(check int) "int" (next (fun g -> Tandem.below32 g 1000)) (Tandem.State.int s 1000);
  same_float "float" (3.5 *. next Tandem.float) (Tandem.State.float s 3.5);
  same_float "normal" (next Tandem.normal) (Tandem.State.normal s);
  same_float "exponential" (next Tandem.exponential) (Tandem.State.exponential s);
  Alcotest.(check bool) "bool" (next Tandem.bool) (Tandem.State.bool s);
  let c = Tandem.State.copy s in
  Alcotest.(check int64) "a copy continues alike" (Tandem.State.bits64 s) (Tandem.State.bits64 c);
  let child = Tandem.State.split s in
  Alcotest.(check bool) "split moves the parent to the next block" true
    (Int64.rem (Tandem.position (Tandem.State.generator s)) 128L = 0L);
  ignore child

let () =
  Alcotest.run "derived"
    [
      ( "bounded",
        [
          Alcotest.test_case "scalar draws match the device core" `Quick below_scalar;
          Alcotest.test_case "fills match the device core at unaligned starts" `Quick below_fills;
          Alcotest.test_case "range 0 returns 0 after one draw" `Quick bound_zero;
          Alcotest.test_case "the width follows the range" `Quick width_from_range;
        ] );
      ( "normals and exponentials",
        [
          Alcotest.test_case "f64 normals are exact" `Quick normal_f64;
          Alcotest.test_case "f32 normals are exact" `Quick normal_f32;
          Alcotest.test_case "f64 exponentials are exact" `Quick exponential_f64;
          Alcotest.test_case "f32 exponentials are exact" `Quick exponential_f32;
          Alcotest.test_case "normal bits equal tandem-c" `Slow
            (long_fill_hash ~count:1999999
               ~fill64:(fun g a -> Tandem.fill_normal g a)
               ~fill32:(fun g a -> Tandem.fill_normal32 g a)
               0x9414e1315e2653beL);
          Alcotest.test_case "exponential bits equal tandem-c" `Slow
            (long_fill_hash ~count:1000000
               ~fill64:(fun g a -> Tandem.fill_exponential g a)
               ~fill32:(fun g a -> Tandem.fill_exponential32 g a)
               0x47f8f98297d94ee2L);
        ] );
      ( "fills",
        [
          Alcotest.test_case "uniform fills equal scalar draws" `Quick uniform_fills_are_draws;
          Alcotest.test_case "normal and exponential fills equal scalar draws" `Quick fills_are_draws;
          Alcotest.test_case "a fill cut at any element equals the whole" `Quick cut_fills;
          Alcotest.test_case "a normal fill cut at an even element equals the whole" `Quick cut_normal_fills;
          Alcotest.test_case "Float.Array fills equal bigarray fills" `Quick float_array_equals_bigarray;
          Alcotest.test_case "empty fills" `Quick empty_fills;
          Alcotest.test_case "positions carry across 2^63" `Quick seek_bounds;
        ] );
      ( "state",
        [ Alcotest.test_case "the wrapper follows the value-type draws" `Quick state_wrapper ] );
    ]
