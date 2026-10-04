(* Bounded integers, normals and exponentials agree bit for bit with tandem-c, which takes its
   fixtures from the shared device core. Fills agree with scalar draws, are cuttable at any
   element, and the C fills equal the pure OCaml fills. The fixed values come from
   derived_data.ml. *)

module A1 = Bigarray.Array1
open Derived_data

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
let to_ints a = Array.init (A1.dim a) (fun i -> Int32.to_int a.{i} land 0xffff_ffff)

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

let normal_fixture () =
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

let exponential_fixture () =
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

(* FNV-1a of the f64 fills of tandem-c's tools/dump_normals.c and tools/dump_exponentials.c,
   without their f32 parts, from the same build of tandem-c b049384 whose full dumps hash to
   0x9414e1315e2653be and 0x47f8f98297d94ee2. *)
let long_fill_hash ~count fill expected () =
  let d = f64_array count in
  let h = ref 0xcbf29ce484222325L in
  List.iter
    (fun from ->
      ignore (fill (Tandem.seek (Tandem.seed_u128 2026L 7L) (Int64.of_int from)) d);
      for i = 0 to count - 1 do
        let x = bits d.{i} in
        for b = 0 to 7 do
          let byte = Int64.logand (Int64.shift_right_logical x (8 * b)) 0xffL in
          h := Int64.mul (Int64.logxor !h byte) 0x100000001b3L
        done
      done)
    [ 0; 1; 77; 12345; 1 lsl 30 ];
  Alcotest.(check int64) "FNV-1a" expected !h

(* Moments to the fourth order within five standard errors, and the Kolmogorov-Smirnov
   distance below its 0.1 % critical value 1.95 / sqrt n. *)
let distribution name ~fill ~cdf ~moments () =
  let n = 10_000_000 in
  let a = Float.Array.create n in
  ignore (fill (Tandem.seed 2026) a);
  List.iteri
    (fun k (want, var) ->
      let k = k + 1 in
      let m = ref 0. in
      Float.Array.iter (fun x -> m := !m +. (x ** float_of_int k)) a;
      let got = !m /. float_of_int n and se = sqrt (var /. float_of_int n) in
      if Float.abs (got -. want) > 5. *. se then
        Alcotest.failf "%s: moment %d is %g, want %g within %g" name k got want (5. *. se))
    moments;
  Float.Array.sort Float.compare a;
  let d = ref 0. in
  Float.Array.iteri
    (fun i x ->
      let f = cdf x in
      d := Float.max !d (Float.max (float_of_int (i + 1) /. float_of_int n -. f) (f -. float_of_int i /. float_of_int n)))
    a;
  if !d > 1.95 /. sqrt (float_of_int n) then Alcotest.failf "%s: KS distance %g" name !d

(* Raw moments E[x^k] and the variances of x^k. *)
let normal_moments = [ (0., 1.); (1., 2.); (0., 15.); (3., 96.) ]
let exponential_moments = [ (1., 1.); (2., 20.); (6., 684.); (24., 39744.) ]
let normal_cdf x = 0.5 *. (1. +. Float.erf (x /. sqrt 2.))
let exponential_cdf x = -.Float.expm1 (-.x)

(* ---- Fills agree with draws and cut anywhere ---- *)

(* A fill of [n] elements equals the fill cut after [cut] elements, from an unaligned start. *)
let cut_fill name ~create ~fill ~get ~equal cases =
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
    cases

let eq_f x y = Int64.equal (bits x) (bits y)

let cut_fills () =
  let cases = [ (0, 700, 1); (5, 700, 333); (1001, 2500, 1024); (33, 3000, 2047) ] in
  cut_fill "u32" ~create:u32_array ~fill:(fun ?off ?len g a -> Tandem.fill_u32 ?off ?len g a) ~get:A1.get
    ~equal:Int32.equal cases;
  cut_fill "u64" ~create:u64_array ~fill:(fun ?off ?len g a -> Tandem.fill_u64 ?off ?len g a) ~get:A1.get
    ~equal:Int64.equal cases;
  cut_fill "float" ~create:f64_array ~fill:(fun ?off ?len g a -> Tandem.fill_float ?off ?len g a) ~get:A1.get
    ~equal:eq_f cases;
  cut_fill "exponential" ~create:f64_array
    ~fill:(fun ?off ?len g a -> Tandem.fill_exponential ?off ?len g a)
    ~get:A1.get ~equal:eq_f cases;
  (* 3 / 4 of the 32-bit range rejects a third of the draws. *)
  cut_fill "below32" ~create:u32_array
    ~fill:(fun ?off ?len g a -> Tandem.fill_below32 ?off ?len g ~range:3221225473 a)
    ~get:A1.get ~equal:Int32.equal cases;
  cut_fill "below64" ~create:u64_array
    ~fill:(fun ?off ?len g a -> Tandem.fill_below64 ?off ?len g ~range:(-4611686018427387903L) a)
    ~get:A1.get ~equal:Int64.equal cases;
  (* Normals pair up draws, so a cut falls on an even element. *)
  cut_fill "normal" ~create:f64_array
    ~fill:(fun ?off ?len g a -> Tandem.fill_normal ?off ?len g a)
    ~get:A1.get ~equal:eq_f
    [ (0, 701, 2); (5, 3001, 1024); (1001, 2500, 2046) ]

(* A normal or exponential fill is the flattened scalar sequence, across the block size of the
   fill, with an odd length that keeps the cosine half of the last pair. *)
let fills_are_draws () =
  let n = 2051 in
  let a = f64_array n in
  let g = Tandem.fill_normal (at 9) a in
  let s = ref (at 9) in
  for j = 0 to n / 2 do
    let c, z, s' = Tandem.normal2 !s in
    s := s';
    same_float "cos" c a.{2 * j};
    if 2 * j + 1 < n then same_float "sin" z a.{(2 * j) + 1}
  done;
  Alcotest.(check bool) "both uniforms of the last pair are consumed" true (Tandem.equal g !s);
  let e = f64_array n in
  let g = Tandem.fill_exponential (at 9) e in
  let s = ref (at 9) in
  for i = 0 to n - 1 do
    let x, s' = Tandem.exponential !s in
    s := s';
    same_float "exponential" x e.{i}
  done;
  Alcotest.(check bool) "end" true (Tandem.equal g !s)

(* A fill of one kind from a position is the scalar draws of that kind. *)
let uniform_fills_are_draws () =
  let n = 300 in
  let u = u32_array n and w = u64_array n and f = f64_array n and fa = Float.Array.create n in
  let ends =
    [ Tandem.fill_u32 (at 7) u; Tandem.fill_u64 (at 7) w; Tandem.fill_float (at 7) f; Tandem.Float_array.fill_float (at 7) fa ]
  in
  let gu = ref (at 7) and gw = ref (at 7) and gf = ref (at 7) in
  for i = 0 to n - 1 do
    let x, g = Tandem.u32 !gu in gu := g; Alcotest.(check int) "u32" x (Int32.to_int u.{i} land 0xffff_ffff);
    let x, g = Tandem.u64 !gw in gw := g; Alcotest.(check int64) "u64" x w.{i};
    let x, g = Tandem.float !gf in gf := g; same_float "float" x f.{i}; same_float "float array" x (Float.Array.get fa i)
  done;
  List.iter2 (fun a b -> Alcotest.(check bool) "end" true (Tandem.equal a b)) ends [ !gu; !gw; !gf; !gf ]

(* ---- The C fills equal the pure OCaml fills ---- *)

(* Cases (start, off, len) at unaligned starts, across rows (1024 bits) and groups (K = 32 rows),
   on an array with room around the region. Before the C fill, a scalar draw at [warm] puts
   the shared row cache in the same row, an earlier row of the group, or another group, so the
   C fill reads the cache, steps it forward or reseeds. After it, a scalar draw from the
   returned generator checks the cache that the C fill leaves. *)
let c_equals_pure () =
  let cases = [ (0, 0, 0); (3, 1, 1); (37, 2, 31); (1000, 0, 33); (5000, 5, 1025); (40000, 0, 4097); (1 lsl 40, 3, 3000) ] in
  let warms start = [ start; max 0 (start - 2048); start + (1 lsl 16) ] in
  let check name create get equal c pure =
    List.iter
      (fun (start, off, len) ->
        List.iter
          (fun warm ->
            let g = at start in
            ignore (Tandem.u32 (Tandem.seek g (Int64.of_int warm)));
            let a = create (off + len + 2) and b = create (off + len + 2) in
            let gc = c ~off ~len g a and gp = pure ~off ~len (at start) b in
            for i = 0 to off + len + 1 do
              if not (equal (get a i) (get b i)) then
                Alcotest.failf "%s: start %d, len %d, warm %d: differs at %d" name start len warm i
            done;
            Alcotest.(check bool) (name ^ ": end") true (Tandem.equal gc gp);
            let fresh = Tandem.seek (Tandem.seed 42) (Tandem.position gc) in
            Alcotest.(check int) (name ^ ": next draw") (fst (Tandem.u32 fresh)) (fst (Tandem.u32 gc)))
          (warms start))
      cases
  in
  let zero create fill n = let a = create n in fill a; a in
  let u32 n = zero u32_array (fun a -> A1.fill a 0l) n and u64 n = zero u64_array (fun a -> A1.fill a 0L) n in
  let f64 n = zero f64_array (fun a -> A1.fill a 0.) n and fa n = Float.Array.make n 0. in
  check "u32" u32 A1.get Int32.equal
    (fun ~off ~len -> Tandem.fill_u32 ~off ~len) (fun ~off ~len -> Tandem.Pure.fill_u32 ~off ~len);
  check "u64" u64 A1.get Int64.equal
    (fun ~off ~len -> Tandem.fill_u64 ~off ~len) (fun ~off ~len -> Tandem.Pure.fill_u64 ~off ~len);
  check "float" f64 A1.get eq_f
    (fun ~off ~len -> Tandem.fill_float ~off ~len) (fun ~off ~len -> Tandem.Pure.fill_float ~off ~len);
  check "normal" f64 A1.get eq_f
    (fun ~off ~len -> Tandem.fill_normal ~off ~len) (fun ~off ~len -> Tandem.Pure.fill_normal ~off ~len);
  check "exponential" f64 A1.get eq_f
    (fun ~off ~len -> Tandem.fill_exponential ~off ~len) (fun ~off ~len -> Tandem.Pure.fill_exponential ~off ~len);
  List.iter
    (fun range ->
      check (Printf.sprintf "below32 %d" range) u32 A1.get Int32.equal
        (fun ~off ~len -> Tandem.fill_below32 ~off ~len ~range)
        (fun ~off ~len -> Tandem.Pure.fill_below32 ~off ~len ~range))
    [ 0; 1000; 3221225473; 1 lsl 32 ];
  List.iter
    (fun range ->
      check (Printf.sprintf "below64 %Ld" range) u64 A1.get Int64.equal
        (fun ~off ~len -> Tandem.fill_below64 ~off ~len ~range)
        (fun ~off ~len -> Tandem.Pure.fill_below64 ~off ~len ~range))
    [ 0L; 1000L; -4611686018427387903L ];
  check "Float.Array float" fa Float.Array.get eq_f
    (fun ~off ~len -> Tandem.Float_array.fill_float ~off ~len)
    (fun ~off ~len -> Tandem.Pure.Float_array.fill_float ~off ~len);
  check "Float.Array normal" fa Float.Array.get eq_f
    (fun ~off ~len -> Tandem.Float_array.fill_normal ~off ~len)
    (fun ~off ~len -> Tandem.Pure.Float_array.fill_normal ~off ~len);
  check "Float.Array exponential" fa Float.Array.get eq_f
    (fun ~off ~len -> Tandem.Float_array.fill_exponential ~off ~len)
    (fun ~off ~len -> Tandem.Pure.Float_array.fill_exponential ~off ~len)

(* ---- Positions ---- *)

let empty_fills () =
  List.iter
    (fun from ->
      let g = at from in
      let moved g' = not (Tandem.equal g g') in
      Alcotest.(check bool) "bounded 32" false (moved (Tandem.fill_below32 g ~range:10 (u32_array 0)));
      Alcotest.(check bool) "bounded 64" false (moved (Tandem.fill_below64 g ~range:10L (u64_array 0)));
      Alcotest.(check bool) "normal" false (moved (Tandem.fill_normal g (f64_array 0)));
      Alcotest.(check bool) "exponential" false (moved (Tandem.fill_exponential g (f64_array 0)));
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
  ignore (Tandem.State.split s);
  Alcotest.(check bool) "split moves the parent to the next block" true
    (Int64.rem (Tandem.position (Tandem.State.generator s)) 128L = 0L)

(* Further integers of [make] select split children, as [Random.State.make] takes any array. *)
let state_make () =
  let key s = Tandem.key (Tandem.State.generator s) in
  let words = Alcotest.(array int) in
  Alcotest.check words "one integer" (Tandem.key (Tandem.seed 7)) (key (Tandem.State.make [| 7 |]));
  Alcotest.check words "empty" (Tandem.key (Tandem.seed 0)) (key (Tandem.State.make [||]));
  Alcotest.check words "three integers" (Tandem.key (Tandem.split (Tandem.split (Tandem.seed 7) 3) 9))
    (key (Tandem.State.make [| 7; 3; 9 |]));
  Alcotest.check words "negative" (Tandem.key (Tandem.seed_u128 (-1L) 0L)) (key (Tandem.State.make [| -1 |]))

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
          Alcotest.test_case "normals match tandem-c" `Quick normal_fixture;
          Alcotest.test_case "exponentials match tandem-c" `Quick exponential_fixture;
          Alcotest.test_case "normal bits equal tandem-c" `Slow
            (long_fill_hash ~count:1999999 (fun g a -> Tandem.fill_normal g a) 0x8ea806b43afe58edL);
          Alcotest.test_case "pure normal bits equal tandem-c" `Slow
            (long_fill_hash ~count:1999999 (fun g a -> Tandem.Pure.fill_normal g a) 0x8ea806b43afe58edL);
          Alcotest.test_case "exponential bits equal tandem-c" `Slow
            (long_fill_hash ~count:1000000 (fun g a -> Tandem.fill_exponential g a) 0x8cb6728a73181814L);
          Alcotest.test_case "pure exponential bits equal tandem-c" `Slow
            (long_fill_hash ~count:1000000 (fun g a -> Tandem.Pure.fill_exponential g a) 0x8cb6728a73181814L);
          Alcotest.test_case "normals are N(0, 1)" `Slow
            (distribution "normal" ~fill:(fun g a -> Tandem.Float_array.fill_normal g a) ~cdf:normal_cdf
               ~moments:normal_moments);
          Alcotest.test_case "exponentials are Exp(1)" `Slow
            (distribution "exponential" ~fill:(fun g a -> Tandem.Float_array.fill_exponential g a)
               ~cdf:exponential_cdf ~moments:exponential_moments);
        ] );
      ( "fills",
        [
          Alcotest.test_case "uniform fills equal scalar draws" `Quick uniform_fills_are_draws;
          Alcotest.test_case "normal and exponential fills equal scalar draws" `Quick fills_are_draws;
          Alcotest.test_case "a fill cut at any element equals the whole" `Quick cut_fills;
          Alcotest.test_case "C fills equal pure fills" `Quick c_equals_pure;
          Alcotest.test_case "empty fills" `Quick empty_fills;
          Alcotest.test_case "positions carry across 2^63" `Quick seek_bounds;
        ] );
      ( "state",
        [
          Alcotest.test_case "the wrapper follows the value-type draws" `Quick state_wrapper;
          Alcotest.test_case "make takes any int array" `Quick state_make;
        ] );
    ]
