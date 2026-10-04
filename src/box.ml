(* Normals and exponentials, with tandem-c's polynomials and operation order.

   Every multiply-add is [Float.fma], so the f64 results equal tandem-c bit for bit. OCaml has
   no single-precision type. The f32 functions hold single-precision values in doubles and
   round to single after every operation, through a one-element float32 bigarray that the
   compiler turns into a plain convert and store. A sum, product, quotient or root of two
   singles computed in double and rounded once is the correctly rounded single result. A fused
   multiply-add is rounded twice, to double and then to single, which differs from a single
   rounding only when the double result lands on a midpoint of two singles. *)

type scratch = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

let make_scratch () : scratch = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout 1

let[@inline] fma a b c = Float.fma a b c

(* -2 ln x for x in (0, 1]. *)
let[@inline] neg2_log x =
  let ix = Int64.add (Int64.bits_of_float x) 0x0009_5f62_0000_0000L in
  let nk = float_of_int (1023 - Int64.to_int (Int64.shift_right_logical ix 52)) in
  let ix = Int64.add (Int64.logand ix 0x000f_ffff_ffff_ffffL) 0x3fe6_a09e_0000_0000L in
  let mant = Int64.float_of_bits ix in
  let s = (mant -. 1.0) /. (mant +. 1.0) in
  let zz = s *. s in
  let p =
    fma zz
      (fma zz
         (fma zz
            (fma zz
               (fma zz (fma zz 0.08312363319426472 0.09070001083303751) 0.11111433317907482)
               0.14285712049336274)
            0.2000000000566491)
         0.33333333333331017)
      1.0
  in
  fma nk 3.816429394731813e-10 (fma nk 1.3862943607382476 (s *. -4.0 *. p))

(* In place. Uniforms [a, b] at [2j], [2j + 1] become the normals [cos, sin]. [sel] holds the
   two halves of the quarter-turn rotation, so that no branch depends on the data. *)
let normal_block_f64 (z : Float.Array.t) off m =
  let sel = Float.Array.create 2 in
  for j = 0 to m - 1 do
    let i = off + (2 * j) in
    let a = Float.Array.unsafe_get z i and b = Float.Array.unsafe_get z (i + 1) in
    let r = sqrt (neg2_log (1.0 -. a)) in
    let q = int_of_float ((b *. 4.0) +. 0.5) in
    let f = fma (-.float_of_int q) 0.25 b in
    let th = f *. 6.283185307179586 in
    let w = th *. th in
    let hs =
      fma w
        (fma w
           (fma w
              (fma w (fma w 1.5914650986900946e-10 (-2.5051097984389413e-08)) 2.755731600073921e-06)
              (-0.00019841269836630226))
           0.008333333333330813)
        (-0.16666666666666669)
    in
    let hc =
      fma w
        (fma w
           (fma w
              (fma w (fma w 2.0665708703855164e-09 (-2.7555858522576447e-07)) 2.480158263811954e-05)
              (-0.0013888888882156126))
           0.04166666666663108)
        (-0.4999999999999997)
    in
    let sn = th *. fma w hs 1.0 and cs = fma w hc 1.0 in
    (* Odd q swaps sine and cosine, bit 1 of q negates the sine, bit 1 of q + 1 negates the
       cosine. *)
    Float.Array.unsafe_set sel 0 cs;
    Float.Array.unsafe_set sel 1 sn;
    let x = Float.Array.unsafe_get sel (q land 1) and y = Float.Array.unsafe_get sel ((q land 1) lxor 1) in
    let x = x *. float_of_int (1 - ((q + 1) land 2)) and y = y *. float_of_int (1 - (q land 2)) in
    Float.Array.unsafe_set z i (r *. x);
    Float.Array.unsafe_set z (i + 1) (r *. y)
  done

(* In place, [-ln (1 - u)]. Halving [-2 ln] is exact. *)
let exponential_block_f64 (z : Float.Array.t) off m =
  for i = off to off + m - 1 do
    Float.Array.unsafe_set z i (0.5 *. neg2_log (1.0 -. Float.Array.unsafe_get z i))
  done

(* ---- Single precision ----------------------------------------------------------------- *)

let[@inline] r32 (sc : scratch) x =
  Bigarray.Array1.unsafe_set sc 0 x;
  Bigarray.Array1.unsafe_get sc 0

let[@inline] fma32 sc a b c = r32 sc (Float.fma a b c)

let single x = Int32.float_of_bits (Int32.bits_of_float x)

let c_ln_a = single 0.14275366
and c_ln_b = single 0.20000061
and c_ln_c = single 0.33333334
and c_ln2_lo = single 2.857213530660374e-06
and c_ln2_hi = single 1.38629150390625
and c_two_pi = single 6.2831855
and c_two_pi_lo = single (-1.7484555e-7)
and c_s_a = single 2.72499e-06
and c_s_b = single (-0.00019840087)
and c_s_c = single 0.008333332
and c_s_d = single (-0.16666667)
and c_c_a = single 2.4463761e-05
and c_c_b = single (-0.0013887589)
and c_c_c = single 0.04166665

let[@inline] neg2_log32 sc x =
  let bits = Int32.to_int (Int32.bits_of_float x) land 0xffff_ffff in
  let ix = (bits + 0x004a_fb0d) land 0xffff_ffff in
  let nk = float_of_int (127 - (ix lsr 23)) in
  let mant = Int32.float_of_bits (Int32.of_int ((ix land 0x007f_ffff) + 0x3f35_04f3)) in
  let s = r32 sc (r32 sc (mant -. 1.0) /. r32 sc (mant +. 1.0)) in
  let zz = r32 sc (s *. s) in
  let p = fma32 sc zz (fma32 sc zz (fma32 sc zz c_ln_a c_ln_b) c_ln_c) 1.0 in
  fma32 sc nk c_ln2_lo (fma32 sc nk c_ln2_hi (r32 sc (r32 sc (s *. -4.0) *. p)))

let normal_block_f32 sc (z : Float.Array.t) off m =
  let sel = Float.Array.create 2 in
  for j = 0 to m - 1 do
    let i = off + (2 * j) in
    let a = Float.Array.unsafe_get z i and b = Float.Array.unsafe_get z (i + 1) in
    let r = r32 sc (sqrt (neg2_log32 sc (1.0 -. a))) in
    let q = int_of_float (r32 sc ((b *. 4.0) +. 0.5)) in
    let f = fma32 sc (-.float_of_int q) 0.25 b in
    let th = fma32 sc f c_two_pi_lo (r32 sc (f *. c_two_pi)) in
    let w = r32 sc (th *. th) in
    let hs = fma32 sc w (fma32 sc w (fma32 sc w c_s_a c_s_b) c_s_c) c_s_d in
    let hc = fma32 sc w (fma32 sc w (fma32 sc w c_c_a c_c_b) c_c_c) (-0.5) in
    let sn = r32 sc (th *. fma32 sc w hs 1.0) and cs = fma32 sc w hc 1.0 in
    Float.Array.unsafe_set sel 0 cs;
    Float.Array.unsafe_set sel 1 sn;
    let x = Float.Array.unsafe_get sel (q land 1) and y = Float.Array.unsafe_get sel ((q land 1) lxor 1) in
    let x = x *. float_of_int (1 - ((q + 1) land 2)) and y = y *. float_of_int (1 - (q land 2)) in
    Float.Array.unsafe_set z i (r32 sc (r *. x));
    Float.Array.unsafe_set z (i + 1) (r32 sc (r *. y))
  done

let exponential_block_f32 sc (z : Float.Array.t) off m =
  for i = off to off + m - 1 do
    Float.Array.unsafe_set z i (0.5 *. neg2_log32 sc (1.0 -. Float.Array.unsafe_get z i))
  done
