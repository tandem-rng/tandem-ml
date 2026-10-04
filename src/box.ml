(* Normals and exponentials with tandem-c's polynomials and operation order. Every multiply-add
   is [Float.fma], so the results equal tandem-c bit for bit. *)

let fma = Float.fma

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

(* The normals [(cos, sin)] of the uniforms [a] and [b]. *)
let[@inline] pair a b =
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
  (* Rotate by q quarter turns: odd q swaps sine and cosine, bit 1 of q negates the sine, bit 1
     of q + 1 negates the cosine. *)
  let x = if q land 1 = 0 then cs else sn and y = if q land 1 = 0 then sn else cs in
  let x = if (q + 1) land 2 = 0 then x else -.x and y = if q land 2 = 0 then y else -.y in
  (r *. x, r *. y)

let[@inline] exponential u = 0.5 *. neg2_log (1.0 -. u)

(* In place: the uniforms at [off + 2j] and [off + 2j + 1] become a pair of normals. *)
let normal_block (z : Float.Array.t) off m =
  for j = 0 to m - 1 do
    let i = off + (2 * j) in
    let x, y = pair (Float.Array.unsafe_get z i) (Float.Array.unsafe_get z (i + 1)) in
    Float.Array.unsafe_set z i x;
    Float.Array.unsafe_set z (i + 1) y
  done

let exponential_block (z : Float.Array.t) off m =
  for i = off to off + m - 1 do
    Float.Array.unsafe_set z i (exponential (Float.Array.unsafe_get z i))
  done
