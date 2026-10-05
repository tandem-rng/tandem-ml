(* The reference logarithm of Appendix A and the exponentials on it, with tandem-c's operation
   order. Every multiply-add is [Float.fma], so the results equal tandem-c bit for bit. *)

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

let[@inline] exponential u = 0.5 *. neg2_log (1.0 -. u)

let exponential_block (z : Float.Array.t) off m =
  for i = off to off + m - 1 do
    Float.Array.unsafe_set z i (exponential (Float.Array.unsafe_get z i))
  done
