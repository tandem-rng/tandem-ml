(* The step T, the seeding function F and the row cache.

   A word is a 32-bit value held in a native int. The one product that needs 64 bits goes
   through an unboxed [Int64]. A row is the eight chunks of a group at one step. The cache
   keeps them word-major: [st.(w * 8 + l)] is exposed word [w] of lane [l], and
   [st.(32 + w * 8 + l)] is hidden word [w]. *)

let mask = 0xffff_ffff
let weyl = 0x9e37_79b9
let domain_stream = 0x9e37_79b9
let domain_split = 0xbb67_ae85
let domain_fork = 0xd251_1f53
let domain_fold = 0xcd9e_8d57
let domain_seed = 0xa54f_f53a
let aux_stream = 0x94d0_49bb

let rc =
  [| 0xd17c_c1b7; 0xa722_0a94; 0xfe13_abe8; 0xfa9a_6ee0;
     0xedb1_4acc; 0x9e21_c820; 0xff28_b1d5; 0xef5d_e2b0 |]

let[@inline] rotl x r = ((x lsl r) lor (x lsr (32 - r))) land mask

(* T on lane [l], in place. *)
let[@inline] step st l =
  let o0 = Array.unsafe_get st l
  and o1 = Array.unsafe_get st (8 + l)
  and o2 = Array.unsafe_get st (16 + l)
  and o3 = Array.unsafe_get st (24 + l)
  and h0 = Array.unsafe_get st (32 + l)
  and h1 = Array.unsafe_get st (40 + l)
  and h2 = Array.unsafe_get st (48 + l)
  and h3 = Array.unsafe_get st (56 + l) in
  let p0 = Int64.mul (Int64.of_int o0) (Int64.of_int (h0 lor 1)) in
  let p1 = Int64.mul (Int64.of_int o2) (Int64.of_int (h1 lor 1)) in
  let lo0 = Int64.to_int p0 land mask
  and hi0 = Int64.to_int (Int64.shift_right_logical p0 32)
  and lo1 = Int64.to_int p1 land mask
  and hi1 = Int64.to_int (Int64.shift_right_logical p1 32) in
  let n0 = o1 lxor hi1 lxor lo1
  and n1 = rotl lo1 16 lxor h2
  and n2 = o3 lxor hi0 lxor lo0
  and n3 = rotl lo0 16 lxor h3 in
  let h0 = h0 lxor rotl h1 7 in
  let h1 = h1 lxor rotl h2 13 in
  let h2 = h2 lxor rotl h3 22 in
  let h3 = h3 lxor rotl h0 3 in
  Array.unsafe_set st l n0;
  Array.unsafe_set st (8 + l) n1;
  Array.unsafe_set st (16 + l) n2;
  Array.unsafe_set st (24 + l) n3;
  Array.unsafe_set st (32 + l) (((h0 + weyl) land mask) lxor n0);
  Array.unsafe_set st (40 + l) h1;
  Array.unsafe_set st (48 + l) h2;
  Array.unsafe_set st (56 + l) h3

let step_row st =
  for l = 0 to 7 do
    step st l
  done

(* F on lane [l]: eight rounds of T, a round constant, and a swap of the halves. *)
let f_lane st l =
  for r = 0 to 7 do
    step st l;
    Array.unsafe_set st l (Array.unsafe_get st l lxor Array.unsafe_get rc r);
    for w = 0 to 3 do
      let a = (w lsl 3) + l and b = 32 + (w lsl 3) + l in
      let t = Array.unsafe_get st a in
      Array.unsafe_set st a (Array.unsafe_get st b);
      Array.unsafe_set st b t
    done
  done

(* [F(key, counter, domain, aux)] with the counter as two 32-bit limbs. Returns the exposed
   half and then the hidden half, as eight words. *)
let f_keyed key ~lo ~hi ~domain ~aux =
  let st = Array.make 64 0 in
  st.(0) <- lo;
  st.(8) <- hi;
  st.(16) <- domain;
  st.(24) <- aux;
  for w = 0 to 3 do
    st.(32 + (w lsl 3)) <- key.(w)
  done;
  f_lane st 0;
  Array.init 8 (fun i -> st.(((i land 3) lsl 3) + ((i lsr 2) lsl 5)))

(* tandem_stubs.c reads and writes these fields by position: keep their order. [st] has the
   layout of [o] and [h] in tandem.c's [tandem_rng], so the C fills share this cache. *)
type ctx = {
  key : int array;
  shift : int;  (* log2 of the chunk length K *)
  st : int array;
  mutable row : int;  (* the row [st] holds, or -1 *)
}

let log2 k =
  let rec go n s = if n = 1 then s else go (n lsr 1) (s + 1) in
  go k 0

let make_ctx key k = { key; shift = log2 k; st = Array.make 64 0; row = -1 }

(* F on the eight chunks of group [g]. *)
let seed_group ctx g =
  let st = ctx.st in
  for l = 0 to 7 do
    let c = (8 * g) + l in
    st.(l) <- c land mask;
    st.(8 + l) <- c lsr 32;
    st.(16 + l) <- domain_stream;
    st.(24 + l) <- aux_stream;
    for w = 0 to 3 do
      st.(32 + (w lsl 3) + l) <- ctx.key.(w)
    done;
    f_lane st l
  done

(* Make [ctx.st] hold row [row]. Stepping forward inside the cached group costs one step per
   row. Any other move reseeds the group. *)
let[@inline never] move ctx row =
  let cur = ctx.row in
  if cur >= 0 && row > cur && row lsr ctx.shift = cur lsr ctx.shift then
    for _ = cur + 1 to row do
      step_row ctx.st
    done
  else begin
    seed_group ctx (row lsr ctx.shift);
    for _ = 0 to row land ((1 lsl ctx.shift) - 1) do
      step_row ctx.st
    done
  end;
  ctx.row <- row

let[@inline] load ctx row = if ctx.row <> row then move ctx row

(* Word [i] of the loaded row in stream order, [i] in 0 to 31. *)
let[@inline] word ctx i =
  Array.unsafe_get ctx.st (((i land 3) lsl 3) lor (i lsr 2))
