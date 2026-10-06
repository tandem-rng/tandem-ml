(* The step T, the seeding function F and the row cache.

   A word is a 32-bit value held in a native int. The one product that needs 64 bits goes
   through an unboxed [Int64]. A row is the eight chunks of a group at one step. The cache
   keeps them word-major: slot [w * 8 + l] is exposed word [w] of lane [l], and slot
   [32 + w * 8 + l] is hidden word [w].

   A state is 64 int64 slots in a Bytes block, not an int array: OCaml 5 on arm64 assigns an
   array element by a release store, and the step stores eight words per lane. *)

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

external get64 : Bytes.t -> int -> int64 = "%caml_bytes_get64u"
external set64 : Bytes.t -> int -> int64 -> unit = "%caml_bytes_set64u"

let make_state () = Bytes.make 512 '\000'
let[@inline] get st i = Int64.to_int (get64 st (i lsl 3))
let[@inline] set st i x = set64 st (i lsl 3) (Int64.of_int x)

(* T on lane [l], in place. It works on the unboxed int64 slots, so no word is tagged. *)
let[@inline] step st l =
  let xor = Int64.logxor and low x = Int64.logand x 0xffff_ffffL in
  let[@inline] high x = Int64.shift_right_logical x 32 in
  let[@inline] rotl x r = low (Int64.logor (Int64.shift_left x r) (Int64.shift_right_logical x (32 - r))) in
  let[@inline] ld w = get64 st ((l + (w lsl 3)) lsl 3) in
  let[@inline] put w x = set64 st ((l + (w lsl 3)) lsl 3) x in
  let o0 = ld 0 and o1 = ld 1 and o2 = ld 2 and o3 = ld 3 in
  let h0 = ld 4 and h1 = ld 5 and h2 = ld 6 and h3 = ld 7 in
  let p0 = Int64.mul o0 (Int64.logor h0 1L) and p1 = Int64.mul o2 (Int64.logor h1 1L) in
  let n0 = xor o1 (xor (high p1) (low p1))
  and n1 = xor (rotl (low p1) 16) h2
  and n2 = xor o3 (xor (high p0) (low p0))
  and n3 = xor (rotl (low p0) 16) h3 in
  let h0 = xor h0 (rotl h1 7) in
  let h1 = xor h1 (rotl h2 13) in
  let h2 = xor h2 (rotl h3 22) in
  let h3 = xor h3 (rotl h0 3) in
  put 0 n0;
  put 1 n1;
  put 2 n2;
  put 3 n3;
  put 4 (xor (low (Int64.add h0 (Int64.of_int weyl))) n0);
  put 5 h1;
  put 6 h2;
  put 7 h3

(* Unrolled, so that every slot offset is a constant. *)
let step_row st =
  step st 0;
  step st 1;
  step st 2;
  step st 3;
  step st 4;
  step st 5;
  step st 6;
  step st 7

(* F on lane [l]: eight rounds of T, a round constant, and a swap of the halves. *)
let f_lane st l =
  for r = 0 to 7 do
    step st l;
    set st l (get st l lxor Array.unsafe_get rc r);
    for w = 0 to 3 do
      let a = (w lsl 3) + l and b = 32 + (w lsl 3) + l in
      let t = get st a in
      set st a (get st b);
      set st b t
    done
  done

(* [F(key, counter, domain, aux)] with the counter as two 32-bit limbs. Returns the exposed
   half and then the hidden half, as eight words. *)
let f_keyed key ~lo ~hi ~domain ~aux =
  let st = make_state () in
  set st 0 lo;
  set st 8 hi;
  set st 16 domain;
  set st 24 aux;
  for w = 0 to 3 do
    set st (32 + (w lsl 3)) key.(w)
  done;
  f_lane st 0;
  Array.init 8 (fun i -> get st (((i land 3) lsl 3) + ((i lsr 2) lsl 5)))

(* tandem_stubs.c reads and writes these fields by position: keep their order. [st] has the
   layout of [o] and [h] in tandem.c's [tandem_rng], so the C fills share this cache. *)
type ctx = {
  key : int array;
  shift : int;  (* log2 of the chunk length K *)
  st : Bytes.t;
  mutable row : int;  (* the row [st] holds, or -1 *)
  buf : Bytes.t;  (* [buf_words] words in stream order for the scalar draws *)
  mutable base : int;  (* the index of the first word in [buf] *)
}

(* Eight rows. A power of two: tandem_stubs.c aligns the start of a refill to it. *)
let buf_words = 256

let log2 k =
  let rec go n s = if n = 1 then s else go (n lsr 1) (s + 1) in
  go k 0

let make_ctx key k =
  { key; shift = log2 k; st = make_state (); row = -1; buf = Bytes.create (4 * buf_words); base = min_int }

(* F on chunk [l] of group [g], into lane [l]. *)
let seed_lane st key g l =
  let c = (8 * g) + l in
  set st l (c land mask);
  set st (8 + l) (c lsr 32);
  set st (16 + l) domain_stream;
  set st (24 + l) aux_stream;
  for w = 0 to 3 do
    set st (32 + (w lsl 3) + l) key.(w)
  done;
  f_lane st l

let seed_group ctx g =
  for l = 0 to 7 do
    seed_lane ctx.st ctx.key g l
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

(* Word [i] of the row in [st] in stream order, [i] in 0 to 31, untagged and tagged. *)
let[@inline] raw st i = get64 st ((((i land 3) lsl 3) lor (i lsr 2)) lsl 3)
let[@inline] word st i = Int64.to_int (raw st i)
