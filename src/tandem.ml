module A1 = Bigarray.Array1

type 'k ba = (float, 'k, Bigarray.c_layout) A1.t
type u32_array = (int32, Bigarray.int32_elt, Bigarray.c_layout) A1.t
type u64_array = (int64, Bigarray.int64_elt, Bigarray.c_layout) A1.t

let mask = Engine.mask

(* The position is [hi * 2^32 + lo] with both limbs below 2^32, so that the whole range up to
   2^64 fits in native ints and a draw allocates no boxed [Int64]. *)
type t = { ctx : Engine.ctx; hi : int; lo : int }

let default_chunk_length = 32

let check_chunk_length k =
  if k < 1 || k > 65536 || k land (k - 1) <> 0 then
    invalid_arg "Tandem: the chunk length must be a power of two in 1 to 65536"

let check_key key =
  if Array.length key <> 4 then invalid_arg "Tandem: a key has four words";
  Array.iter
    (fun w -> if w < 0 || w > mask then invalid_arg "Tandem: a key word is 32 bits")
    key

let of_key_at key k hi lo = { ctx = Engine.make_ctx key k; hi; lo }

let of_key ?(chunk_length = default_chunk_length) ?(position = 0L) key =
  check_chunk_length chunk_length;
  check_key key;
  if Int64.compare position 0L < 0 then invalid_arg "Tandem: a start position is below 2^63";
  of_key_at (Array.copy key) chunk_length
    (Int64.to_int (Int64.shift_right_logical position 32))
    (Int64.to_int (Int64.logand position 0xffff_ffffL))

let chunk_length t = 1 lsl t.ctx.shift
let key t = Array.copy t.ctx.key

let position t = Int64.logor (Int64.shift_left (Int64.of_int t.hi) 32) (Int64.of_int t.lo)

let seek t p =
  if Int64.compare p 0L < 0 then invalid_arg "Tandem: a position is below 2^63";
  { t with hi = Int64.to_int (Int64.shift_right_logical p 32); lo = Int64.to_int (Int64.logand p 0xffff_ffffL) }

let equal a b =
  a.hi = b.hi && a.lo = b.lo && a.ctx.shift = b.ctx.shift && a.ctx.key = b.ctx.key

let seed_u128 ?(chunk_length = default_chunk_length) lo hi =
  check_chunk_length chunk_length;
  let limb x s = Int64.to_int (Int64.logand (Int64.shift_right_logical x s) 0xffff_ffffL) in
  let h = [| limb lo 0; limb lo 32; limb hi 0; limb hi 32 |] in
  let o = Engine.f_keyed h ~lo:0 ~hi:0 ~domain:Engine.domain_seed ~aux:0 in
  of_key_at (Array.sub o 0 4) chunk_length 0 0

let seed ?chunk_length z =
  if z < 0 then invalid_arg "Tandem.seed: a seed is not negative";
  seed_u128 ?chunk_length (Int64.of_int z) 0L

let overflow () = invalid_arg "Tandem: the stream ends at bit 2^64"

(* The position after [bits] more bits, from the aligned low limb [alo], which may be 2^32. *)
let[@inline] advance t alo bits =
  let lo2 = alo + bits in
  let hi2 = t.hi + (lo2 lsr 32) in
  if hi2 > mask then overflow ();
  { t with hi = hi2; lo = lo2 land mask }

let[@inline] align_lo t w = (t.lo + w - 1) land lnot (w - 1)

(* Index of the 32-bit word at the aligned limb [alo]. *)
let[@inline] word_index t alo = (t.hi lsl 27) + (alo lsr 5)

(* ---- Scalar draws -------------------------------------------------------------------- *)

let[@inline] read32 ctx w =
  Engine.load ctx (w lsr 5);
  Engine.word ctx (w land 31)

let bool t =
  let w = word_index t t.lo in
  let b = (read32 t.ctx w lsr (t.lo land 31)) land 1 = 1 in
  (b, advance t t.lo 1)

let u32 t =
  let alo = align_lo t 32 in
  let x = read32 t.ctx (word_index t alo) in
  (x, advance t alo 32)

let u64 t =
  let alo = align_lo t 64 in
  let w = word_index t alo in
  Engine.load t.ctx (w lsr 5);
  let i = w land 31 in
  let lo = Engine.word t.ctx i and hi = Engine.word t.ctx (i + 1) in
  (Int64.logor (Int64.of_int lo) (Int64.shift_left (Int64.of_int hi) 32), advance t alo 64)

let[@inline] to_f64 lo hi = float_of_int ((hi lsl 21) lor (lo lsr 11)) *. 0x1p-53
let[@inline] to_f32 x = float_of_int (x lsr 8) *. 0x1p-24

let float t =
  let alo = align_lo t 64 in
  let w = word_index t alo in
  Engine.load t.ctx (w lsr 5);
  let i = w land 31 in
  (to_f64 (Engine.word t.ctx i) (Engine.word t.ctx (i + 1)), advance t alo 64)

let float32 t =
  let alo = align_lo t 32 in
  let x = read32 t.ctx (word_index t alo) in
  (to_f32 x, advance t alo 32)

(* ---- Derived generators -------------------------------------------------------------- *)

let child t ~lo ~hi ~domain ~aux ~half =
  let o = Engine.f_keyed t.ctx.Engine.key ~lo ~hi ~domain ~aux in
  of_key_at (Array.sub o (4 * half) 4) (chunk_length t) 0 0

let limbs x = (Int64.to_int (Int64.logand x 0xffff_ffffL), Int64.to_int (Int64.shift_right_logical x 32))

let split_u64 t i =
  let lo, hi = limbs (Int64.shift_right_logical i 1) in
  child t ~lo ~hi ~domain:Engine.domain_split ~aux:0 ~half:(Int64.to_int (Int64.logand i 1L))

let purpose_u64 t u =
  let lo, hi = limbs u in
  child t ~lo ~hi ~domain:Engine.domain_fold ~aux:0 ~half:0

let non_negative name i = if i < 0 then invalid_arg ("Tandem." ^ name ^ ": the index is not negative")
let split t i = non_negative "split" i; split_u64 t (Int64.of_int i)
let purpose t u = non_negative "purpose" u; purpose_u64 t (Int64.of_int u)

let fork t n =
  if n < 0 then invalid_arg "Tandem.fork: the batch size is not negative";
  let b = (t.hi lsl 25) lor (t.lo lsr 7) in
  let next = b + 1 in
  if next lsr 57 <> 0 then overflow ();
  let kids =
    Array.init n (fun i ->
        child t ~lo:(b land mask) ~hi:(b lsr 32) ~domain:Engine.domain_fork
          ~aux:((i lsr 1) land mask) ~half:(i land 1))
  in
  ({ t with hi = next lsr 25; lo = (next lsl 7) land mask }, kids)

(* ---- Bounded integers ---------------------------------------------------------------- *)

let purpose_below32 = 0x42_4c57_3332
let purpose_below64 = 0x42_4c57_3634

let rec below32 t range =
  if range < 0 || range > 1 lsl 32 then invalid_arg "Tandem.below32: the range is 0 to 2^32";
  let x, t = u32 t in
  if range = 0 then (0, t)
  else begin
    let rejected = ((1 lsl 32) - range) mod range in
    lemire32 t x range rejected
  end

and lemire32 t x range rejected =
  let m = Int64.mul (Int64.of_int x) (Int64.of_int range) in
  if Int64.to_int m land mask >= rejected then (Int64.to_int (Int64.shift_right_logical m 32), t)
  else
    let x, t = u32 t in
    lemire32 t x range rejected

let mulhi64 a b =
  let m = 0xffff_ffffL in
  let a0 = Int64.logand a m and a1 = Int64.shift_right_logical a 32 in
  let b0 = Int64.logand b m and b1 = Int64.shift_right_logical b 32 in
  let p00 = Int64.mul a0 b0 and p01 = Int64.mul a0 b1 in
  let p10 = Int64.mul a1 b0 and p11 = Int64.mul a1 b1 in
  let mid =
    Int64.add
      (Int64.add (Int64.shift_right_logical p00 32) (Int64.logand p01 m))
      (Int64.logand p10 m)
  in
  Int64.add
    (Int64.add (Int64.add p11 (Int64.shift_right_logical p01 32)) (Int64.shift_right_logical p10 32))
    (Int64.shift_right_logical mid 32)

let below64 t range =
  let x, t = u64 t in
  if Int64.equal range 0L then (0L, t)
  else begin
    let rejected = Int64.unsigned_rem (Int64.neg range) range in
    let rec go x t =
      if Int64.unsigned_compare (Int64.mul x range) rejected >= 0 then (mulhi64 x range, t)
      else
        let x, t = u64 t in
        go x t
    in
    go x t
  end

let below t range =
  if range < 0 then invalid_arg "Tandem.below: the range is not negative";
  if range <= 1 lsl 32 then below32 t range
  else
    let x, t = below64 t (Int64.of_int range) in
    (Int64.to_int x, t)

let between t ~lo ~hi =
  if hi <= lo then invalid_arg "Tandem.between: the interval is empty";
  let range = Int64.sub (Int64.of_int hi) (Int64.of_int lo) in
  if Int64.compare range 0L > 0 && Int64.compare range 0x1_0000_0000L <= 0 then
    let x, t = below32 t (Int64.to_int range) in
    (lo + x, t)
  else
    let x, t = below64 t range in
    (lo + Int64.to_int x, t)

(* ---- Normals and exponentials, scalar ------------------------------------------------ *)

let scratch_key = Domain.DLS.new_key Box.make_scratch

let normal2 t =
  let a, t = float t in
  let b, t = float t in
  let z = Float.Array.of_list [ a; b ] in
  Box.normal_block_f64 z 0 1;
  (Float.Array.get z 0, Float.Array.get z 1, t)

let normal t =
  let z0, _, t = normal2 t in
  (z0, t)

let normal2_f32 t =
  let a, t = float32 t in
  let b, t = float32 t in
  let z = Float.Array.of_list [ a; b ] in
  Box.normal_block_f32 (Domain.DLS.get scratch_key) z 0 1;
  (Float.Array.get z 0, Float.Array.get z 1, t)

let normal_f32 t =
  let z0, _, t = normal2_f32 t in
  (z0, t)

let exponential t =
  let u, t = float t in
  let z = Float.Array.make 1 u in
  Box.exponential_block_f64 z 0 1;
  (Float.Array.get z 0, t)

let exponential_f32 t =
  let u, t = float32 t in
  let z = Float.Array.make 1 u in
  Box.exponential_block_f32 (Domain.DLS.get scratch_key) z 0 1;
  (Float.Array.get z 0, t)

(* ---- Fills --------------------------------------------------------------------------- *)

let range_of name dim off len =
  let off = Option.value off ~default:0 in
  let len = Option.value len ~default:(dim - off) in
  if off < 0 || len < 0 || off > dim - len then invalid_arg ("Tandem." ^ name ^ ": the range is outside the array");
  (off, len)

(* Rows of a fill of [n] elements of [32 lsl sh] bits from the 32-bit word [word0], which is a
   multiple of the element width. [f first count dst] writes [count] elements, the first at
   index [first] of the loaded row and the first output at [dst]. *)
let iter_rows ctx ~word0 ~sh ~n f =
  let per = 32 lsr sh in
  let i = ref 0 and w = ref word0 in
  while !i < n do
    Engine.load ctx (!w lsr 5);
    let first = (!w land 31) lsr sh in
    let cnt = min (per - first) (n - !i) in
    f first cnt !i;
    i := !i + cnt;
    w := !w + (cnt lsl sh)
  done

(* Align, check the end of the fill against 2^64, and run [f] on the words. Returns the
   position after the fill. An empty fill returns the aligned position. *)
let fill_with t ~w ~n f =
  if n > 1 lsl 55 then overflow ();
  let alo = align_lo t w in
  let t' = advance t alo (n * w) in
  if n > 0 then f ~word0:(word_index t alo);
  t'

let fill_u32 ?off ?len t (a : u32_array) =
  let off, n = range_of "fill_u32" (A1.dim a) off len in
  fill_with t ~w:32 ~n (fun ~word0 ->
      iter_rows t.ctx ~word0 ~sh:0 ~n (fun first cnt dst ->
          for k = 0 to cnt - 1 do
            A1.unsafe_set a (off + dst + k) (Int32.of_int (Engine.word t.ctx (first + k)))
          done))

let fill_u64 ?off ?len t (a : u64_array) =
  let off, n = range_of "fill_u64" (A1.dim a) off len in
  fill_with t ~w:64 ~n (fun ~word0 ->
      iter_rows t.ctx ~word0 ~sh:1 ~n (fun first cnt dst ->
          for k = 0 to cnt - 1 do
            let e = (first + k) lsl 1 in
            let lo = Engine.word t.ctx e and hi = Engine.word t.ctx (e + 1) in
            A1.unsafe_set a (off + dst + k)
              (Int64.logor (Int64.of_int lo) (Int64.shift_left (Int64.of_int hi) 32))
          done))

let fill_f64_bigarray ?off ?len t (a : Bigarray.float64_elt ba) =
  let off, n = range_of "fill_float" (A1.dim a) off len in
  fill_with t ~w:64 ~n (fun ~word0 ->
      iter_rows t.ctx ~word0 ~sh:1 ~n (fun first cnt dst ->
          for k = 0 to cnt - 1 do
            let e = (first + k) lsl 1 in
            A1.unsafe_set a (off + dst + k) (to_f64 (Engine.word t.ctx e) (Engine.word t.ctx (e + 1)))
          done))

let fill_f32_bigarray ?off ?len t (a : Bigarray.float32_elt ba) =
  let off, n = range_of "fill_float32" (A1.dim a) off len in
  fill_with t ~w:32 ~n (fun ~word0 ->
      iter_rows t.ctx ~word0 ~sh:0 ~n (fun first cnt dst ->
          for k = 0 to cnt - 1 do
            A1.unsafe_set a (off + dst + k) (to_f32 (Engine.word t.ctx (first + k)))
          done))

(* The same on a [Float.Array] region. [off] and [n] are checked by the caller. *)
let fa_f64 t (a : Float.Array.t) off n =
  fill_with t ~w:64 ~n (fun ~word0 ->
      iter_rows t.ctx ~word0 ~sh:1 ~n (fun first cnt dst ->
          for k = 0 to cnt - 1 do
            let e = (first + k) lsl 1 in
            Float.Array.unsafe_set a (off + dst + k)
              (to_f64 (Engine.word t.ctx e) (Engine.word t.ctx (e + 1)))
          done))

let fa_f32 t (a : Float.Array.t) off n =
  fill_with t ~w:32 ~n (fun ~word0 ->
      iter_rows t.ctx ~word0 ~sh:0 ~n (fun first cnt dst ->
          for k = 0 to cnt - 1 do
            Float.Array.unsafe_set a (off + dst + k) (to_f32 (Engine.word t.ctx (first + k)))
          done))

(* Derived fills draw the uniforms in blocks that stay in cache and map them in place. An
   empty one moves nothing. *)
let block = 1024

let normal_region ~wide t a off n =
  if n = 0 then t
  else begin
    let uniform = if wide then fa_f64 else fa_f32 in
    let sc = Domain.DLS.get scratch_key in
    let map z o m = if wide then Box.normal_block_f64 z o m else Box.normal_block_f32 sc z o m in
    let t = ref t and at = ref 0 in
    let even = n land lnot 1 in
    while !at < even do
      let b = min block (even - !at) in
      t := uniform !t a (off + !at) b;
      map a (off + !at) (b / 2);
      at := !at + b
    done;
    if n land 1 = 1 then begin
      let pair = Float.Array.create 2 in
      t := uniform !t pair 0 2;
      map pair 0 1;
      Float.Array.set a (off + n - 1) (Float.Array.get pair 0)
    end;
    !t
  end

let exponential_region ~wide t a off n =
  let uniform = if wide then fa_f64 else fa_f32 in
  let sc = Domain.DLS.get scratch_key in
  let t = ref t and at = ref 0 in
  while !at < n do
    let b = min block (n - !at) in
    t := uniform !t a (off + !at) b;
    if wide then Box.exponential_block_f64 a (off + !at) b else Box.exponential_block_f32 sc a (off + !at) b;
    at := !at + b
  done;
  !t

module Float_array = struct
  let region name a off len =
    range_of name (Float.Array.length a) off len

  let fill_float ?off ?len t a =
    let off, n = region "fill_float" a off len in
    fa_f64 t a off n

  let fill_float32 ?off ?len t a =
    let off, n = region "fill_float32" a off len in
    fa_f32 t a off n

  let fill_normal ?off ?len t a =
    let off, n = region "fill_normal" a off len in
    normal_region ~wide:true t a off n

  let fill_normal32 ?off ?len t a =
    let off, n = region "fill_normal32" a off len in
    normal_region ~wide:false t a off n

  let fill_exponential ?off ?len t a =
    let off, n = region "fill_exponential" a off len in
    exponential_region ~wide:true t a off n

  let fill_exponential32 ?off ?len t a =
    let off, n = region "fill_exponential32" a off len in
    exponential_region ~wide:false t a off n
end

let fill_float = fill_f64_bigarray
let fill_float32 = fill_f32_bigarray

(* A bigarray target goes through a block of floats. The bigarray stores are written out for
   each kind, because the compiler inlines them only for a known kind. *)
let via_scratch region t n copy =
  let scratch = Float.Array.create (min block n) in
  let t = ref t and at = ref 0 in
  while !at < n do
    let b = min block (n - !at) in
    t := region !t scratch 0 b;
    copy scratch !at b;
    at := !at + b
  done;
  !t

let copy64 (a : Bigarray.float64_elt ba) off scratch at b =
  for i = 0 to b - 1 do
    A1.unsafe_set a (off + at + i) (Float.Array.unsafe_get scratch i)
  done

let copy32 (a : Bigarray.float32_elt ba) off scratch at b =
  for i = 0 to b - 1 do
    A1.unsafe_set a (off + at + i) (Float.Array.unsafe_get scratch i)
  done

let fill_normal ?off ?len t (a : Bigarray.float64_elt ba) =
  let off, n = range_of "fill_normal" (A1.dim a) off len in
  if n = 0 then t else via_scratch (normal_region ~wide:true) t n (copy64 a off)

let fill_normal32 ?off ?len t (a : Bigarray.float32_elt ba) =
  let off, n = range_of "fill_normal32" (A1.dim a) off len in
  if n = 0 then t else via_scratch (normal_region ~wide:false) t n (copy32 a off)

let fill_exponential ?off ?len t (a : Bigarray.float64_elt ba) =
  let off, n = range_of "fill_exponential" (A1.dim a) off len in
  if n = 0 then t else via_scratch (exponential_region ~wide:true) t n (copy64 a off)

let fill_exponential32 ?off ?len t (a : Bigarray.float32_elt ba) =
  let off, n = range_of "fill_exponential32" (A1.dim a) off len in
  if n = 0 then t else via_scratch (exponential_region ~wide:false) t n (copy32 a off)

(* ---- Bounded fills ------------------------------------------------------------------- *)

(* A rejected draw retries on the fallback stream of its global draw index [g]. *)
let fallback t p g = split (purpose { t with hi = 0; lo = 0 } p) g

let fill_below32 ?off ?len t ~range (a : u32_array) =
  if range < 0 || range > 1 lsl 32 then invalid_arg "Tandem.fill_below32: the range is 0 to 2^32";
  let off, n = range_of "fill_below32" (A1.dim a) off len in
  if n = 0 then t
  else begin
    let g0 = word_index t (align_lo t 32) in
    let t' = fill_u32 ~off ~len:n t a in
    if range = 0 then
      for i = off to off + n - 1 do
        A1.unsafe_set a i 0l
      done
    else begin
      let rejected = ((1 lsl 32) - range) mod range in
      for i = 0 to n - 1 do
        let x = Int32.to_int (A1.unsafe_get a (off + i)) land mask in
        let m = Int64.mul (Int64.of_int x) (Int64.of_int range) in
        let v =
          if Int64.to_int m land mask >= rejected then Int64.to_int (Int64.shift_right_logical m 32)
          else fst (below32 (fallback t purpose_below32 (g0 + i)) range)
        in
        A1.unsafe_set a (off + i) (Int32.of_int v)
      done
    end;
    t'
  end

let fill_below64 ?off ?len t ~range (a : u64_array) =
  let off, n = range_of "fill_below64" (A1.dim a) off len in
  if n = 0 then t
  else begin
    let g0 = word_index t (align_lo t 64) lsr 1 in
    let t' = fill_u64 ~off ~len:n t a in
    if Int64.equal range 0L then
      for i = off to off + n - 1 do
        A1.unsafe_set a i 0L
      done
    else begin
      let rejected = Int64.unsigned_rem (Int64.neg range) range in
      for i = 0 to n - 1 do
        let x = A1.unsafe_get a (off + i) in
        let v =
          if Int64.unsigned_compare (Int64.mul x range) rejected >= 0 then mulhi64 x range
          else fst (below64 (fallback t purpose_below64 (g0 + i)) range)
        in
        A1.unsafe_set a (off + i) v
      done
    end;
    t'
  end

(* ---- Specification building blocks --------------------------------------------------- *)

module Spec = struct
  let step ~o ~h =
    let st = Array.make 64 0 in
    for w = 0 to 3 do
      st.(w lsl 3) <- o.(w);
      st.(32 + (w lsl 3)) <- h.(w)
    done;
    Engine.step st 0;
    (Array.init 4 (fun w -> st.(w lsl 3)), Array.init 4 (fun w -> st.(32 + (w lsl 3))))

  let f_keyed key ~counter ~domain ~aux =
    let lo, hi = limbs counter in
    let r = Engine.f_keyed key ~lo ~hi ~domain ~aux in
    (Array.sub r 0 4, Array.sub r 4 4)

  let block key ~chunk ~j =
    let o, h = f_keyed key ~counter:(Int64.of_int chunk) ~domain:Engine.domain_stream ~aux:Engine.aux_stream in
    let o = ref o and h = ref h in
    for _ = 0 to j do
      let o2, h2 = step ~o:!o ~h:!h in
      o := o2;
      h := h2
    done;
    !o
end

(* ---- Stateful wrapper -------------------------------------------------------------- *)

module State = struct
  type g = t
  type t = { mutable g : g }

  let of_generator g = { g }
  let generator s = s.g
  let make_seed ?chunk_length z = { g = seed ?chunk_length z }

  let make a =
    let n = Array.length a in
    if n < 1 || n > 2 then invalid_arg "Tandem.State.make: one or two seed integers";
    { g = seed_u128 (Int64.of_int a.(0)) (if n = 2 then Int64.of_int a.(1) else 0L) }

  let copy s = { g = s.g }

  let draw s f =
    let x, g = f s.g in
    s.g <- g;
    x

  let bits32 s = Int32.of_int (draw s u32)
  let bits64 s = draw s u64
  let bits s = draw s u32 lsr 2
  let bool s = draw s bool
  let float s scale = scale *. draw s float

  let int s bound =
    if bound <= 0 || bound > 0x3fff_ffff then invalid_arg "Random.int";
    draw s (fun g -> below32 g bound)

  let full_int s bound =
    if bound <= 0 then invalid_arg "Random.full_int";
    draw s (fun g -> below g bound)

  let int32 s bound =
    if Int32.compare bound 0l <= 0 then invalid_arg "Random.int32";
    Int32.of_int (draw s (fun g -> below32 g (Int32.to_int bound)))

  let int64 s bound =
    if Int64.compare bound 0L <= 0 then invalid_arg "Random.int64";
    draw s (fun g -> below64 g bound)

  let normal s = draw s normal
  let exponential s = draw s exponential

  let split s =
    let g, kids = fork s.g 1 in
    s.g <- g;
    { g = kids.(0) }
end
