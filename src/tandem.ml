module A1 = Bigarray.Array1

type u32_array = (int32, Bigarray.int32_elt, Bigarray.c_layout) A1.t
type u64_array = (int64, Bigarray.int64_elt, Bigarray.c_layout) A1.t
type f64_array = (float, Bigarray.float64_elt, Bigarray.c_layout) A1.t

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

let[@inline] align lo w = (lo + w - 1) land lnot (w - 1)

(* Index of the 32-bit word at the aligned low limb [alo], which may be 2^32. *)
let[@inline] word_at hi alo = (hi lsl 27) + (alo lsr 5)

(* The high limb after [bits] more bits from [alo]. *)
let[@inline] end_hi hi alo bits =
  let hi2 = hi + ((alo + bits) lsr 32) in
  if hi2 > mask then overflow ();
  hi2

let[@inline] advance t alo bits = { t with hi = end_hi t.hi alo bits; lo = (alo + bits) land mask }

(* ---- Scalar draws -------------------------------------------------------------------- *)

(* A reader maps the index [w] of a 32-bit word to the value that starts there. *)

let[@inline] f64_of_u64 x = float_of_int (Int64.to_int (Int64.shift_right_logical x 11)) *. 0x1p-53

(* The 64-bit value at the even word [e] of the row in [st]. *)
let[@inline] row_u64 st e = Int64.logor (Engine.raw st e) (Int64.shift_left (Engine.raw st (e + 1)) 32)

(* The readers of [Pure], through the row cache that Engine steps in OCaml. *)
let[@inline] read32 (ctx : Engine.ctx) w =
  Engine.load ctx (w lsr 5);
  Engine.word ctx.st (w land 31)

let[@inline] read_u64 (ctx : Engine.ctx) w =
  Engine.load ctx (w lsr 5);
  row_u64 ctx.st (w land 31)

(* The scalar draws read [ctx.buf], which one C fill refills. A noalloc call keeps the caller's
   values in callee-saved registers, where an OCaml call would spill them on every draw. *)
external refill : Engine.ctx -> (int[@untagged]) -> unit = "tandem_ml_refill_byte" "tandem_ml_refill"
  [@@noalloc]

external get32 : Bytes.t -> int -> int32 = "%caml_bytes_get32u"

(* The buffer index of word [w]. The two words of a 64-bit value share a row, so one check
   covers both. *)
let[@inline] slot (ctx : Engine.ctx) w =
  if (w - ctx.base) land lnot (Engine.buf_words - 1) <> 0 then refill ctx w;
  w - ctx.base

let[@inline] fast32 (ctx : Engine.ctx) w = Int32.to_int (get32 ctx.buf (slot ctx w lsl 2)) land mask

(* The 64-bit value at the even word [w], in one load. *)
let[@inline] fast_u64 (ctx : Engine.ctx) w =
  let x = Engine.get64 ctx.buf (slot ctx w lsl 2) in
  if Sys.big_endian then Int64.logor (Int64.shift_left x 32) (Int64.shift_right_logical x 32) else x

let[@inline] fast_f64 ctx w = f64_of_u64 (fast_u64 ctx w)

let[@inline] read_bool ctx hi lo = (fast32 ctx (word_at hi lo) lsr (lo land 31)) land 1 = 1

let[@inline] draw32 read t =
  let alo = align t.lo 32 in
  (read t.ctx (word_at t.hi alo), advance t alo 32)

let[@inline] draw64 read t =
  let alo = align t.lo 64 in
  (read t.ctx (word_at t.hi alo), advance t alo 64)

let[@inline] bool t = (read_bool t.ctx t.hi t.lo, advance t t.lo 1)
let[@inline] u32 t = draw32 fast32 t
let[@inline] u64 t = draw64 fast_u64 t
let[@inline] float t = draw64 fast_f64 t

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

(* Lemire's multiply and reject. The threshold is below [range], so a low half at or above
   [range] accepts without the division. *)
let[@inline] below32_with draw t range =
  if range < 0 || range > 1 lsl 32 then invalid_arg "Tandem.below32: the range is 0 to 2^32";
  let x, t = draw t in
  if range = 0 then (0, t)
  else begin
    (* The low 32 bits of the product are exact in an int. *)
    let x = ref x and t = ref t in
    if (!x * range) land mask < range then begin
      let rejected = ((1 lsl 32) - range) mod range in
      while (!x * range) land mask < rejected do
        let x', t' = draw !t in
        x := x';
        t := t'
      done
    end;
    (Int64.to_int (Int64.shift_right_logical (Int64.mul (Int64.of_int !x) (Int64.of_int range)) 32), !t)
  end

let[@inline] below32 t range = below32_with u32 t range

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

let below64_with draw t range =
  let x, t = draw t in
  if Int64.equal range 0L then (0L, t)
  else begin
    let rejected = Int64.unsigned_rem (Int64.neg range) range in
    let rec go x t =
      if Int64.unsigned_compare (Int64.mul x range) rejected >= 0 then (mulhi64 x range, t)
      else
        let x, t = draw t in
        go x t
    in
    go x t
  end

let below64 t range = below64_with u64 t range

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

(* The ziggurat of Appendix A on the 64-bit draw with words [lo] and [hi]. The bits 0 to 9 of
   [lo] pick the layer, bit 10 the sign through the signed width table, and the bits 11 to 63
   are the magnitude [ra]. *)
let purpose_normal64 = 0x4e_524d_3634L

let[@inline] zig_magnitude lo hi = (hi lsl 21) lor (lo lsr 11)
let[@inline] zig_x lo ra = float_of_int ra *. Array.unsafe_get Zig_tables.w (lo land 2047)
let[@inline] zig_inside lo ra = ra < Array.unsafe_get Zig_tables.k (lo land 1023)

(* A draw outside the inner rectangles continues on the fallback stream of its global draw
   index [g], from the key of [ctx] alone. OCaml never fuses a product into a sum, so only
   [neg2_log] fuses, as the spec requires. The fallback draws 64 bits at a time from position
   0. Draw [k] below 16 lies in row 0, lane [k / 2], so only the lanes it reaches are seeded:
   one F each, not eight. *)
let[@inline never] zig_miss ctx g lo hi =
  let f = split_u64 (purpose_u64 { ctx; hi = 0; lo = 0 } purpose_normal64) (Int64.of_int g) in
  let st = Engine.make_state () and lanes = ref 0 and d = ref 0 in
  let next_u64 () =
    let k = !d in
    incr d;
    if k >= 16 then read_u64 f.ctx (2 * k)
    else begin
      while !lanes <= k lsr 1 do
        Engine.seed_lane st f.ctx.key 0 !lanes;
        Engine.step st !lanes;
        incr lanes
      done;
      row_u64 st (2 * k)
    end
  in
  let next_float () = f64_of_u64 (next_u64 ()) in
  let rec layer lo hi =
    let i = lo land 1023 and ra = zig_magnitude lo hi in
    let x = zig_x lo ra in
    if zig_inside lo ra then x
    else if i = 0 then begin
      let rec tail () =
        let a = Logarithm.exponential (next_float ()) /. Zig_tables.r in
        let b = Logarithm.exponential (next_float ()) in
        if b +. b >= a *. a then a else tail ()
      in
      let x = Zig_tables.r +. tail () in
      if (lo lsr 10) land 1 = 1 then -.x else x
    end
    else
      let y0 = Array.unsafe_get Zig_tables.y i in
      let u = next_float () in
      let y = y0 +. (u *. (Array.unsafe_get Zig_tables.y (i + 1) -. y0)) in
      if -0.5 *. Logarithm.neg2_log y < -0.5 *. (x *. x) then x
      else
        let r = next_u64 () in
        layer (Int64.to_int r land mask) (Int64.to_int (Int64.shift_right_logical r 32))
  in
  layer lo hi

let[@inline] zig ctx g lo hi =
  let ra = zig_magnitude lo hi in
  if zig_inside lo ra then zig_x lo ra else zig_miss ctx g lo hi

external c_normal : Engine.ctx -> (int[@untagged]) -> (float[@unboxed])
  = "tandem_ml_normal_byte" "tandem_ml_normal" [@@noalloc]

(* The scalar draws take a miss to tandem.c, which runs the fallback far faster. The layer and
   sign need only the low bits of [lo]. *)
let[@inline] fast_normal ctx w =
  let x = fast_u64 ctx w in
  let lo = Int64.to_int x and ra = Int64.to_int (Int64.shift_right_logical x 11) in
  if zig_inside lo ra then zig_x lo ra else c_normal ctx w

let[@inline] normal t = draw64 fast_normal t

let exponential t =
  let u, t = float t in
  (Logarithm.exponential u, t)

(* ---- Fills --------------------------------------------------------------------------- *)

module type Fills = sig
  val fill_u32 : ?off:int -> ?len:int -> t -> u32_array -> t
  val fill_u64 : ?off:int -> ?len:int -> t -> u64_array -> t
  val fill_float : ?off:int -> ?len:int -> t -> f64_array -> t
  val fill_normal : ?off:int -> ?len:int -> t -> f64_array -> t
  val fill_exponential : ?off:int -> ?len:int -> t -> f64_array -> t
  val fill_below32 : ?off:int -> ?len:int -> t -> range:int -> u32_array -> t
  val fill_below64 : ?off:int -> ?len:int -> t -> range:int64 -> u64_array -> t

  module Float_array : sig
    val fill_float : ?off:int -> ?len:int -> t -> Float.Array.t -> t
    val fill_normal : ?off:int -> ?len:int -> t -> Float.Array.t -> t
    val fill_exponential : ?off:int -> ?len:int -> t -> Float.Array.t -> t
  end
end

let range_of name dim off len =
  let off = Option.value off ~default:0 in
  let len = Option.value len ~default:(dim - off) in
  if off < 0 || len < 0 || off > dim - len then invalid_arg ("Tandem." ^ name ^ ": the range is outside the array");
  (off, len)

let check_range32 name range =
  if range < 0 || range > 1 lsl 32 then invalid_arg ("Tandem." ^ name ^ ": the range is 0 to 2^32")

(* Align, check the end of a fill of [n] draws of [w] bits against 2^64, and run [f] on the
   index of its first 32-bit word. Returns the position after the fill. An empty fill returns
   the aligned position. *)
let fill_with t ~w ~n f =
  if n > 1 lsl 55 then overflow ();
  let alo = align t.lo w in
  let t' = advance t alo (n * w) in
  if n > 0 then f (word_at t.hi alo);
  t'

module Pure = struct
  (* Rows of a fill of [n] elements of [32 lsl sh] bits from the 32-bit word [word0], which is
     a multiple of the element width. [f st first count dst] writes [count] elements, the first
     at index [first] of the row in [st] and the first output at [dst]. Inlined, so that each
     fill gets its own loop. *)
  let[@inline] iter_rows (ctx : Engine.ctx) ~word0 ~sh ~n f =
    let per = 32 lsr sh in
    let i = ref 0 and w = ref word0 in
    while !i < n do
      Engine.load ctx (!w lsr 5);
      let first = (!w land 31) lsr sh in
      let cnt = Int.min (per - first) (n - !i) in
      f ctx.st first cnt !i;
      i := !i + cnt;
      w := !w + (cnt lsl sh)
    done

  let fill_u32 ?off ?len t (a : u32_array) =
    let off, n = range_of "fill_u32" (A1.dim a) off len in
    fill_with t ~w:32 ~n (fun word0 ->
        iter_rows t.ctx ~word0 ~sh:0 ~n (fun st first cnt dst ->
            for k = 0 to cnt - 1 do
              A1.unsafe_set a (off + dst + k) (Int64.to_int32 (Engine.raw st (first + k)))
            done))

  let fill_u64 ?off ?len t (a : u64_array) =
    let off, n = range_of "fill_u64" (A1.dim a) off len in
    fill_with t ~w:64 ~n (fun word0 ->
        iter_rows t.ctx ~word0 ~sh:1 ~n (fun st first cnt dst ->
            for k = 0 to cnt - 1 do
              let e = (first + k) lsl 1 in
              A1.unsafe_set a (off + dst + k) (row_u64 st e)
            done))

  let fill_float ?off ?len t (a : f64_array) =
    let off, n = range_of "fill_float" (A1.dim a) off len in
    fill_with t ~w:64 ~n (fun word0 ->
        iter_rows t.ctx ~word0 ~sh:1 ~n (fun st first cnt dst ->
            for k = 0 to cnt - 1 do
              let e = (first + k) lsl 1 in
              A1.unsafe_set a (off + dst + k) (f64_of_u64 (row_u64 st e))
            done))

  (* The same on a [Float.Array] region that the caller checked. *)
  let uniforms t (a : Float.Array.t) off n =
    fill_with t ~w:64 ~n (fun word0 ->
        iter_rows t.ctx ~word0 ~sh:1 ~n (fun st first cnt dst ->
            for k = 0 to cnt - 1 do
              let e = (first + k) lsl 1 in
              Float.Array.unsafe_set a (off + dst + k) (f64_of_u64 (row_u64 st e))
            done))

  (* Element [i] of a normal fill is the ziggurat on draw [i] of the u64 fill. *)
  let[@inline] normals t n set =
    fill_with t ~w:64 ~n (fun word0 ->
        iter_rows t.ctx ~word0 ~sh:1 ~n (fun st first cnt dst ->
            let g = (word0 lsr 1) + dst in
            for k = 0 to cnt - 1 do
              let e = (first + k) lsl 1 in
              set (dst + k) (zig t.ctx (g + k) (Engine.word st e) (Engine.word st (e + 1)))
            done))

  (* Exponential fills draw the uniforms in blocks that stay in cache and map them in place. An
     empty one moves nothing. *)
  let block = 1024

  let exponentials t a off n =
    let t = ref t and at = ref 0 in
    while !at < n do
      let b = Int.min block (n - !at) in
      t := uniforms !t a (off + !at) b;
      Logarithm.exponential_block a (off + !at) b;
      at := !at + b
    done;
    !t

  module Float_array = struct
    let fill_float ?off ?len t a =
      let off, n = range_of "fill_float" (Float.Array.length a) off len in
      uniforms t a off n

    let fill_normal ?off ?len t a =
      let off, n = range_of "fill_normal" (Float.Array.length a) off len in
      normals t n (fun i x -> Float.Array.unsafe_set a (off + i) x)

    let fill_exponential ?off ?len t a =
      let off, n = range_of "fill_exponential" (Float.Array.length a) off len in
      exponentials t a off n
  end

  (* A bigarray target goes through a block of floats. *)
  let via_floats map t (a : f64_array) off n =
    let floats = Float.Array.create (Int.min block n) in
    let t = ref t and at = ref 0 in
    while !at < n do
      let b = Int.min block (n - !at) in
      t := map !t floats 0 b;
      for i = 0 to b - 1 do
        A1.unsafe_set a (off + !at + i) (Float.Array.unsafe_get floats i)
      done;
      at := !at + b
    done;
    !t

  let fill_normal ?off ?len t (a : f64_array) =
    let off, n = range_of "fill_normal" (A1.dim a) off len in
    normals t n (fun i x -> A1.unsafe_set a (off + i) x)

  let fill_exponential ?off ?len t (a : f64_array) =
    let off, n = range_of "fill_exponential" (A1.dim a) off len in
    if n = 0 then t else via_floats exponentials t a off n

  (* A rejected draw retries on the fallback stream of its global draw index [g]. *)
  let fallback t p g = split (purpose t p) g

  let fill_below32 ?off ?len t ~range (a : u32_array) =
    check_range32 "fill_below32" range;
    let off, n = range_of "fill_below32" (A1.dim a) off len in
    if n = 0 then t
    else begin
      let g0 = word_at t.hi (align t.lo 32) in
      let t' = fill_u32 ~off ~len:n t a in
      if range = 0 then A1.fill (A1.sub a off n) 0l
      else begin
        let rejected = ((1 lsl 32) - range) mod range in
        for i = 0 to n - 1 do
          let x = Int32.to_int (A1.unsafe_get a (off + i)) land mask in
          let m = Int64.mul (Int64.of_int x) (Int64.of_int range) in
          let v =
            if Int64.to_int m land mask >= rejected then Int64.to_int (Int64.shift_right_logical m 32)
            else fst (below32_with (draw32 read32) (fallback t purpose_below32 (g0 + i)) range)
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
      let g0 = word_at t.hi (align t.lo 64) lsr 1 in
      let t' = fill_u64 ~off ~len:n t a in
      if Int64.equal range 0L then A1.fill (A1.sub a off n) 0L
      else begin
        let rejected = Int64.unsigned_rem (Int64.neg range) range in
        for i = 0 to n - 1 do
          let x = A1.unsafe_get a (off + i) in
          let v =
            if Int64.unsigned_compare (Int64.mul x range) rejected >= 0 then mulhi64 x range
            else fst (below64_with (draw64 read_u64) (fallback t purpose_below64 (g0 + i)) range)
          in
          A1.unsafe_set a (off + i) v
        done
      end;
      t'
    end
end

(* The C fills of tandem.c, over the generator's row cache. *)
external c_fill_u32 : Engine.ctx -> (int[@untagged]) -> u32_array -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "tandem_ml_fill_u32_byte" "tandem_ml_fill_u32" [@@noalloc]
external c_fill_u64 : Engine.ctx -> (int[@untagged]) -> u64_array -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "tandem_ml_fill_u64_byte" "tandem_ml_fill_u64" [@@noalloc]
external c_fill_f64 : Engine.ctx -> (int[@untagged]) -> f64_array -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "tandem_ml_fill_f64_byte" "tandem_ml_fill_f64" [@@noalloc]
external c_fill_normal : Engine.ctx -> (int[@untagged]) -> f64_array -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "tandem_ml_fill_normal_byte" "tandem_ml_fill_normal" [@@noalloc]
external c_fill_exponential :
  Engine.ctx -> (int[@untagged]) -> f64_array -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "tandem_ml_fill_exponential_byte" "tandem_ml_fill_exponential" [@@noalloc]
external c_fill_f64_fa :
  Engine.ctx -> (int[@untagged]) -> Float.Array.t -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "tandem_ml_fill_f64_fa_byte" "tandem_ml_fill_f64_fa" [@@noalloc]
external c_fill_normal_fa :
  Engine.ctx -> (int[@untagged]) -> Float.Array.t -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "tandem_ml_fill_normal_fa_byte" "tandem_ml_fill_normal_fa" [@@noalloc]
external c_fill_exponential_fa :
  Engine.ctx -> (int[@untagged]) -> Float.Array.t -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "tandem_ml_fill_exponential_fa_byte" "tandem_ml_fill_exponential_fa" [@@noalloc]
external c_fill_below32 :
  Engine.ctx -> (int[@untagged]) -> u32_array -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) -> unit
  = "tandem_ml_fill_below32_byte" "tandem_ml_fill_below32" [@@noalloc]
external c_fill_below64 :
  Engine.ctx -> (int[@untagged]) -> u64_array -> (int[@untagged]) -> (int[@untagged]) -> (int64[@unboxed]) -> unit
  = "tandem_ml_fill_below64_byte" "tandem_ml_fill_below64" [@@noalloc]

let fill_u32 ?off ?len t (a : u32_array) =
  let off, n = range_of "fill_u32" (A1.dim a) off len in
  fill_with t ~w:32 ~n (fun word0 -> c_fill_u32 t.ctx word0 a off n)

let fill_u64 ?off ?len t (a : u64_array) =
  let off, n = range_of "fill_u64" (A1.dim a) off len in
  fill_with t ~w:64 ~n (fun word0 -> c_fill_u64 t.ctx word0 a off n)

let fill_float ?off ?len t (a : f64_array) =
  let off, n = range_of "fill_float" (A1.dim a) off len in
  fill_with t ~w:64 ~n (fun word0 -> c_fill_f64 t.ctx word0 a off n)

let fill_normal ?off ?len t (a : f64_array) =
  let off, n = range_of "fill_normal" (A1.dim a) off len in
  fill_with t ~w:64 ~n (fun word0 -> c_fill_normal t.ctx word0 a off n)

let fill_exponential ?off ?len t (a : f64_array) =
  let off, n = range_of "fill_exponential" (A1.dim a) off len in
  if n = 0 then t else fill_with t ~w:64 ~n (fun word0 -> c_fill_exponential t.ctx word0 a off n)

(* tandem.c takes ranges below 2^32. A range of 2^32 maps every draw to itself. *)
let fill_below32 ?off ?len t ~range (a : u32_array) =
  check_range32 "fill_below32" range;
  let off, n = range_of "fill_below32" (A1.dim a) off len in
  if n = 0 then t
  else if range = 1 lsl 32 then fill_u32 ~off ~len:n t a
  else fill_with t ~w:32 ~n (fun word0 -> c_fill_below32 t.ctx word0 a off n range)

let fill_below64 ?off ?len t ~range (a : u64_array) =
  let off, n = range_of "fill_below64" (A1.dim a) off len in
  if n = 0 then t else fill_with t ~w:64 ~n (fun word0 -> c_fill_below64 t.ctx word0 a off n range)

module Float_array = struct
  let fill_float ?off ?len t a =
    let off, n = range_of "fill_float" (Float.Array.length a) off len in
    fill_with t ~w:64 ~n (fun word0 -> c_fill_f64_fa t.ctx word0 a off n)

  let fill_normal ?off ?len t a =
    let off, n = range_of "fill_normal" (Float.Array.length a) off len in
    fill_with t ~w:64 ~n (fun word0 -> c_fill_normal_fa t.ctx word0 a off n)

  let fill_exponential ?off ?len t a =
    let off, n = range_of "fill_exponential" (Float.Array.length a) off len in
    if n = 0 then t else fill_with t ~w:64 ~n (fun word0 -> c_fill_exponential_fa t.ctx word0 a off n)
end

(* ---- Specification building blocks --------------------------------------------------- *)

module Spec = struct
  let step ~o ~h =
    let st = Engine.make_state () in
    for w = 0 to 3 do
      Engine.set st (w lsl 3) o.(w);
      Engine.set st (32 + (w lsl 3)) h.(w)
    done;
    Engine.step st 0;
    (Array.init 4 (fun w -> Engine.get st (w lsl 3)), Array.init 4 (fun w -> Engine.get st (32 + (w lsl 3))))

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

  (* The limbs of the position are int64 slots of [pos], [hi] at byte 0 and [lo] at byte 8.
     OCaml 5 on arm64 assigns a mutable field by a release store, which here doubled the time
     of a draw, as each draw reads back the last one's store. A Bytes store is a plain store. *)
  type t = { ctx : Engine.ctx; pos : Bytes.t }

  let[@inline] hi s = Int64.to_int (Engine.get64 s.pos 0)
  let[@inline] lo s = Int64.to_int (Engine.get64 s.pos 8)
  let[@inline] set_hi s x = Engine.set64 s.pos 0 (Int64.of_int x)
  let[@inline] set_lo s x = Engine.set64 s.pos 8 (Int64.of_int x)

  let[@inline] set s (g : g) =
    set_hi s g.hi;
    set_lo s g.lo

  let of_generator (g : g) =
    let s = { ctx = g.ctx; pos = Bytes.create 16 } in
    set s g;
    s

  let generator s : g = { ctx = s.ctx; hi = hi s; lo = lo s }
  let make_seed ?chunk_length z = of_generator (seed ?chunk_length z)

  let make a =
    let first = if Array.length a = 0 then 0L else Int64.of_int a.(0) in
    let g = ref (seed_u128 first 0L) in
    for i = 1 to Array.length a - 1 do
      g := split_u64 !g (Int64.of_int a.(i))
    done;
    of_generator !g

  let copy s = { s with pos = Bytes.copy s.pos }

  (* The draws below move the position in place, so that they allocate nothing. A draw of at
     most 64 bits carries at most one into the high limb, and only that case stores it. *)
  let[@inline] move s alo w =
    let lo = alo + w in
    if lo <= mask then set_lo s lo
    else begin
      set_hi s (end_hi (hi s) alo w);
      set_lo s (lo land mask)
    end

  let[@inline] next32 s =
    let alo = align (lo s) 32 in
    let x = fast32 s.ctx (word_at (hi s) alo) in
    move s alo 32;
    x

  let[@inline] next64 read s =
    let alo = align (lo s) 64 in
    let x = read s.ctx (word_at (hi s) alo) in
    move s alo 64;
    x

  let[@inline] next_float s = next64 fast_f64 s
  let[@inline] bits s = next32 s lsr 2
  let[@inline] bits32 s = Int32.of_int (next32 s)
  let[@inline] bits64 s = next64 fast_u64 s

  let[@inline] bool s =
    let lo = lo s in
    let x = read_bool s.ctx (hi s) lo in
    move s lo 1;
    x

  let[@inline] float s scale = scale *. next_float s

  let[@inline] normal s = next64 fast_normal s
  let exponential s = Logarithm.exponential (next_float s)

  (* [below32] for a bound below 2^30, whose products fit in an int. *)
  let int s bound =
    if bound <= 0 || bound > 0x3fff_ffff then invalid_arg "Random.int";
    let m = ref (next32 s * bound) in
    if !m land mask < bound then begin
      let rejected = ((1 lsl 32) - bound) mod bound in
      while !m land mask < rejected do
        m := next32 s * bound
      done
    end;
    !m lsr 32

  let draw s f =
    let x, g = f (generator s) in
    set s g;
    x

  let full_int s bound =
    if bound <= 0 then invalid_arg "Random.full_int";
    draw s (fun g -> below g bound)

  let int32 s bound =
    if Int32.compare bound 0l <= 0 then invalid_arg "Random.int32";
    Int32.of_int (draw s (fun g -> below32 g (Int32.to_int bound)))

  let int64 s bound =
    if Int64.compare bound 0L <= 0 then invalid_arg "Random.int64";
    draw s (fun g -> below64 g bound)

  let split s =
    let g, kids = fork (generator s) 1 in
    set s g;
    of_generator kids.(0)
end
