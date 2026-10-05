(** Tandem8x32: a noncryptographic pseudorandom number generator, fast on CPUs and GPUs alike.

    This library implements the {{:https://github.com/tandem-rng/spec}specification} and produces
    the stream it defines, bit for bit.

    A generator {!t} is a value: its transport form (a 128-bit key, a 64-bit bit position and the
    chunk length [K]). Every draw returns the value and the successor generator, so the same
    generator gives the same draw again.

    A generator carries a cache of its current row that its copies share. The cache changes no
    value, but it is not safe to use copies of one generator from two domains at once. Give each
    domain its own generator through {!split}, {!fork} or {!purpose}.

    The bounded draws, normals and exponentials of Appendix A are not part of the specification.
    They follow its conventions, so that every port returns the same values.

    OCaml has no single precision float, so the library has no [Float32] draws. *)

(** {1 Generators} *)

type t

val default_chunk_length : int
(** [32]. *)

val seed : ?chunk_length:int -> int -> t
(** [seed z] whitens the non-negative seed [z] into a key, as the specification does, and starts
    at position 0.

    @raise Invalid_argument if [z] is negative or [chunk_length] is not a power of two in 1 to
    65536. *)

val seed_u128 : ?chunk_length:int -> int64 -> int64 -> t
(** [seed_u128 lo hi] seeds from the 128-bit integer [hi * 2^64 + lo], both halves unsigned. *)

val of_key : ?chunk_length:int -> ?position:int64 -> int array -> t
(** A generator from its transport form: four key words in [0, 2^32) and a start position below
    2^63 (default 0).

    @raise Invalid_argument on a key or position out of range. *)

val key : t -> int array
(** The four key words. *)

val chunk_length : t -> int

val position : t -> int64
(** The stream bit position of the next draw before alignment, as an unsigned 64-bit integer. *)

val seek : t -> int64 -> t
(** [seek t p] moves to bit position [p], which costs the same at any distance.

    @raise Invalid_argument if [p] is not below 2^63. *)

val equal : t -> t -> bool
(** Equal transport forms. *)

(** {1 Derived generators} *)

val split : t -> int -> t
(** [split t i] is child [i] by key alone, at position 0. It does not depend on the position of
    [t].

    @raise Invalid_argument if [i] is negative. *)

val split_u64 : t -> int64 -> t
(** [split] for an index in the full unsigned 64-bit range. *)

val purpose : t -> int -> t
(** [purpose t u] is the child for the named purpose [u], by key alone, at position 0. The values
    [0x424c573332] and [0x424c573634] are reserved for bounded fills, [0x4e524d3634] for
    normals. *)

val purpose_u64 : t -> int64 -> t

val fork : t -> int -> t * t array
(** [fork t n] derives [n] children from the block that holds the position of [t] and returns [t]
    moved to the start of the next block, also for [n = 0]. *)

(** {1 Scalar draws}

    Each draw aligns the position to the width of the value, reads, and advances past it.

    @raise Invalid_argument when the stream would pass bit 2^64. *)

val bool : t -> bool * t
val u32 : t -> int * t
val u64 : t -> int64 * t

val float : t -> float * t
(** A uniform draw in \[0, 1) with 53 bits, from a 64-bit draw. *)

(** {2 Bounded integers}

    Lemire's multiply-and-reject method on the uniform draws, a rejected draw being discarded.
    A range of 0 gives 0 after one draw. *)

val below32 : t -> int -> int * t
(** On 32-bit draws, for a range up to 2^32. *)

val below64 : t -> int64 -> int64 * t
(** On 64-bit draws, for an unsigned range. *)

val below : t -> int -> int * t
(** In \[0, range), on 32-bit draws when [range <= 2^32] and on 64-bit draws otherwise. *)

val between : t -> lo:int -> hi:int -> int * t
(** In \[lo, hi), with the draw width chosen from [hi - lo] as {!below} does. *)

(** {2 Normals and exponentials}

    Normals follow the 1024-layer ziggurat on one 64-bit draw, exponentials [-ln (1 - u)] on one
    uniform. The values equal tandem-c bit for bit.

    A draw outside the inner rectangles of the ziggurat, 0.43 % of them, continues on the
    fallback stream [split (purpose t0 0x4e524d3634) g], where [t0] has the key of the generator
    and [g] is the global index of the draw: the aligned position over 64. The fallback draws
    never move the generator. *)

val normal : t -> float * t
(** From one 64-bit draw. It equals element 0 of {!fill_normal}. *)

val exponential : t -> float * t

(** {1 Fills}

    A fill writes [len] elements from [off] (the whole array by default) and returns the generator
    after it. A fill equals the same number of scalar draws, so a fill cut at any element
    equals the whole fill.

    A plain or normal fill of 0 elements returns the position aligned to the element width. The
    bounded and exponential fills of 0 elements move nothing. A normal fill consumes [len]
    64-bit draws, element [i] from draw [i].

    @raise Invalid_argument if the range lies outside the array or the fill would pass bit 2^64. *)

type u32_array = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
type u64_array = (int64, Bigarray.int64_elt, Bigarray.c_layout) Bigarray.Array1.t
type f64_array = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

module type Fills = sig
  val fill_u32 : ?off:int -> ?len:int -> t -> u32_array -> t
  val fill_u64 : ?off:int -> ?len:int -> t -> u64_array -> t
  val fill_float : ?off:int -> ?len:int -> t -> f64_array -> t
  val fill_normal : ?off:int -> ?len:int -> t -> f64_array -> t
  val fill_exponential : ?off:int -> ?len:int -> t -> f64_array -> t

  val fill_below32 : ?off:int -> ?len:int -> t -> range:int -> u32_array -> t
  (** Element [i] takes draw [i] of the 32-bit fill, which has the global draw index [g]: the
      aligned start position over 32, plus [i]. A rejected draw retries on the 32-bit draws of
      [split (purpose t 0x424c573332) g]. The fill consumes exactly [len] draws. *)

  val fill_below64 : ?off:int -> ?len:int -> t -> range:int64 -> u64_array -> t
  (** As {!fill_below32} on 64-bit draws with the purpose [0x424c573634]. *)

  (** Fills into a [Float.Array], with the values of the bigarray fills of the same name. *)
  module Float_array : sig
    val fill_float : ?off:int -> ?len:int -> t -> Float.Array.t -> t
    val fill_normal : ?off:int -> ?len:int -> t -> Float.Array.t -> t
    val fill_exponential : ?off:int -> ?len:int -> t -> Float.Array.t -> t
  end
end

include Fills
(** The fills run tandem.c, vendored and compiled with the library. *)

module Pure : Fills
(** The fills in OCaml alone, ten to fifteen times slower. They are the reference that the C
    fills must equal bit for bit. *)

(** {1 Stateful wrapper}

    The shape of [Random.State], on a mutable position. [bits], [bits32], [bits64], [bool],
    [int] and [float] allocate nothing. A scalar
    normal or exponential draw keeps nothing between calls, so repeated calls equal the fills.
    A state shares the row cache of the generator it came from. *)
module State : sig
  type g := t
  type t

  val of_generator : g -> t
  val generator : t -> g
  val make_seed : ?chunk_length:int -> int -> t

  val make : int array -> t
  (** [make [|z|]] is {!seed} [z], and each further integer [i] takes [split i] of the generator
      so far. An empty array acts as [[|0|]]. A negative integer counts modulo 2^64. *)

  val copy : t -> t
  val bits : t -> int
  (** 30 random bits. *)

  val bits32 : t -> int32
  val bits64 : t -> int64
  val bool : t -> bool
  val int : t -> int -> int
  (** Below a bound in 1 to [2^30 - 1]. *)

  val full_int : t -> int -> int
  val int32 : t -> int32 -> int32
  val int64 : t -> int64 -> int64
  val float : t -> float -> float
  val normal : t -> float
  val exponential : t -> float

  val split : t -> t
  (** A child from {!fork} of one. *)
end

(** {1 Specification building blocks}

    The step, the seeding function and the blocks of the stream, for conformance tests. *)
module Spec : sig
  val step : o:int array -> h:int array -> int array * int array
  (** The step T. *)

  val f_keyed : int array -> counter:int64 -> domain:int -> aux:int -> int array * int array
  (** [F(key, counter, domain, aux)], as the exposed and the hidden half. *)

  val block : int array -> chunk:int -> j:int -> int array
  (** Block [B(chunk, j)]. *)
end
