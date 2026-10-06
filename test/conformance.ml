(* The conformance files of the specification (test/conformance, byte copies of tandem-spec
   conformance/*.json) and every behaviour of its CHECKLIST.md that this port offers: the
   bounded, normal, exponential and weighted choice fixtures through the C fills, the pure fills
   and the scalar draws, cut fills, the stream and dump hashes, and the position bounds. OCaml has
   no single precision float, so the Float32 cases and the Float32, binary16, Bool, 128-bit, Char
   and complex streams are counted and left out. *)

module A1 = Bigarray.Array1

(* ---- A case is one line of a file ---- *)

let find s pat from =
  let n = String.length s and m = String.length pat in
  let rec go i = if i + m > n then -1 else if String.sub s i m = pat then i else go (i + 1) in
  go from

let read_cases file =
  let text = In_channel.with_open_bin (Filename.concat "conformance" file) In_channel.input_all in
  List.filter
    (fun l -> find l "{\"id\":" 0 >= 0 || find l "{\"file\":" 0 >= 0)
    (String.split_on_char '\n' text)

let at line key =
  let i = find line ("\"" ^ key ^ "\": ") 0 in
  if i < 0 then None else Some (i + String.length key + 4)

let has line key = at line key <> None

let required line key =
  match at line key with Some p -> p | None -> Alcotest.failf "no field %s in %s" key line

let text line key =
  let p = required line key in
  String.sub line (p + 1) (String.index_from line (p + 1) '"' - p - 1)

let number line key =
  let p = required line key in
  let q = ref p in
  while !q < String.length line && line.[!q] >= '0' && line.[!q] <= '9' do incr q done;
  int_of_string (String.sub line p (!q - p))

(* The region of a list value, from its opening bracket to its closing one. *)
let region line key =
  let p = required line key in
  String.sub line p (String.index_from line p ']' - p)

let strings line key =
  List.filteri (fun i _ -> i land 1 = 1) (String.split_on_char '"' (region line key))

let numbers line key =
  String.split_on_char ',' (String.sub (region line key) 1 (String.length (region line key) - 1))
  |> List.filter_map (fun s -> int_of_string_opt (String.trim s))

let hex s = Int64.of_string ("0x" ^ s)

let key_of line =
  match List.map (fun w -> Int64.to_int (hex w)) (strings line "key") with
  | [ a; b; c; d ] -> [| a; b; c; d |]
  | _ -> Alcotest.fail "key"

let generator line =
  Tandem.of_key ~chunk_length:(number line "K") ~position:(Int64.of_int (number line "start")) (key_of line)

(* The (kind, n) fills of a dump. *)
let dump_draws line =
  let rec go from acc =
    let i = find line "{\"kind\": \"" from in
    if i < 0 then List.rev acc
    else begin
      let s = i + 10 in
      let q = String.index_from line s '"' in
      let p = find line "\"n\": " q + 5 in
      let e = ref p in
      while line.[!e] >= '0' && line.[!e] <= '9' do incr e done;
      go !e ((String.sub line s (q - s), int_of_string (String.sub line p (!e - p))) :: acc)
    end
  in
  go (required line "draws") []

(* ---- SHA-256 and FNV-1a ---- *)

module Sha256 = struct
  let k =
    [| 0x428a2f98; 0x71374491; 0xb5c0fbcf; 0xe9b5dba5; 0x3956c25b; 0x59f111f1; 0x923f82a4; 0xab1c5ed5;
       0xd807aa98; 0x12835b01; 0x243185be; 0x550c7dc3; 0x72be5d74; 0x80deb1fe; 0x9bdc06a7; 0xc19bf174;
       0xe49b69c1; 0xefbe4786; 0x0fc19dc6; 0x240ca1cc; 0x2de92c6f; 0x4a7484aa; 0x5cb0a9dc; 0x76f988da;
       0x983e5152; 0xa831c66d; 0xb00327c8; 0xbf597fc7; 0xc6e00bf3; 0xd5a79147; 0x06ca6351; 0x14292967;
       0x27b70a85; 0x2e1b2138; 0x4d2c6dfc; 0x53380d13; 0x650a7354; 0x766a0abb; 0x81c2c92e; 0x92722c85;
       0xa2bfe8a1; 0xa81a664b; 0xc24b8b70; 0xc76c51a3; 0xd192e819; 0xd6990624; 0xf40e3585; 0x106aa070;
       0x19a4c116; 0x1e376c08; 0x2748774c; 0x34b0bcb5; 0x391c0cb3; 0x4ed8aa4a; 0x5b9cca4f; 0x682e6ff3;
       0x748f82ee; 0x78a5636f; 0x84c87814; 0x8cc70208; 0x90befffa; 0xa4506ceb; 0xbef9a3f7; 0xc67178f2 |]

  type t = { h : int array; pending : Bytes.t; mutable fill : int; mutable total : int; w : int array }

  let create () =
    { h = [| 0x6a09e667; 0xbb67ae85; 0x3c6ef372; 0xa54ff53a; 0x510e527f; 0x9b05688c; 0x1f83d9ab; 0x5be0cd19 |];
      pending = Bytes.create 64; fill = 0; total = 0; w = Array.make 64 0 }

  let m32 = 0xffff_ffff
  let rotr x n = ((x lsr n) lor (x lsl (32 - n))) land m32

  let compress t b off =
    let w = t.w in
    for i = 0 to 15 do
      w.(i) <- Int32.to_int (Bytes.get_int32_be b (off + (4 * i))) land m32
    done;
    for i = 16 to 63 do
      let x = w.(i - 15) and y = w.(i - 2) in
      let s0 = rotr x 7 lxor rotr x 18 lxor (x lsr 3) and s1 = rotr y 17 lxor rotr y 19 lxor (y lsr 10) in
      w.(i) <- (w.(i - 16) + s0 + w.(i - 7) + s1) land m32
    done;
    let a = ref t.h.(0) and b' = ref t.h.(1) and c = ref t.h.(2) and d = ref t.h.(3) in
    let e = ref t.h.(4) and f = ref t.h.(5) and g = ref t.h.(6) and h = ref t.h.(7) in
    for i = 0 to 63 do
      let t1 =
        (!h + (rotr !e 6 lxor rotr !e 11 lxor rotr !e 25) + ((!e land !f) lxor (lnot !e land m32 land !g)) + k.(i) + w.(i))
        land m32
      in
      let t2 = (rotr !a 2 lxor rotr !a 13 lxor rotr !a 22) + ((!a land !b') lxor (!a land !c) lxor (!b' land !c)) in
      h := !g; g := !f; f := !e; e := (!d + t1) land m32;
      d := !c; c := !b'; b' := !a; a := (t1 + t2) land m32
    done;
    List.iteri (fun i x -> t.h.(i) <- (t.h.(i) + x) land m32) [ !a; !b'; !c; !d; !e; !f; !g; !h ]

  let update t b len =
    t.total <- t.total + len;
    let i = ref 0 in
    if t.fill > 0 then begin
      let take = min len (64 - t.fill) in
      Bytes.blit b 0 t.pending t.fill take;
      t.fill <- t.fill + take;
      i := take;
      if t.fill = 64 then (compress t t.pending 0; t.fill <- 0)
    end;
    while !i + 64 <= len do compress t b !i; i := !i + 64 done;
    if !i < len then begin
      Bytes.blit b !i t.pending t.fill (len - !i);
      t.fill <- t.fill + len - !i
    end

  let hexdigest t =
    let bits = t.total * 8 in
    let pad = Bytes.make (1 + ((55 - t.total) land 63) + 8) '\000' in
    Bytes.set pad 0 '\x80';
    Bytes.set_int64_be pad (Bytes.length pad - 8) (Int64.of_int bits);
    let total = t.total in
    update t pad (Bytes.length pad);
    t.total <- total;
    String.concat "" (Array.to_list (Array.map (Printf.sprintf "%08x") t.h))
end

let fnv_update h b len =
  let h = ref h in
  for i = 0 to len - 1 do
    h := Int64.mul (Int64.logxor !h (Int64.of_int (Char.code (Bytes.unsafe_get b i)))) 0x100000001b3L
  done;
  !h

let fnv0 = 0xcbf29ce484222325L

(* ---- The kinds of case ---- *)

type kind = Below32 | Below64 | Fill_below32 | Fill_below64 | Normal64 | Normal32 | Exp64 | Exp32 | Choice

let kind_of line =
  match text line "kind" with
  | "below_u32" -> Below32
  | "below_u64" -> Below64
  | "fill_below_u32" -> Fill_below32
  | "fill_below_u64" -> Fill_below64
  | "fill_normal_f64" -> Normal64
  | "fill_normal_f32" -> Normal32
  | "fill_exponential_f64" -> Exp64
  | "fill_exponential_f32" -> Exp32
  | k -> if k = "fill_choice" then Choice else Alcotest.failf "kind %s" k

let single_precision = function Normal32 | Exp32 -> true | _ -> false

type out = { u32 : Tandem.u32_array; u64 : Tandem.u64_array; f64 : Tandem.f64_array }

let make_out n =
  { u32 = A1.create Bigarray.int32 Bigarray.c_layout n; u64 = A1.create Bigarray.int64 Bigarray.c_layout n;
    f64 = A1.create Bigarray.float64 Bigarray.c_layout n }

let bits_of kind o i =
  match kind with
  | Below32 | Fill_below32 | Choice -> Int64.of_int (Int32.to_int o.u32.{i} land 0xffff_ffff)
  | Below64 | Fill_below64 -> o.u64.{i}
  | _ -> Int64.bits_of_float o.f64.{i}

let table_of line =
  Tandem.Choice.create (Array.of_list (List.map (fun w -> Int64.float_of_bits (hex w)) (strings line "weights")))

let via_float_array (fill : ?off:int -> ?len:int -> Tandem.t -> Float.Array.t -> Tandem.t) g o ~off ~len =
  let fa = Float.Array.create (off + len) in
  let g = fill ~off ~len g fa in
  for i = off to off + len - 1 do o.f64.{i} <- Float.Array.get fa i done;
  g

(* The fills of a case, each as a function of the generator and the range of the output. *)
let fills kind line =
  match kind with
  | Fill_below32 ->
      let range = Int64.to_int (hex (text line "range")) in
      [ ("fill", fun g o ~off ~len -> Tandem.fill_below32 ~off ~len g ~range o.u32);
        ("pure fill", fun g o ~off ~len -> Tandem.Pure.fill_below32 ~off ~len g ~range o.u32) ]
  | Fill_below64 ->
      let range = hex (text line "range") in
      [ ("fill", fun g o ~off ~len -> Tandem.fill_below64 ~off ~len g ~range o.u64);
        ("pure fill", fun g o ~off ~len -> Tandem.Pure.fill_below64 ~off ~len g ~range o.u64) ]
  | Normal64 ->
      [ ("fill", fun g o ~off ~len -> Tandem.fill_normal ~off ~len g o.f64);
        ("pure fill", fun g o ~off ~len -> Tandem.Pure.fill_normal ~off ~len g o.f64);
        ("Float.Array fill", fun g o ~off ~len -> via_float_array Tandem.Float_array.fill_normal g o ~off ~len);
        ("pure Float.Array fill", fun g o ~off ~len -> via_float_array Tandem.Pure.Float_array.fill_normal g o ~off ~len) ]
  | Exp64 ->
      [ ("fill", fun g o ~off ~len -> Tandem.fill_exponential ~off ~len g o.f64);
        ("pure fill", fun g o ~off ~len -> Tandem.Pure.fill_exponential ~off ~len g o.f64);
        ("Float.Array fill", fun g o ~off ~len -> via_float_array Tandem.Float_array.fill_exponential g o ~off ~len);
        ("pure Float.Array fill", fun g o ~off ~len -> via_float_array Tandem.Pure.Float_array.fill_exponential g o ~off ~len) ]
  | Choice ->
      let table = table_of line in
      [ ("fill", fun g o ~off ~len -> Tandem.fill_choice ~off ~len g table o.u32);
        ("pure fill", fun g o ~off ~len -> Tandem.Pure.fill_choice ~off ~len g table o.u32) ]
  | _ -> []

(* [n] scalar draws of the kind: the draws of the module and, for normals and exponentials, the
   stateful wrapper too. The result is the generator after them. *)
let scalars kind line =
  let loop draw g o n =
    let g = ref g in
    for i = 0 to n - 1 do g := draw !g o i done;
    !g
  in
  let state draw g o n =
    let s = Tandem.State.of_generator g in
    for i = 0 to n - 1 do o.f64.{i} <- draw s done;
    Tandem.State.generator s
  in
  match kind with
  | Below32 | Fill_below32 ->
      let range = Int64.to_int (hex (text line "range")) in
      [ ("draws", loop (fun g o i -> let x, g = Tandem.below32 g range in o.u32.{i} <- Int32.of_int x; g)) ]
  | Below64 | Fill_below64 ->
      let range = hex (text line "range") in
      [ ("draws", loop (fun g o i -> let x, g = Tandem.below64 g range in o.u64.{i} <- x; g)) ]
  | Normal64 ->
      [ ("draws", loop (fun g o i -> let x, g = Tandem.normal g in o.f64.{i} <- x; g));
        ("State", state Tandem.State.normal) ]
  | Exp64 ->
      [ ("draws", loop (fun g o i -> let x, g = Tandem.exponential g in o.f64.{i} <- x; g));
        ("State", state Tandem.State.exponential) ]
  | Choice ->
      let table = table_of line in
      [ ("draws", loop (fun g o i -> let x, g = Tandem.choice g table in o.u32.{i} <- Int32.of_int x; g)) ]
  | _ -> []

let align p w = (p + w - 1) / w * w

(* The position after a fill by the rules of Appendices A and C: an empty bounded or exponential
   fill moves nothing and any other empty fill aligns. *)
let expected_end kind p n =
  let bounded w = if n = 0 then p else align p w + (w * n) in
  match kind with
  | Fill_below32 | Exp32 -> bounded 32
  | Fill_below64 | Exp64 -> bounded 64
  | Normal32 -> if n = 0 then p else align p 32 + (64 * ((n + 1) / 2))
  | _ -> align p 64 + (64 * n)

(* The draws below the Lemire threshold of the plain fill: the elements that take the fallback. *)
let rejections kind line =
  let n = number line "n" and g = generator line in
  let range = hex (text line "range") in
  if kind = Fill_below32 then begin
    let a = A1.create Bigarray.int32 Bigarray.c_layout n in
    ignore (Tandem.fill_u32 g a);
    let r = Int64.to_int range in
    let t = ((1 lsl 32) - r) mod r in
    let c = ref 0 in
    for i = 0 to n - 1 do
      if ((Int32.to_int a.{i} land 0xffff_ffff) * r) land 0xffff_ffff < t then incr c
    done;
    !c
  end
  else begin
    let a = A1.create Bigarray.int64 Bigarray.c_layout n in
    ignore (Tandem.fill_u64 g a);
    let t = Int64.unsigned_rem (Int64.neg range) range in
    let c = ref 0 in
    for i = 0 to n - 1 do
      if Int64.unsigned_compare (Int64.mul a.{i} range) t < 0 then incr c
    done;
    !c
  end

let pos = Alcotest.int64
let want_bits line = Array.of_list (List.map hex (strings line "values"))

(* One case: every fill, the scalar draws, and every fill cut at elements 1, 7, 20, 21 and n - 1
   and run in order on one generator. Returns whether the case takes the fallback. *)
let check_case line =
  let kind = kind_of line and id = text line "id" and n = number line "n" and p = number line "start" in
  let want = want_bits line in
  Alcotest.(check int) (id ^ ": values") n (Array.length want);
  let scalar_only = kind = Below32 || kind = Below64 in
  (* A scalar bounded draw retries on its own stream, so its end is the file's. *)
  let stop = if scalar_only then number line "end" else expected_end kind p n in
  if has line "end" then Alcotest.(check int) (id ^ ": end") stop (number line "end");
  let is_below = kind = Fill_below32 || kind = Fill_below64 in
  let rejected = if is_below && n > 0 then rejections kind line else 0 in
  if has line "rejected" then Alcotest.(check int) (id ^ ": rejections") rejected (number line "rejected");
  if kind = Choice then begin
    let t = table_of line in
    Alcotest.(check int64) (id ^ ": capacity") (hex (text line "capacity")) (Tandem.Choice.capacity t);
    if has line "cut" then begin
      let cut = strings line "cut" and alias = strings line "alias" in
      Alcotest.(check int) (id ^ ": size") (List.length cut) (Tandem.Choice.size t);
      List.iteri (fun j c -> Alcotest.(check int64) (Printf.sprintf "%s: cut %d" id j) (hex c) (Tandem.Choice.cut t j)) cut;
      List.iteri (fun j a -> Alcotest.(check int) (Printf.sprintf "%s: alias %d" id j) (Int64.to_int (hex a)) (Tandem.Choice.alias t j)) alias
    end
  end;
  let check name o g =
    for i = 0 to n - 1 do
      if bits_of kind o i <> want.(i) then Alcotest.failf "%s: %s differs at element %d" id name i
    done;
    Alcotest.check pos (id ^ ": " ^ name ^ " end") (Int64.of_int stop) (Tandem.position g)
  in
  (* A fill is the scalar draws, except that a rejected bounded draw retries elsewhere. *)
  if scalar_only || (n > 0 && rejected = 0) then
    List.iter
      (fun (name, draw) ->
        let o = make_out n in
        check name o (draw (generator line) o n))
      (scalars kind line);
  if not scalar_only then
    List.iter
      (fun (name, fill) ->
        let o = make_out n in
        check name o (fill (generator line) o ~off:0 ~len:n);
        (* A Float32 normal cut at an odd element would drop a sin half, but this port has none. *)
        List.iter
          (fun k ->
            if k > 0 && k < n then begin
              let o = make_out n in
              let g = fill (generator line) o ~off:0 ~len:k in
              check (Printf.sprintf "%s cut at %d" name k) o (fill g o ~off:k ~len:(n - k))
            end)
          [ 1; 7; 20; 21; n - 1 ])
      (fills kind line);
  rejected > 0

let cases file count ~float32 () =
  let lines = read_cases file in
  Alcotest.(check int) (file ^ ": cases") count (List.length lines);
  Alcotest.(check int) (file ^ ": Float32 cases") float32 (List.length (List.filter (fun l -> single_precision (kind_of l)) lines));
  let rejecting = List.map (fun l -> if single_precision (kind_of l) then false else check_case l) lines in
  if file = "fill_below.json" then Alcotest.(check bool) "some case takes the fallback" true (List.mem true rejecting)

(* ---- Rules of the appendices ---- *)

let find_case lines name =
  match List.filter (fun l -> let id = text l "id" in
                              let t = " " ^ name in
                              String.length id >= String.length t
                              && String.sub id (String.length id - String.length t) (String.length t) = t) lines with
  | [ l ] -> l
  | _ -> Alcotest.failf "no case %s" name

let values lines name = Array.to_list (want_bits (find_case lines name))
let drop n l = List.filteri (fun i _ -> i >= n) l
let take n l = List.filteri (fun i _ -> i < n) l

(* Element i of a fill from start 1 is element i + 1 of the fill from start 0, so the fallback of a
   rejected draw is keyed by its index in the stream. *)
let fallback_index () =
  let fb = read_cases "fill_below.json" in
  List.iter
    (fun (a, b) -> Alcotest.(check (list int64)) b (drop 1 (values fb a)) (take 63 (values fb b)))
    [ ("CROSS_BELOW32[4]", "CROSS_BELOW32_AT[4]"); ("CROSS_BELOW64[6]", "CROSS_BELOW64_AT[6]") ];
  let nm = read_cases "normal.json" in
  Alcotest.(check (list int64)) "normal" (drop 1 (values nm "CROSS_NORMAL[0]")) (take 63 (values nm "CROSS_NORMAL[1]"));
  (* Element 20 of CROSS_NORMAL[3] to [5] misses, so their cut fills take the fallback at and after
     the cut. *)
  for i = 3 to 5 do
    Alcotest.(check int) "n" 64 (number (find_case nm (Printf.sprintf "CROSS_NORMAL[%d]" i)) "n")
  done

let key42 = [| 0x421d21eb; 0x32d31777; 0x62e7564b; 0xdf2bdf82 |]

(* The bounded draws name their width: below32 and below64, and below picks it from the range.
   Range 1000 gives CROSS_BELOW32[3] on 32-bit draws and CROSS_BELOW64[3] on 64-bit draws, and
   range 0 returns 0 after one draw of that width. *)
let width_from_range () =
  let fb = read_cases "fill_below.json" in
  let w32 = values fb "CROSS_BELOW32[3]" and w64 = values fb "CROSS_BELOW64[3]" in
  let g = Tandem.of_key key42 in
  let o = make_out 64 in
  ignore (Tandem.fill_below32 g ~range:1000 o.u32);
  Alcotest.(check (list int64)) "u32" w32 (List.init 64 (fun i -> bits_of Fill_below32 o i));
  ignore (Tandem.fill_below64 g ~range:1000L o.u64);
  Alcotest.(check (list int64)) "u64" w64 (List.init 64 (fun i -> bits_of Fill_below64 o i));
  Alcotest.(check bool) "the widths differ" true (w32 <> w64);
  let g = ref g and via_below = ref [] in
  for _ = 1 to 64 do
    let x, g' = Tandem.below !g 1000 in
    g := g';
    via_below := Int64.of_int x :: !via_below
  done;
  Alcotest.(check (list int64)) "below" w32 (List.rev !via_below);
  List.iter
    (fun p ->
      let g = Tandem.of_key ~position:(Int64.of_int p) key42 in
      let x, g32 = Tandem.below32 g 0 and y, g64 = Tandem.below64 g 0L in
      Alcotest.(check int) "below32 0" 0 x;
      Alcotest.check pos "one 32-bit draw" (Int64.of_int (align p 32 + 32)) (Tandem.position g32);
      Alcotest.(check int64) "below64 0" 0L y;
      Alcotest.check pos "one 64-bit draw" (Int64.of_int (align p 64 + 64)) (Tandem.position g64))
    [ 0; 1; 33 ]

(* The seven cases with n = 0 start at 33. A uniform, Float64 normal or choice fill aligns, and a
   bounded or exponential fill leaves the position. [cases] checks each end. *)
let empty_fills () =
  let all = List.concat_map read_cases [ "fill_below.json"; "normal.json"; "exponential.json"; "choice.json" ] in
  let empty = List.filter (fun l -> number l "n" = 0) all in
  Alcotest.(check int) "empty cases" 7 (List.length empty);
  List.iter (fun l -> Alcotest.(check int) "start" 33 (number l "start")) empty;
  let g = Tandem.of_key ~position:33L key42 in
  Alcotest.check pos "u32 aligns" 64L (Tandem.position (Tandem.fill_u32 g (A1.create Bigarray.int32 Bigarray.c_layout 0)))

(* ---- Weighted choice ---- *)

let weighted_choice () =
  let cs = read_cases "choice.json" in
  Alcotest.(check int) "tables" 5 (List.length (List.filter (fun l -> has l "cut") cs));
  let a = values cs "CROSS_CHOICE[0]" and s = values cs "CROSS_CHOICE[1]" in
  Alcotest.(check (pair int int)) "starts" (0, 1)
    (number (find_case cs "CROSS_CHOICE[0]") "start", number (find_case cs "CROSS_CHOICE[1]") "start");
  Alcotest.(check (list int64)) "shifted" (drop 1 a) (take (List.length a - 1) s);
  (* m = 1 returns 0 and still consumes 64 bits. *)
  let i, g = Tandem.choice (Tandem.of_key ~position:5L key42) (Tandem.Choice.create [| 0.25 |]) in
  Alcotest.(check int) "m = 1" 0 i;
  Alcotest.check pos "64 bits" 128L (Tandem.position g);
  (* A zero weight, written as -0. too, never appears. *)
  let t = Tandem.Choice.create [| -0.; 3.; 0.; 1. |] in
  let out = A1.create Bigarray.int32 Bigarray.c_layout 5000 in
  ignore (Tandem.fill_choice (Tandem.seed 9) t out);
  for i = 0 to 4999 do
    let v = Int32.to_int out.{i} in
    if v <> 1 && v <> 3 then Alcotest.failf "index %d at %d" v i
  done

let rejects_invalid_weights () =
  List.iter
    (fun w ->
      Alcotest.(check bool) "rejected" true
        (match Tandem.Choice.create w with _ -> false | exception Invalid_argument _ -> true))
    [ [||]; [| 1.; -1. |]; [| 1.; Float.nan |]; [| 1.; Float.infinity |]; [| 1.; Float.neg_infinity |]; [| 0.; -0. |]; [| 0. |] ];
  ignore (Tandem.Choice.create [| 5e-324 |])

(* Chi-square of 10^6 draws against nine positive weights, 8 degrees of freedom: the 0.0005 and
   0.9995 quantiles are 0.71 and 27.87. The zero weight never appears. *)
let choice_law () =
  let w = [| 0.5; 3.; 0.; 1.; 7.; 2.25; 0.1; 4.; 1.; 6. |] and n = 1_000_000 in
  let out = A1.create Bigarray.int32 Bigarray.c_layout n in
  ignore (Tandem.fill_choice (Tandem.seed 2028) (Tandem.Choice.create w) out);
  let counts = Array.make (Array.length w) 0 in
  for i = 0 to n - 1 do
    let v = Int32.to_int out.{i} in
    counts.(v) <- counts.(v) + 1
  done;
  Alcotest.(check int) "zero weight" 0 counts.(2);
  let total = Array.fold_left ( +. ) 0. w in
  let chi2 = ref 0. in
  Array.iteri
    (fun i wi ->
      if wi > 0. then begin
        let e = float_of_int n *. wi /. total in
        chi2 := !chi2 +. ((float_of_int counts.(i) -. e) ** 2. /. e)
      end)
    w;
  if !chi2 < 0.71 || !chi2 > 27.87 then Alcotest.failf "chi-square %g" !chi2

(* ---- Hashes ---- *)

let le_bytes64 n f = let b = Bytes.create (8 * n) in for i = 0 to n - 1 do Bytes.set_int64_le b (8 * i) (f i) done; b
let le_bytes32 n f = let b = Bytes.create (4 * n) in for i = 0 to n - 1 do Bytes.set_int32_le b (4 * i) (f i) done; b

(* The bytes of a uniform stream, little endian. A complex element is its real and imaginary
   parts, so a complex fill is the real fill of twice the length. *)
let stream_bytes ty n g =
  let u32 n = let a = A1.create Bigarray.int32 Bigarray.c_layout n in ignore (Tandem.fill_u32 g a); a in
  let f64 n = let a = A1.create Bigarray.float64 Bigarray.c_layout n in ignore (Tandem.fill_float g a); a in
  match ty with
  | "UInt32" -> let a = u32 n in le_bytes32 n (fun i -> a.{i})
  | "UInt64" ->
      let a = A1.create Bigarray.int64 Bigarray.c_layout n in
      ignore (Tandem.fill_u64 g a);
      le_bytes64 n (fun i -> a.{i})
  | "Float64" -> let a = f64 n in le_bytes64 n (fun i -> Int64.bits_of_float a.{i})
  | "ComplexF64" -> let a = f64 (2 * n) in le_bytes64 (2 * n) (fun i -> Int64.bits_of_float a.{i})
  | _ -> Alcotest.failf "unsupported type %s" ty

let supported = [ "UInt32"; "UInt64"; "Float64"; "ComplexF64" ]

(* The SHA-256 of every uniform stream of hashes.json that this port draws, from the key and K of
   its entry. They cross 128-bit blocks, 1024-bit rows and chunks, and k1234_K8_u32 crosses a chunk
   every 8 rows. *)
let stream_hashes () =
  let streams = List.filter (fun l -> has l "file") (read_cases "hashes.json") in
  Alcotest.(check int) "streams" 12 (List.length streams);
  let seen = ref 0 in
  List.iter
    (fun l ->
      if List.mem (text l "type") supported then begin
        incr seen;
        let b = stream_bytes (text l "type") (number l "n") (generator l) in
        Alcotest.(check int) (text l "file" ^ ": bytes") (number l "bytes") (Bytes.length b);
        let h = Sha256.create () in
        Sha256.update h b (Bytes.length b);
        Alcotest.(check string) (text l "file") (text l "sha256") (Sha256.hexdigest h)
      end)
    streams;
  Alcotest.(check int) "streams drawn" 5 !seen

(* The FNV-1a and SHA-256 of the long dumps of Float64 normals: for each start a generator from the
   key and K runs the fill, through the C fill and the pure fill. The dumps with Float32 parts are
   left out. *)
let dump_hashes fill () =
  let dumps = List.filter (fun l -> not (has l "file")) (read_cases "hashes.json") in
  Alcotest.(check int) "dumps" 5 (List.length dumps);
  let seen = ref 0 in
  List.iter
    (fun l ->
      match dump_draws l with
      | [ ("fill_normal_f64", n) ] ->
          incr seen;
          let id = text l "id" and a = A1.create Bigarray.float64 Bigarray.c_layout n in
          let fnv = ref fnv0 and sha = Sha256.create () and last = ref 0L in
          List.iter
            (fun start ->
              let g = fill (Tandem.of_key ~chunk_length:(number l "K") ~position:(Int64.of_int start) (key_of l)) a in
              let b = le_bytes64 n (fun i -> Int64.bits_of_float a.{i}) in
              fnv := fnv_update !fnv b (Bytes.length b);
              Sha256.update sha b (Bytes.length b);
              last := Tandem.position g)
            (numbers l "starts");
          Alcotest.(check int64) (id ^ ": FNV-1a") (hex (text l "fnv1a")) !fnv;
          if has l "sha256" then Alcotest.(check string) (id ^ ": SHA-256") (text l "sha256") (Sha256.hexdigest sha);
          if has l "end" then Alcotest.check pos (id ^ ": end") (Int64.of_int (number l "end")) !last
      | _ -> ())
    dumps;
  Alcotest.(check int) "dumps drawn" 3 !seen

(* ---- Positions ---- *)

(* A draw at any position equals the sequential fill, across block, row and chunk boundaries: 2048
   words at K = 8 cross two chunks. *)
let random_access () =
  List.iter
    (fun k ->
      let g = Tandem.of_key ~chunk_length:k key42 in
      let w32 = A1.create Bigarray.int32 Bigarray.c_layout 2048 and w64 = A1.create Bigarray.int64 Bigarray.c_layout 1024 in
      ignore (Tandem.fill_u32 g w32);
      ignore (Tandem.fill_u64 g w64);
      for i = 0 to 2047 do
        let x, _ = Tandem.u32 (Tandem.seek g (Int64.of_int (32 * i))) in
        Alcotest.(check int) "u32" (Int32.to_int w32.{i} land 0xffff_ffff) x
      done;
      for i = 0 to 1023 do
        let x, _ = Tandem.u64 (Tandem.seek g (Int64.of_int (64 * i))) in
        Alcotest.(check int64) "u64" w64.{i} x
      done)
    [ 8; 32 ]

(* A start of 2^63 - 1 is accepted, and 2^63 and 2^64 - 1 are rejected, and as a generator is a
   value, a rejected start changes nothing. A 64-bit draw at 2^63 - 1 aligns to 2^63. A fill
   reaches 2^64 only with 2^55 elements or more, which no array holds, so the check in the fill
   has no input that the tests can build. *)
let position_bounds () =
  let top = Int64.max_int in
  let g = Tandem.of_key ~position:top key42 in
  Alcotest.check pos "2^63 - 1" top (Tandem.position g);
  let raises f = match f () with _ -> false | exception Invalid_argument _ -> true in
  Alcotest.(check bool) "seek 2^63" true (raises (fun () -> Tandem.seek g Int64.min_int));
  Alcotest.(check bool) "seek 2^64 - 1" true (raises (fun () -> Tandem.seek g (-1L)));
  Alcotest.(check bool) "of_key 2^63" true (raises (fun () -> Tandem.of_key ~position:Int64.min_int key42));
  Alcotest.(check bool) "of_key 2^64 - 1" true (raises (fun () -> Tandem.of_key ~position:(-1L) key42));
  Alcotest.check pos "unchanged" top (Tandem.position g);
  let _, g' = Tandem.u64 g in
  Alcotest.check pos "aligns to 2^63" (Int64.add Int64.min_int 64L) (Tandem.position g')

let () =
  Alcotest.run "conformance"
    [ ( "cases",
        [ Alcotest.test_case "below.json" `Quick (cases "below.json" 11 ~float32:0);
          Alcotest.test_case "fill_below.json" `Quick (cases "fill_below.json" 77 ~float32:0);
          Alcotest.test_case "normal.json" `Quick (cases "normal.json" 20 ~float32:7);
          Alcotest.test_case "exponential.json" `Quick (cases "exponential.json" 12 ~float32:6);
          Alcotest.test_case "choice.json" `Quick (cases "choice.json" 24 ~float32:0) ] );
      ( "checklist",
        [ Alcotest.test_case "fallback by global draw index" `Quick fallback_index;
          Alcotest.test_case "width from range" `Quick width_from_range;
          Alcotest.test_case "empty fills" `Quick empty_fills;
          Alcotest.test_case "weighted choice" `Quick weighted_choice;
          Alcotest.test_case "weighted choice rejects invalid weights" `Quick rejects_invalid_weights;
          Alcotest.test_case "weighted choice follows its weights" `Quick choice_law;
          Alcotest.test_case "stream hashes" `Quick stream_hashes;
          Alcotest.test_case "dump hashes" `Slow (dump_hashes (fun g a -> Tandem.fill_normal g a));
          Alcotest.test_case "pure dump hashes" `Slow (dump_hashes (fun g a -> Tandem.Pure.fill_normal g a));
          Alcotest.test_case "random access equals the sequential fill" `Quick random_access;
          Alcotest.test_case "position bounds" `Quick position_bounds ] ) ]
