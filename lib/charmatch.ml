(*
 * Copyright (c) 2019 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)


type interval = char * char

type options = { match_type: [`table | `ranges] }

$let default_options : options = { match_type = `table }

module type SET = sig
  type t
  $val cardinal : t -> int
  $val fold : 'acc. (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  $val iter : (char -> unit) -> t -> unit
  $val union : t -> t -> t
  $val empty : t
end

module Make $(C : SET) =
struct
    $let labels : 'a. (C.t * 'a) list -> int array = fun cases ->
    let n = List.length cases in
    let a = Array.make 256 n in
    List.iteri
      (fun i (cs, _) ->
        C.iter (fun c -> let k = Char.code c in if a.(k) = n then a.(k) <- i) cs)
      cases;
    a

  $let default_otherwise : 'a. unit -> 'a expr = fun () ->
    << raise (Match_failure ("charmatch-generated", 0, 0)) >>

    $let body : 'a. 'a expr array -> 'a expr option -> int -> 'a expr =
    fun bodies otherwise k ->
    if k < Array.length bodies then bodies.(k)
    else match otherwise with Some e -> e | None -> default_otherwise ()

  $let ifmem_table : 'a. char expr -> ?otherwise:'a expr -> (C.t * 'a expr) list -> 'a expr =
    fun c ?otherwise cases ->
    let n = List.length cases in
    let bodies = Array.of_list (List.map snd cases) in
    let ls = labels cases in
    let covered = Array.for_all (fun l -> l < n) ls in
    let t = String.init 256 (fun k -> Char.chr (min ls.(k) 255)) (* There may be 256 cases *) in
        let numconsts = if covered then n else n + 1 in
    if numconsts <= 1 then body bodies otherwise 0
    else
      let cases = List.init n (fun k -> (Char.chr k, bodies.(k))) in
      let fail = if covered then None else Some (body bodies otherwise n) in
      Expr.switch (module Char)
        << String.unsafe_get $(Expr.string t) (Char.code $c) >>
        ~numconsts cases fail

    $let runs : int array -> (int * int * int) array = fun ls ->
    let acc = ref [] and lo = ref 0 in
    for k = 1 to 256 do
      if k = 256 || ls.(k) <> ls.(!lo) then begin
        acc := (!lo, k - 1, ls.(!lo)) :: !acc; lo := k
      end
    done;
    Array.of_list (List.rev !acc)

  $let ifmem_ranges : 'a. char expr -> ?otherwise:'a expr -> (C.t * 'a expr) list -> 'a expr =
    fun c ?otherwise cases ->
    let n = List.length cases in
    let bodies = Array.of_list (List.map snd cases) in
    let rs = runs (labels cases) in
    if Array.length rs = 1 then (let (_, _, l) = rs.(0) in body bodies otherwise l)
    else
            let ivs = Array.make n [] in
      Array.iter (fun (lo, hi, l) ->
          if l < n then ivs.(l) <- (Char.chr lo, Char.chr hi) :: ivs.(l)) rs;
      let groups =
        List.filter_map (fun k -> if ivs.(k) = [] then None
                                  else Some (List.rev ivs.(k), bodies.(k)))
          (List.init n Fun.id) in
      let fail =
        if Array.exists (fun (_, _, l) -> l = n) rs
        then Some (body bodies otherwise n) else None in
      Expr.switch_intervals (module Char) c ~numconsts:256 groups fail

  $let ifmem : 'a. ?options:options -> char expr -> ?otherwise:'a expr -> (C.t * 'a expr) list -> 'a expr =
    fun ?options c ?otherwise l ->
    let options = match options with Some o -> o | None -> default_options in
    match options.match_type with
    | `table -> ifmem_table c ?otherwise l
    | `ranges -> ifmem_ranges c ?otherwise l
end

module Charset_set = struct
  type t = Charset.t
  $let cardinal : t -> int = fun s -> Charset.cardinal s
  $let fold : 'acc. (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc =
    fun f s acc -> Charset.fold f s acc
  $let iter : (char -> unit) -> t -> unit = fun f s -> Charset.iter f s
  $let union : t -> t -> t = fun s1 s2 -> Charset.union s1 s2
  $let empty : t = Charset.empty
end

include Make$(Charset_set)
