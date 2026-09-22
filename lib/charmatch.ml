(*
 * Copyright (c) 2019 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)


type interval = char * char

type options = { match_type: [`table | `ranges] }

$gen default_options : unit -> options = fun () -> { match_type = `table }

module type SET = sig
  type t
  $gen cardinal : t -> int
  $gen fold : 'acc. (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  $gen iter : (char -> unit) -> t -> unit
  $gen union : t -> t -> t
  $gen empty : unit -> t
end

module Make [C : SET] =
struct
  $gen intervals : C.t -> interval list = fun cs ->
    let add ch = function
      | (l, h) :: tail when Char.code ch = succ (Char.code h) -> (l, ch) :: tail
      | l -> (ch, ch) :: l
    in
    List.rev (C.fold add cs [])

  $gen covering : 'a. (C.t * 'a) list -> bool = fun cases ->
    C.cardinal
      (List.fold_left (fun s (cs, _) -> C.union s cs) (C.empty ()) cases)
    = 256

  $gen table : 'a. (C.t * 'a) list -> string = fun cases ->
    let n = List.length cases in
    let b = Bytes.make 256 (Char.chr (min n 255)) (* There may be 256 cases *) in
    List.iteri
      (fun i (cs, _) -> C.iter (fun c -> Bytes.set b (Char.code c) (Char.chr i)) cs)
      cases;
    Bytes.unsafe_to_string b

  $gen default_otherwise : 'a. unit -> 'a expr = fun () ->
    << raise (Match_failure ("charmatch-generated", 0, 0)) >>

    $gen cond : interval list -> char expr -> bool expr = fun is x ->
    let one (l, h) =
      if l = h then << $x = $(Expr.char l) >>
      else << $(Expr.char l) <= $x && $x <= $(Expr.char h) >>
    in
    match is with
    | [] -> failwith "empty interval list not supported"
    | i :: is -> List.fold_left (fun acc i -> << $acc || $(one i) >>) (one i) is

  $gen ifmem_ranges : 'a. char expr -> ?otherwise:'a expr -> (C.t * 'a expr) list -> 'a expr =
    fun c ?otherwise cases ->
    let tail () = match otherwise with
      | Some e -> e
      | None -> default_otherwise ()
    in
    let rec build x = function
      | [] -> tail ()
      | (cs, rhs) :: rest ->
        << if $(cond (intervals cs) x) then $rhs else $(build x rest) >>
    in
        << let x = $c in $(build <<x>> cases) >>

  $gen ifmem_table : 'a. char expr -> ?otherwise:'a expr -> (C.t * 'a expr) list -> 'a expr =
    fun c ?otherwise cases ->
    let len = List.length cases in
    let covered = covering cases in
    let t = table cases in
    let rec build i k = function
      | [] -> (match otherwise with
               | Some e -> e
               | None -> default_otherwise ())
      | [ (_, rhs) ] when k + 1 = len && covered -> rhs
      | (_, rhs) :: rest ->
        << if $i = $(Expr.int k) then $rhs else $(build i (k + 1) rest) >>
    in
    << let i = Char.code (String.unsafe_get $(Expr.string t) (Char.code $c)) in
       $(build <<i>> 0 cases) >>

  $gen ifmem : 'a. ?options:options -> char expr -> ?otherwise:'a expr -> (C.t * 'a expr) list -> 'a expr =
    fun ?options c ?otherwise l ->
    let options = match options with Some o -> o | None -> default_options () in
    match options.match_type with
    | `table -> ifmem_table c ?otherwise l
    | `ranges -> ifmem_ranges c ?otherwise l
end

module Charset_set = struct
  type t = Charset.t
  $gen cardinal : t -> int = fun s -> Charset.cardinal s
  $gen fold : 'acc. (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc =
    fun f s acc -> Charset.fold f s acc
  $gen iter : (char -> unit) -> t -> unit = fun f s -> Charset.iter f s
  $gen union : t -> t -> t = fun s1 s2 -> Charset.union s1 s2
  $gen empty : unit -> t = fun () -> Charset.empty
end

include Make[Charset_set]
