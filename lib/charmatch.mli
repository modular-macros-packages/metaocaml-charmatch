(*
 * Copyright (c) 2019 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type options =
  { match_type: [`table | `ranges]
    (** Whether to generate a lookup table or a chain of range
        comparisons for the character match (default [`table]) *) }

(** [{ match_type = `table }].  A macro: constants are thunked, and a
    level-0 value would be invisible to macro callers. *)
$gen default_options : unit -> options

(** The set operations the generator consumes at generation time. *)
module type SET = sig
  type t
  $gen cardinal : t -> int
  $gen fold : 'acc. (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  $gen iter : (char -> unit) -> t -> unit
  $gen union : t -> t -> t
  $gen empty : unit -> t
end

module Make [C : SET] : sig
  $gen ifmem : 'a. ?options:options -> char expr ->
    ?otherwise:'a expr -> (C.t * 'a expr) list -> 'a expr
end

(** The default instance, over the charset package. *)
module Charset_set : SET with type t = Charset.t

$gen ifmem : 'a. ?options:options -> char expr ->
  ?otherwise:'a expr -> (Charset_set.t * 'a expr) list -> 'a expr
