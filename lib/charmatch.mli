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
macro default_options : unit -> options

(** The set operations the generator consumes at generation time. *)
module type SET = sig
  type t
  macro cardinal : t -> int
  macro fold : 'acc. (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  macro iter : (char -> unit) -> t -> unit
  macro union : t -> t -> t
  macro empty : unit -> t
end

module Make [C : SET] : sig
  macro ifmem : 'a. ?options:options -> char expr ->
    ?otherwise:'a expr -> (C.t * 'a expr) list -> 'a expr
end

(** The default instance, over the charset package. *)
module Charset_set : SET with type t = Charset.t

macro ifmem : 'a. ?options:options -> char expr ->
  ?otherwise:'a expr -> (Charset_set.t * 'a expr) list -> 'a expr
