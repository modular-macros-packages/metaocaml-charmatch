(*
 * Copyright (c) 2019 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

type options =
  { match_type: [`table | `ranges]
    (** [`table] (the default): a 256-byte class lookup and a
        constant-time jump table on the class; [`ranges]: no table, a
        switch on the character code over each class's intervals (the
        backend chooses comparisons or a jump table by density). *) }

(** [{ match_type = `table }].  A static value: a level-0 value would
    be invisible to compile-time callers. *)
$val default_options : options

(** The set operations the generator consumes at generation time. *)
module type SET = sig
  type t
  $val cardinal : t -> int
  $val fold : 'acc. (char -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  $val iter : (char -> unit) -> t -> unit
  $val union : t -> t -> t
  $val empty : t
end

module Make $(C : SET) : sig
  $val ifmem : 'a. ?options:options -> char expr ->
    ?otherwise:'a expr -> (C.t * 'a expr) list -> 'a expr
end

(** The default instance, over the charset package. *)
module Charset_set : SET with type t = Charset.t

$val ifmem : 'a. ?options:options -> char expr ->
  ?otherwise:'a expr -> (Charset_set.t * 'a expr) list -> 'a expr
