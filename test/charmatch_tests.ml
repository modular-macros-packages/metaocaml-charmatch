(*
 * Copyright (c) 2022 Jeremy Yallop
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)


open OUnit2


macro split_at : 'a. int -> 'a list -> 'a list * 'a list = fun n l ->
  let rec go n l =
    match n, l with
    | 0, _ -> [], l
    | _, [] -> invalid_arg "split_at"
    | n, x :: xs -> let xs', ys = go (pred n) xs in x :: xs', ys
  in go n l

macro shuffle : 'a. 'a array -> unit = fun arr ->
  for i = pred (Array.length arr) downto 1 do
    let j = Random.int i in
    let ai, aj = arr.(i), arr.(j) in
    arr.(i) <- aj; arr.(j) <- ai
  done

macro all_chars : unit -> char array = fun () -> Array.init 256 Char.chr

macro randomized_items : 'a. 'a array -> 'a list = fun arr ->
  let arr = Array.copy arr in shuffle arr; Array.to_list arr

macro random_covering_partition : char array -> int -> Charset.t list =
  fun arr n ->
  let rec loop elems nelems parts n =
    if n < 1 then assert false
    else if n = 1 then Charset.of_list elems :: parts
    else let size = succ (Random.int (nelems - n + 1)) in
         let part, elems' = split_at size elems in
         loop elems' (nelems - size) (Charset.of_list part :: parts) (n - 1)
  in loop (randomized_items arr) (Array.length arr) [] n

macro random_non_covering_partition : char array -> int -> Charset.t list =
  fun arr n ->
  let arr = Array.copy arr in
  shuffle arr;
  let bound = n + Random.int (Array.length arr - n) in
  random_covering_partition (Array.sub arr 0 bound) n

macro ground_truth : Charset.t list -> string = fun partition ->
  let b = Bytes.make 256 '\255' in
  List.iteri
    (fun i cs -> Charset.iter (fun c -> Bytes.set b (Char.code c) (Char.chr i)) cs)
    partition;
  Bytes.unsafe_to_string b

macro battery :
  seed:int -> covering:bool -> options:Charmatch.options -> int list ->
  ((char -> int) * string) list expr =
  fun ~seed ~covering ~options ns ->
  Random.init seed;
  let mk n =
    let partition =
      if covering then random_covering_partition (all_chars ()) n
      else random_non_covering_partition (all_chars ()) n
    in
    let cases = List.mapi (fun i p -> (p, Expr.int i)) partition in
    let ground = ground_truth partition in
    let matcher =
      if covering
      then << fun c -> $(Charmatch.ifmem ~options <<c>> cases) >>
      else << fun c -> $(Charmatch.ifmem ~options <<c>> ~otherwise:<< -1 >> cases) >>
    in
    << ($matcher, $(Expr.string ground)) >>
  in
  List.fold_right (fun n acc -> << $(mk n) :: $acc >>) ns << [] >>

let check_battery ~covering name tests =
  List.iteri
    (fun j (f, ground) ->
       for i = 0 to 255 do
         let k = Char.code ground.[i] in
         let expected = if not covering && k = 255 then -1 else k in
         assert_equal ~printer:string_of_int
           ~msg:(Printf.sprintf "%s: matcher %d, char %d" name j i)
           expected (f (Char.chr i))
       done)
    tests


let test_covering_table _ =
  check_battery ~covering:true "covering/table"
    $( battery ~seed:0x0C0FFEE ~covering:true ~options:{ match_type = `table }
         [1; 2; 3; 4; 5; 8; 13; 32; 64; 101; 256] )

let test_covering_match _ =
  check_battery ~covering:true "covering/ranges"
    $( battery ~seed:0x1C0FFEE ~covering:true ~options:{ match_type = `ranges }
         [1; 2; 3; 4; 5; 8; 13; 32; 64; 101; 256] )

let test_non_covering_table _ =
  check_battery ~covering:false "non-covering/table"
    $( battery ~seed:0x2C0FFEE ~covering:false ~options:{ match_type = `table }
         [1; 2; 3; 4; 5; 8; 13; 32; 64; 101; 255] )

let test_non_covering_match _ =
  check_battery ~covering:false "non-covering/ranges"
    $( battery ~seed:0x3C0FFEE ~covering:false ~options:{ match_type = `ranges }
         [1; 2; 3; 4; 5; 8; 13; 32; 64; 101; 255] )

let suite = "Charmatch tests" >::: [
      "covering tests (table)"     >:: test_covering_table;
      "covering tests (match)"     >:: test_covering_match;
      "non-covering tests (table)" >:: test_non_covering_table;
      "non-covering tests (match)" >:: test_non_covering_match;
    ]

let _ =
  run_test_tt_main suite
