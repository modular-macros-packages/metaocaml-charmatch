
$gen range_set : char -> char -> Charset.t = fun l h ->
  let rec go i s =
    if i > Char.code h then s
    else go (i + 1) (Charset.add (Char.chr i) s)
  in
  go (Char.code l) Charset.empty

$gen lower : unit -> Charset.t = fun () -> range_set 'a' 'z'
$gen digit : unit -> Charset.t = fun () -> range_set '0' '9'
$gen rest : unit -> Charset.t = fun () ->
  Charset.diff (range_set '\000' '\255') (Charset.union (lower ()) (digit ()))

$gen noncovering : options:Charmatch.options -> unit -> (char -> int) expr =
  fun ~options () ->
    << fun ch ->
      $(Charmatch.ifmem ~options <<ch>> ~otherwise:<< -1 >>
          [ (lower (), << 0 >>); (digit (), << 1 >>) ]) >>

$gen covering : options:Charmatch.options -> unit -> (char -> int) expr =
  fun ~options () ->
    << fun ch ->
      $(Charmatch.ifmem ~options <<ch>>
          [ (lower (), << 0 >>); (digit (), << 1 >>); (rest (), << 2 >>) ]) >>

let expected_nc ch =
  if 'a' <= ch && ch <= 'z' then 0
  else if '0' <= ch && ch <= '9' then 1
  else -1

let expected_cov ch =
  if 'a' <= ch && ch <= 'z' then 0
  else if '0' <= ch && ch <= '9' then 1
  else 2

let check name f expected =
  for i = 0 to 255 do
    let ch = Char.chr i in
    let got = f ch and want = expected ch in
    if got <> want then begin
      Printf.printf "%s: mismatch at %d: got %d, want %d\n" name i got want;
      exit 1
    end
  done;
  Printf.printf "%s: ok\n" name

let () = begin
  check "table/noncovering"
    $( noncovering ~options:{ match_type = `table } () ) expected_nc;
  check "ranges/noncovering"
    $( noncovering ~options:{ match_type = `ranges } () ) expected_nc;
  check "table/covering"
    $( covering ~options:{ match_type = `table } () ) expected_cov;
  check "ranges/covering"
    $( covering ~options:{ match_type = `ranges } () ) expected_cov;
  print_endline "charmatch smoke: all ok"
end
