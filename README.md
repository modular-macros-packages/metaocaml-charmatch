## charmatch: pattern-matching generation for characters

[![Charmatch test](https://github.com/yallop/metaocaml-charmatch/actions/workflows/test.yml/badge.svg)](https://github.com/yallop/metaocaml-charmatch/actions/workflows/test.yml)

*This branch is the MacoCaml port: `ifmem` is a macro, the set
operations it consumes at generation time are macro members of a
template-functor parameter (`Make [C : SET]`), and the default
instance works over the [charset](https://github.com/yallop/charset)
package.  Patterns exist only in surface syntax in MacoCaml, so the
generator emits what the original's matches compiled to anyway:
comparison chains, or a table lookup with a small integer dispatch.*

The following call:

```ocaml
macro f : unit -> (char -> int) expr = fun () ->
  << fun x -> $(ifmem <<x>>
                  [Charset.of_list ['a';'b';'c';'e';'f';'g';'z'], <<1>>;
                   Charset.of_list ['1';'2';'3';'9'], <<2>>]
                  ~otherwise:<<3>>) >>
```

either generates compact code for matching characters
(`~options:{match_type=`ranges}`):

```ocaml
fun x ->
  let x = x in
  if ('a' <= x && x <= 'c') || ('e' <= x && x <= 'g') || x = 'z' then 1
  else if ('1' <= x && x <= '3') || x = '9' then 2
  else 3
```

or, with the default options, equivalent (but typically faster) code
based on a table lookup:

```ocaml
fun x ->
  let i = Char.code
            (String.unsafe_get
               "\002...\002\001\001\001\002\002...\002\002\000\000\000\002\000\000\000\002\002...\002\002"
               (Char.code x)) in
  if i = 0 then 1 else if i = 1 then 2 else 3
```
