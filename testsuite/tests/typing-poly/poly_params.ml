(* TEST
   * expect
*)

let poly1 (id : 'a. 'a -> 'a) = id 3, id "three"
[%%expect {|
val poly1 : ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly1 (fun x -> x)
[%%expect {|
- : int * string = (3, "three")
|}];;

let _ = poly1 (fun x -> x + 1)
[%%expect {|
Line 1, characters 14-30:
1 | let _ = poly1 (fun x -> x + 1)
                  ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;

let id x = x
let _ = poly1 id
[%%expect {|
val id : 'a -> 'a = <fun>
- : int * string = (3, "three")
|}];;

let _ = poly1 (id (fun x -> x))
[%%expect {|
Line 1, characters 14-31:
1 | let _ = poly1 (id (fun x -> x))
                  ^^^^^^^^^^^^^^^^^
Error: This argument has type 'b -> 'b which is less general than
         'a. 'a -> 'a
|}];;

let poly2 : ('a. 'a -> 'a) -> int * string =
  fun id -> id 3, id "three"
[%%expect {|
val poly2 : ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly2 (fun x -> x)
[%%expect {|
- : int * string = (3, "three")
|}];;

let _ = poly2 (fun x -> x + 1)
[%%expect {|
Line 1, characters 14-30:
1 | let _ = poly2 (fun x -> x + 1)
                  ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;

let poly3 : 'b. ('a. 'a -> 'a) -> 'b -> 'b * 'b option =
  fun id x -> id x, id (Some x)
[%%expect {|
val poly3 : ('a. 'a -> 'a) -> 'b -> 'b * 'b option = <fun>
|}];;

let _ = poly3 (fun x -> x) 8
[%%expect {|
- : int * int option = (8, Some 8)
|}];;

let _ = poly3 (fun x -> x + 1) 8
[%%expect {|
Line 1, characters 14-30:
1 | let _ = poly3 (fun x -> x + 1) 8
                  ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;

let rec poly4 p (id : 'a. 'a -> 'a) =
  if p then poly4 false id else id 4, id "four"
[%%expect {|
val poly4 : bool -> ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly4 true (fun x -> x)
[%%expect {|
- : int * string = (4, "four")
|}];;

let _ = poly4 true (fun x -> x + 1)
[%%expect {|
Line 1, characters 19-35:
1 | let _ = poly4 true (fun x -> x + 1)
                       ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;

let rec poly5 : bool -> ('a. 'a -> 'a) -> int * string =
  fun p id ->
    if p then poly5 false id else id 5, id "five"
[%%expect {|
val poly5 : bool -> ('a. 'a -> 'a) -> int * string = <fun>
|}];;

let _ = poly5 true (fun x -> x)
[%%expect {|
- : int * string = (5, "five")
|}];;

let _ = poly5 true (fun x -> x + 1)
[%%expect {|
Line 1, characters 19-35:
1 | let _ = poly5 true (fun x -> x + 1)
                       ^^^^^^^^^^^^^^^^
Error: This argument has type int -> int which is less general than
         'a. 'a -> 'a
|}];;


let rec poly6 : 'b. bool -> ('a. 'a -> 'a) -> 'b -> 'b * 'b option =
  fun p id x ->
    if p then poly6 false id x else id x, id (Some x)
[%%expect {||}];;

let _ = poly6 true (fun x -> x) 8
[%%expect {||}];;

let _ = poly6 true (fun x -> x + 1) 8
[%%expect {||}];;

let needs_magic (magic : 'a 'b. 'a -> 'b) = (magic 5 : string)
let _ = needs_magic (fun x -> x)
[%%expect {|
val needs_magic : ('a 'b. 'a -> 'b) -> string = <fun>
Line 2, characters 20-32:
2 | let _ = needs_magic (fun x -> x)
                        ^^^^^^^^^^^^
Error: This argument has type 'c. 'c -> 'c which is less general than
         'a 'b. 'a -> 'b
|}];;

let with_id (f : ('a. 'a -> 'a) -> 'b) = f (fun x -> x)
[%%expect {|
val with_id : (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

let _ = with_id (fun id -> id 4, id "four")
[%%expect {|
- : int * string = (4, "four")
|}];;

let non_principal1 p f =
  if p then with_id f
  else f (fun x -> x)
[%%expect {|
val non_principal1 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}, Principal{|
Line 3, characters 7-21:
3 |   else f (fun x -> x)
           ^^^^^^^^^^^^^^
Warning 18 [not-principal]: applying a higher-rank function here is not principal.
val non_principal1 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

let non_principal2 p f =
  if p then f (fun x -> x)
  else with_id f
[%%expect {|
Line 3, characters 15-16:
3 |   else with_id f
                   ^
Error: This expression has type ('b -> 'b) -> 'c
       but an expression was expected of type ('a. 'a -> 'a) -> 'd
       The universal variable 'a would escape its scope
|}];;

let principal1 p (f : ('a. 'a -> 'a) -> 'b) =
  if p then f (fun x -> x)
  else with_id f
[%%expect {|
val principal1 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;

let principal2 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b =
  fun p f ->
    if p then f (fun x -> x)
    else with_id f
[%%expect {|
val principal2 : bool -> (('a. 'a -> 'a) -> 'b) -> 'b = <fun>
|}];;
