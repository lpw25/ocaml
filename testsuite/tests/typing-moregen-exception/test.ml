(* TEST
 * expect
*)

module type T = sig
  type t
end
module Int = struct
  type t = int
end
module type S = sig
  module Choice : T
  val r : Choice.t list ref ref
end
module Force (X : functor () -> S) = struct end
module Choose () = struct
  module Choice =
    (val (module Int : T))
  let r = ref (ref [])
end
module Ignore = Force(Choose)
[%%expect{|
module type T = sig type t end
module Int : sig type t = int end
module type S = sig module Choice : T val r : Choice.t list ref ref end
module Force : functor (X : functor () -> S) -> sig  end
module Choose :
  functor () -> sig module Choice : T val r : '_weak1 list ref ref end
Line 17, characters 22-28:
17 | module Ignore = Force(Choose)
                           ^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor () -> sig module Choice : T val r : '_weak1 list ref ref end
       is not included in
         functor () -> S
       At position functor () -> <here>
       Modules do not match:
         sig module Choice : T val r : '_weak1 list ref ref end
       is not included in
         S
       At position functor () -> <here>
       Values do not match:
         val r : '_weak1 list ref ref
       is not included in
         val r : Choice.t list ref ref
|}];;

module O = struct
  module type s
  module M: sig
    val f: (module s) -> unit
  end = struct
    module type s
    let f (module X:s) = ()
  end
end;;
[%%expect{|
Line 5, characters 8-66:
5 | ........struct
6 |     module type s
7 |     let f (module X:s) = ()
8 |   end
Error: Signature mismatch:
       Modules do not match:
         sig module type s val f : (module s) -> unit end
       is not included in
         sig val f : (module s) -> unit end
       Values do not match:
         val f : (module s/1) -> unit
       is not included in
         val f : (module s/2) -> unit
       Line 6, characters 4-17:
         Definition of module type s/1
       Line 2, characters 2-15:
         Definition of module type s/2
|}];;

module M : sig
  val f : (<m : 'b. ('b * <m: 'c. 'c * 'bar> as 'bar)>) -> unit
end = struct
  let f (x : <m : 'a. ('a * 'foo)> as 'foo) = ()
end;;
[%%expect{|
Line 3, characters 6-65:
3 | ......struct
4 |   let f (x : <m : 'a. ('a * 'foo)> as 'foo) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : (< m : 'a. 'a * 'b > as 'b) -> unit end
       is not included in
         sig val f : < m : 'b. 'b * < m : 'c. 'c * 'a > as 'a > -> unit end
       Values do not match:
         val f : (< m : 'a. 'a * 'b > as 'b) -> unit
       is not included in
         val f : < m : 'b. 'b * < m : 'c. 'c * 'a > as 'a > -> unit
|}];;

type s = private < m : int; .. >;;

module M : sig
  val f : s -> s
end = struct
  let f (x : <m : int>) = x
end;;
[%%expect{|
type s = private < m : int; .. >
Line 5, characters 6-44:
5 | ......struct
6 |   let f (x : <m : int>) = x
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < m : int > -> < m : int > end
       is not included in
         sig val f : s -> s end
       Values do not match:
         val f : < m : int > -> < m : int >
       is not included in
         val f : s -> s
|}];;

module M : sig
  val x : 'a list ref
end = struct
  let x = ref []
end;;
[%%expect{|
Line 3, characters 6-33:
3 | ......struct
4 |   let x = ref []
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val x : '_weak2 list ref end
       is not included in
         sig val x : 'a list ref end
       Values do not match:
         val x : '_weak2 list ref
       is not included in
         val x : 'a list ref
|}];;

module M = struct let r = ref [] end;;
type t;;
module N : sig val r : t list ref end = M;;
[%%expect{|
module M : sig val r : '_weak3 list ref end
type t
Line 3, characters 40-41:
3 | module N : sig val r : t list ref end = M;;
                                            ^
Error: Signature mismatch:
       Modules do not match:
         sig val r : '_weak3 list ref end
       is not included in
         sig val r : t list ref end
       Values do not match:
         val r : '_weak3 list ref
       is not included in
         val r : t list ref
|}];;

type (_, _) eq = Refl : ('a, 'a) eq;;

module T : sig
  type t
  type s
  val eq : (t, s) eq
end = struct
  type t = int
  type s = int
  let eq = Refl
end;;

module M = struct let r = ref [] end;;

let foo p (e : (T.t, T.s) eq) (x : T.t) (y : T.s) =
  match e with
  | Refl ->
    let z = if p then x else y in (* should fail *)
    let module N = struct
      module type S = module type of struct let r = ref [z] end
    end in
    let module O : N.S = M in
    ();;
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
module T : sig type t type s val eq : (t, s) eq end
module M : sig val r : '_weak4 list ref end
val foo : bool -> (T.t, T.s) eq -> T.t -> T.s -> unit = <fun>
|}];;

module M: sig
  val f : int -> float
end = struct
  let f (x : 'a) = x
end;;
[%%expect{|
Line 3, characters 6-37:
3 | ......struct
4 |   let f (x : 'a) = x
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : 'a -> 'a end
       is not included in
         sig val f : int -> float end
       Values do not match:
         val f : 'a -> 'a
       is not included in
         val f : int -> float
|}];;

module M: sig
  val f : (int * float * int) -> (int -> int)
end = struct
  let f (x : (int * int)) = x
end;;
[%%expect{|
Line 3, characters 6-46:
3 | ......struct
4 |   let f (x : (int * int)) = x
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : int * int -> int * int end
       is not included in
         sig val f : int * float * int -> int -> int end
       Values do not match:
         val f : int * int -> int * int
       is not included in
         val f : int * float * int -> int -> int
|}];;

module M: sig
  val f : <m : int; n : float> -> <m : int; n : float>
end = struct
  let f (x : <m : int; f : float>) = x
end;;
[%%expect{|
Line 3, characters 6-55:
3 | ......struct
4 |   let f (x : <m : int; f : float>) = x
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < f : float; m : int > -> < f : float; m : int > end
       is not included in
         sig val f : < m : int; n : float > -> < m : int; n : float > end
       Values do not match:
         val f : < f : float; m : int > -> < f : float; m : int >
       is not included in
         val f : < m : int; n : float > -> < m : int; n : float >
|}];;

module M : sig
  val f : [`Foo] -> unit
end = struct
  let f (x : [ `Foo | `Bar]) = ()
end;;
[%%expect{|
Line 3, characters 6-50:
3 | ......struct
4 |   let f (x : [ `Foo | `Bar]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [ `Bar | `Foo ] -> unit end
       is not included in
         sig val f : [ `Foo ] -> unit end
       Values do not match:
         val f : [ `Bar | `Foo ] -> unit
       is not included in
         val f : [ `Foo ] -> unit
|}];;

module M : sig
  val f : [>`Foo] -> unit
end = struct
  let f (x : [< `Foo]) = ()
end;;
[%%expect{|
Line 3, characters 6-44:
3 | ......struct
4 |   let f (x : [< `Foo]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [< `Foo ] -> unit end
       is not included in
         sig val f : [> `Foo ] -> unit end
       Values do not match:
         val f : [< `Foo ] -> unit
       is not included in
         val f : [> `Foo ] -> unit
|}];;

module M : sig
  val f : [< `Foo | `Bar] -> unit
end = struct
  let f (x : [< `Foo]) = ()
end;;
[%%expect{|
Line 3, characters 6-44:
3 | ......struct
4 |   let f (x : [< `Foo]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [< `Foo ] -> unit end
       is not included in
         sig val f : [< `Bar | `Foo ] -> unit end
       Values do not match:
         val f : [< `Foo ] -> unit
       is not included in
         val f : [< `Bar | `Foo ] -> unit
|}];;

module M : sig
  val f : < m : [< `Foo]> -> unit
end = struct
  let f (x : < m : 'a. [< `Foo] as 'a >) = ()
end;;
[%%expect{|
Line 3, characters 6-62:
3 | ......struct
4 |   let f (x : < m : 'a. [< `Foo] as 'a >) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < m : 'a. [< `Foo ] as 'a > -> unit end
       is not included in
         sig val f : < m : [< `Foo ] > -> unit end
       Values do not match:
         val f : < m : 'a. [< `Foo ] as 'a > -> unit
       is not included in
         val f : < m : [< `Foo ] > -> unit
|}];;

module M : sig
  val f : < m : 'a. [< `Foo] as 'a > -> unit
end = struct
  let f (x : < m : [`Foo]>) = ()
end;;
[%%expect{|
Line 3, characters 6-49:
3 | ......struct
4 |   let f (x : < m : [`Foo]>) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : < m : [ `Foo ] > -> unit end
       is not included in
         sig val f : < m : 'a. [< `Foo ] as 'a > -> unit end
       Values do not match:
         val f : < m : [ `Foo ] > -> unit
       is not included in
         val f : < m : 'a. [< `Foo ] as 'a > -> unit
|}];;

module M : sig
  val f : [< `C] -> unit
end = struct
  let f (x : [< `C of int&float]) = ()
end;;
[%%expect{|
Line 3, characters 6-55:
3 | ......struct
4 |   let f (x : [< `C of int&float]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [< `C of int & float ] -> unit end
       is not included in
         sig val f : [< `C ] -> unit end
       Values do not match:
         val f : [< `C of int & float ] -> unit
       is not included in
         val f : [< `C ] -> unit
|}];;

module M : sig
  val f : [`Foo] -> unit
end = struct
  let f (x : [`Foo of int]) = ()
end;;
[%%expect{|
Line 3, characters 6-49:
3 | ......struct
4 |   let f (x : [`Foo of int]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [ `Foo of int ] -> unit end
       is not included in
         sig val f : [ `Foo ] -> unit end
       Values do not match:
         val f : [ `Foo of int ] -> unit
       is not included in
         val f : [ `Foo ] -> unit
|}];;

module M : sig
  val f : [`Foo of int] -> unit
end = struct
  let f (x : [`Foo]) = ()
end;;
[%%expect{|
Line 3, characters 6-42:
3 | ......struct
4 |   let f (x : [`Foo]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [ `Foo ] -> unit end
       is not included in
         sig val f : [ `Foo of int ] -> unit end
       Values do not match:
         val f : [ `Foo ] -> unit
       is not included in
         val f : [ `Foo of int ] -> unit
|}];;

module M : sig
  val f : [< `Foo | `Bar | `Baz] -> unit
end = struct
  let f (x : [< `Foo | `Bar | `Baz]) = ()
end;;
[%%expect{|
module M : sig val f : [< `Bar | `Baz | `Foo ] -> unit end
|}];;

module M : sig
  val f : [< `Foo | `Bar | `Baz] -> unit
end = struct
  let f (x : [> `Foo | `Bar]) = ()
end;;
[%%expect{|
Line 3, characters 6-51:
3 | ......struct
4 |   let f (x : [> `Foo | `Bar]) = ()
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f : [> `Bar | `Foo ] -> unit end
       is not included in
         sig val f : [< `Bar | `Baz | `Foo ] -> unit end
       Values do not match:
         val f : [> `Bar | `Foo ] -> unit
       is not included in
         val f : [< `Bar | `Baz | `Foo ] -> unit
|}];;
