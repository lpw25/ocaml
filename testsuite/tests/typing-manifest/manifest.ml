(* TEST
 * expect
*)

module M : sig
  type t = private [< `A | `B]
end = struct
  type t = [`C]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`C]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `C ] end
       is not included in
         sig type t = private [< `A | `B ] end
       Type declarations do not match:
         type t = [ `C ]
       is not included in
         type t = private [< `A | `B ]
|}];;

module M : sig
  type t = private [< `A | `B]
end = struct
  type t = private [> `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [> `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [> `A ] end
       is not included in
         sig type t = private [< `A | `B ] end
       Type declarations do not match:
         type t = private [> `A ]
       is not included in
         type t = private [< `A | `B ]
|}];;

module M : sig
  type t = private [< `A | `B > `A]
end = struct
  type t = [`B]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`B]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `B ] end
       is not included in
         sig type t = private [< `A | `B > `A ] end
       Type declarations do not match:
         type t = [ `B ]
       is not included in
         type t = private [< `A | `B > `A ]
|}];;

module M : sig
  type t = private [> `A of int]
end = struct
  type t = [`A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `A ] end
       is not included in
         sig type t = private [> `A of int ] end
       Type declarations do not match:
         type t = [ `A ]
       is not included in
         type t = private [> `A of int ]
|}];;

module M : sig
   type t = private [< `A of int]
end = struct
   type t = private [< `A of & int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |    type t = private [< `A of & int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A of & int ] end
       is not included in
         sig type t = private [< `A of int ] end
       Type declarations do not match:
         type t = private [< `A of & int ]
       is not included in
         type t = private [< `A of int ]
|}];;


module M : sig
  type t = private [< `A of int]
end = struct
  type t = private [< `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A ] end
       is not included in
         sig type t = private [< `A of int ] end
       Type declarations do not match:
         type t = private [< `A ]
       is not included in
         type t = private [< `A of int ]
|}];;


module M : sig
  type t = private [< `A of int & float]
end = struct
  type t = private [< `A]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `A]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `A ] end
       is not included in
         sig type t = private [< `A of int & float ] end
       Type declarations do not match:
         type t = private [< `A ]
       is not included in
         type t = private [< `A of int & float ]
|}];;

module M : sig
  type t = private [> `A of int]
end = struct
  type t = [`A of float]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = [`A of float]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `A of float ] end
       is not included in
         sig type t = private [> `A of int ] end
       Type declarations do not match:
         type t = [ `A of float ]
       is not included in
         type t = private [> `A of int ]
|}];;

module M : sig
  type t = private <a : int; ..>
end = struct
  type t = <b : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <b : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < b : int > end
       is not included in
         sig type t = private < a : int; .. > end
       Type declarations do not match:
         type t = < b : int >
       is not included in
         type t = private < a : int; .. >
|}];;

module M : sig
  type t = private <a : float; ..>
end = struct
  type t = <a : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <a : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < a : int > end
       is not included in
         sig type t = private < a : float; .. > end
       Type declarations do not match:
         type t = < a : int >
       is not included in
         type t = private < a : float; .. >
|}];;

type w = private float
type q = private (int * w)
type u = private (int * q)
module M : sig (* Confussing error message :( *)
  type t = private (int * (int * int))
end = struct
  type t = private u
end;;
[%%expect{|
type w = private float
type q = private int * w
type u = private int * q
Lines 6-8, characters 6-3:
6 | ......struct
7 |   type t = private u
8 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private u end
       is not included in
         sig type t = private int * (int * int) end
       Type declarations do not match:
         type t = private u
       is not included in
         type t = private int * (int * int)
|}];;

type w = float
type q = (int * w)
type u = private (int * q)
module M : sig (* Confussing error message :( *)
  type t = private (int * (int * int))
end = struct
  type t = private u
end;;
[%%expect{|
type w = float
type q = int * w
type u = private int * q
Lines 6-8, characters 6-3:
6 | ......struct
7 |   type t = private u
8 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private u end
       is not included in
         sig type t = private int * (int * int) end
       Type declarations do not match:
         type t = private u
       is not included in
         type t = private int * (int * int)
|}];;

type s = private int

module M : sig
  type t = private float
end = struct
  type t = private s
end;;
[%%expect{|
type s = private int
Lines 5-7, characters 6-3:
5 | ......struct
6 |   type t = private s
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private s end
       is not included in
         sig type t = private float end
       Type declarations do not match:
         type t = private s
       is not included in
         type t = private float
|}];;
