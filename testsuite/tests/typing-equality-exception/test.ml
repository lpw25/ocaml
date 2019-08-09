(* TEST
 * expect
*)

module M : sig
  type ('a, 'b) t = 'a * 'b
end = struct
  type ('a, 'b) t = 'a * 'a
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t = 'a * 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t = 'a * 'a end
       is not included in
         sig type ('a, 'b) t = 'a * 'b end
       Type declarations do not match:
         type ('a, 'b) t = 'a * 'a
       is not included in
         type ('a, 'b) t = 'a * 'b
       Type 'a * 'a is not equal to type 'a * 'b
       Type 'a is not equal to type 'b
|}];;

module M : sig
  type ('a, 'b) t = 'a * 'a
end = struct
  type ('a, 'b) t = 'a * 'b
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a, 'b) t = 'a * 'b
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type ('a, 'b) t = 'a * 'b end
       is not included in
         sig type ('a, 'b) t = 'a * 'a end
       Type declarations do not match:
         type ('a, 'b) t = 'a * 'b
       is not included in
         type ('a, 'b) t = 'a * 'a
       Type 'a * 'b is not equal to type 'a * 'a
       Type 'b is not equal to type 'a
|}];;

module M : sig
  type t = <m : 'b. 'b * ('b * <m:'c. 'c * 'bar> as 'bar)>
end = struct
  type t = <m : 'a. 'a * ('a * 'foo)> as 'foo
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <m : 'a. 'a * ('a * 'foo)> as 'foo
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < m : 'a. 'a * ('a * 'b) > as 'b end
       is not included in
         sig type t = < m : 'b. 'b * ('b * < m : 'c. 'c * 'a > as 'a) > end
       Type declarations do not match:
         type t = < m : 'a. 'a * ('a * 'b) > as 'b
       is not included in
         type t = < m : 'b. 'b * ('b * < m : 'c. 'c * 'a > as 'a) >
       Type < m : 'a. 'a * ('a * 'd) > as 'd is not equal to type
         < m : 'b. 'b * ('b * < m : 'c. 'c * 'e > as 'e) >
       The method m has type 'a. 'a * ('a * 'd),
       but the expected method type was 'c. 'c * 'e
       The universal variable 'b would escape its scope
|}];;

type s = private < m : int; .. >;;
[%%expect{|
type s = private < m : int; .. >
|}];;

module M : sig
  type t = s
end = struct
  type t = <m : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <m : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < m : int > end
       is not included in
         sig type t = s end
       Type declarations do not match:
         type t = < m : int >
       is not included in
         type t = s
       Type < m : int > is not equal to type s
       The second object type has an abstract row, it cannot be closed
|}];;

module M : sig
  type t = <m : int>
end = struct
  type t = s
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = s
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = s end
       is not included in
         sig type t = < m : int > end
       Type declarations do not match:
         type t = s
       is not included in
         type t = < m : int >
       Type s is not equal to type < m : int >
       The first object type has an abstract row, it cannot be closed
|}];;

module M : sig
  type t =
    | Foo of (int)*float
end = struct
  type t =
    | Foo of (int*int)*float
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of (int*int)*float
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of (int * int) * float end
       is not included in
         sig type t = Foo of int * float end
       Type declarations do not match:
         type t = Foo of (int * int) * float
       is not included in
         type t = Foo of int * float
       Constructors do not match:
         Foo of (int * int) * float
       is not equal to:
         Foo of int * float
       Type int * int is not equal to type int
|}];;

module M : sig
  type t = (int * float)
end = struct
  type t = (int * float * int)
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = (int * float * int)
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = int * float * int end
       is not included in
         sig type t = int * float end
       Type declarations do not match:
         type t = int * float * int
       is not included in
         type t = int * float
       Type int * float * int is not equal to type int * float
|}];;

module M : sig
  type t = <n : int; m : float>
end = struct
  type t = <n : int; f : float>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <n : int; f : float>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < f : float; n : int > end
       is not included in
         sig type t = < m : float; n : int > end
       Type declarations do not match:
         type t = < f : float; n : int >
       is not included in
         type t = < m : float; n : int >
       Type < f : float; n : int > is not equal to type
         < m : float; n : int >
       The second object type has no method f
|}];;

module M : sig
  type t = <n : int; m : float>
end = struct
  type t = <n : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <n : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < n : int > end
       is not included in
         sig type t = < m : float; n : int > end
       Type declarations do not match:
         type t = < n : int >
       is not included in
         type t = < m : float; n : int >
       Type < n : int > is not equal to type < m : float; n : int >
       The first object type has no method m
|}];;

module M4 : sig
  type t = <n : int; m : float * int>
end = struct
  type t = <n : int; m : int>
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = <n : int; m : int>
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = < m : int; n : int > end
       is not included in
         sig type t = < m : float * int; n : int > end
       Type declarations do not match:
         type t = < m : int; n : int >
       is not included in
         type t = < m : float * int; n : int >
       Type < m : int; n : int > is not equal to type
         < m : float * int; n : int >
       Types for method m are incompatible
|}];;

module M4 : sig
  type t =
    | Foo of [`Foo of string | `Bar of string]
end = struct
  type t =
    | Foo of [`Bar of string]
end;;
[%%expect{|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   type t =
6 |     | Foo of [`Bar of string]
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = Foo of [ `Bar of string ] end
       is not included in
         sig type t = Foo of [ `Bar of string | `Foo of string ] end
       Type declarations do not match:
         type t = Foo of [ `Bar of string ]
       is not included in
         type t = Foo of [ `Bar of string | `Foo of string ]
       Constructors do not match:
         Foo of [ `Bar of string ]
       is not equal to:
         Foo of [ `Bar of string | `Foo of string ]
       Type [ `Bar of string ] is not equal to type
         [ `Bar of string | `Foo of string ]
       The first declaration has no tag `Foo
|}];;

module M : sig
  type t = private [`C of int]
end = struct
  type t = private [`C]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`C]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `C ] end
       is not included in
         sig type t = private [ `C of int ] end
       Type declarations do not match:
         type t = private [ `C ]
       is not included in
         type t = private [ `C of int ]
       Type [ `C ] is not equal to type [ `C of int ]
       Types for tag `C are incompatible
|}];;

module M : sig
  type t = private [`C]
end = struct
  type t = private [`C of int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`C of int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `C of int ] end
       is not included in
         sig type t = private [ `C ] end
       Type declarations do not match:
         type t = private [ `C of int ]
       is not included in
         type t = private [ `C ]
       Type [ `C of int ] is not equal to type [ `C ]
       Types for tag `C are incompatible
|}];;

module M : sig
  type t = [`C of [< `A] | `C of [`A]]
end = struct
  type t = [`C of [< `A | `B] | `C of [`A]]
end;;
[%%expect{|
module M : sig type t = [ `C of [ `A ] ] end
|}];;

module M : sig
  type t = private [> `A of int]
end = struct
  type t = private [`A of int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [`A of int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [ `A of int ] end
       is not included in
         sig type t = private [> `A of int ] end
       Type declarations do not match:
         type t = private [ `A of int ]
       is not included in
         type t = private [> `A of int ]
       Type [ `A of int ] is not equal to type [> `A of int ]
       The second is open and the first is not
|}];;

module M : sig
  type t = private [`A of int]
end = struct
  type t = private [> `A of int]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [> `A of int]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [> `A of int ] end
       is not included in
         sig type t = private [ `A of int ] end
       Type declarations do not match:
         type t = private [> `A of int ]
       is not included in
         type t = private [ `A of int ]
       Type [> `A of int ] is not equal to type [ `A of int ]
       The first is open and the second is not
|}];;

module M : sig
  type 'a t =  [> `A of int | `B of int] as 'a
end = struct
  type 'a t =  [> `A of int] as 'a
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t =  [> `A of int] as 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a constraint 'a = [> `A of int ] end
       is not included in
         sig type 'a t = 'a constraint 'a = [> `A of int | `B of int ] end
       Type declarations do not match:
         type 'a t = 'a constraint 'a = [> `A of int ]
       is not included in
         type 'a t = 'a constraint 'a = [> `A of int | `B of int ]
       Type [> `A of int ] is not equal to type [> `A of int | `B of int ]
       The first declaration has no tag `B
|}];;

module M : sig
  type 'a t =  [> `A of int] as 'a
end = struct
  type 'a t =  [> `A of int | `C of float] as 'a
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type 'a t =  [> `A of int | `C of float] as 'a
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a constraint 'a = [> `A of int | `C of float ] end
       is not included in
         sig type 'a t = 'a constraint 'a = [> `A of int ] end
       Type declarations do not match:
         type 'a t = 'a constraint 'a = [> `A of int | `C of float ]
       is not included in
         type 'a t = 'a constraint 'a = [> `A of int ]
       Type [> `A of int | `C of float ] is not equal to type [> `A of int ]
       The second declaration has no tag `C
|}];;

module M : sig
  type t = [`C of [< `A | `B] | `C of [`A]]
end = struct
  type t = [`C of [< `A] | `C of [`A]]
end;;
[%%expect{|
module M : sig type t = [ `C of [ `A ] ] end
|}];;

module M : sig
  type t = private [< `C]
end = struct
  type t = private [< `C of int&float]
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private [< `C of int&float]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private [< `C of int & float ] end
       is not included in
         sig type t = private [< `C ] end
       Type declarations do not match:
         type t = private [< `C of int & float ]
       is not included in
         type t = private [< `C ]
|}];;
