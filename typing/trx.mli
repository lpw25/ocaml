(* BER MetaOCaml compilation
   Transforming the Typedtree to eliminate brackets and escapes,
   replacing them with calls to ordinary OCaml functions
   to build the code representation (that is, Parsetree).
*)

(* The function to process the body of the bracket at level n.
   This function `lifts' the Typedtree to the code that will evaluate
   to the corresponding Parsetree.
*)
val trx_bracket : int -> Typedtree.expression -> Typedtree.expression

(* Build a Typedtree node for brackets or escape (the attribute tells
   which is which)
*)
