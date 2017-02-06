(*  The Ident Abstract Data Type is used to represent the Boolean Variables in the Boolean expression. This ADT ensures that variabls are always started with lower case letters followed by lower or upper case letters and ending with the numbers.
The Ident module depends on the in-built  module Str for regex. To get str module in toplevel, use command '#load "str.cma"'. *)

open Str
module Ident : sig
    exception Non_var
    type t = private string
    val create: string -> t
	val toString: t -> string
    (* val (^): t -> t -> t *)
    (* declare, and define below other functions as needed *)
end = struct
    type t = string
    exception Non_var
    let create s = if (Str.string_match (Str.regexp "[a-z#][A-Za-z]*[0-9_]*$") s 0)
then s else raise Non_var
    let toString va = va
    (*   let ( (^) s1 s2 = create (s1 ^ s2) *)
end;;

(* Binary Operators *)
type binop = And | Or | Imply | Iff | Xor

(* Unary Operators *)
type unop = Neg

(* expression in new Boolean language *)
type expression =  
    Var of Ident.t
  | Binop of binop * expression * expression
  | Unop of unop * expression
