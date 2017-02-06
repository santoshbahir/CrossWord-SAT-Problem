open Ast
exception ParseError of string

type token =
  | VarT of Ident.t
  | BinopT of binop
  | UnopT of unop
  | LParen
  | RParen
  | EOF

let recognized_tokens = [|"->"; "<->"|] ;;

let token_expressions = [|BinopT Imply; BinopT Iff|] ;;

let recognized_token_start_char = [|'-'; '<'; '&'; '|'; '^'; '~'; '('; ')'|] ;;

let string_to_char_list (s:string) : char list =
  let rec string_to_char_list' (s:string) (acc:char list) (i:int) =
    if i < 0 then acc else
      let c = String.get s i in
	string_to_char_list' s (c::acc) (i-1)
  in string_to_char_list' s [] (String.length s - 1)

(* The precedence of a binary operator.  Used in the parse_string and
      to_string_smart functions. *)
let binop_precedence (b:binop) : int =
  match b with
    | And -> 1
    | Or -> 2
	| Xor -> 3
    | Iff -> 4
    | Imply -> 5
;;

let unop_precedence (u:unop) : int = 6 ;;

(* A strict upper bound on the precedence of any operator. *)
let prec_bound : int = 7 ;;

let binop_is_associative (b:binop) : bool =
  match b with
    | And | Or | Xor | Iff -> true
    | Imply -> false ;;

(* Pretty-printing functions for expressions *)

let binop_to_string (b:binop) : string =
  match b with
    | And -> " & "
    | Or -> " | "
    | Imply -> " -> "
	| Xor -> " ^ "
    | Iff -> " <-> "
;;

let unop_to_string (u:unop) : string =
  match u with
    | Neg -> "~"
;;

let token_to_string (t:token) : string =
  match t with
    | VarT va -> Ident.toString va
    | BinopT b -> binop_to_string b
    | UnopT u -> unop_to_string u
    | LParen -> "("
    | RParen -> ")"
    | EOF -> "EOF"
;;

(* Only adds parentheses when needed to prevent ambiguity. *)
let to_string_smart (e:expression) : string =
  let rec to_string_smart' e parent_precedence parent_associative =
	match e with	
	  | Var va -> Ident.toString va
      | Unop (u,e1) ->
	  unop_to_string u ^ "(" ^
	    to_string_smart' e1 (unop_precedence u) false ^ ")"
      | Binop (b,e1,e2) ->
	  let prec = binop_precedence b in
          let e_str = 
	      (to_string_smart' e1 prec false ^
	       binop_to_string b ^
	       to_string_smart' e2 prec (binop_is_associative b)) in
            if prec > parent_precedence ||
                  (prec = parent_precedence && not parent_associative)
            then "(" ^ e_str ^ ")"
	    else e_str
  in to_string_smart' e prec_bound false
;;

(* Always adds parentheses around all binary ops. Completely unambiguous;
       however, often very hard to read... *)
let rec to_string (e:expression) : string =
  match e with
    | Var va -> Ident.toString va
    | Unop (u,e1) -> "(" ^ unop_to_string u ^ "(" ^ to_string e1 ^ "))"
    | Binop (b,e1,e2) -> 
        "(" ^ to_string e1 ^ binop_to_string b ^ to_string e2 ^ ")"
;;

(* Match input string to front consecutive elements of char list and return
   remainder of the input list *)
let rec match_string (l:char list) (s:string) : char list option =
  if s = "" then Some l else
    match l with
      | [] -> None
      | h::t ->
	  if h = String.get s 0 then
            match_string t (String.sub s 1 (String.length s - 1))
          else None ;;

let detect_special_char ch =
      let rec mem arr ch i =
                          if i >= Array.length arr then false
                          else 
			  	if (Array.get arr i) = ch then
                                	true
	                          else
        	                          mem arr ch (i+1)
		   in mem recognized_token_start_char ch 0

let rec get_string_token (l : char list) : string * char list =
	match l with
	    | [] -> ("", [])
            | c::[] -> ((Char.escaped c), [])
            | c1::c2::cs ->
                          if (detect_special_char c2) || c2 = ' ' || c2 = '\t' || c2 = '\n' then
                                (Char.escaped c1, (c2::cs))
						  else
			    			let t1 = get_string_token (c2::cs) in
							let e1 = fst t1 in
							let e2 = snd t1 in 
			  				((Char.escaped c1) ^ e1, e2) 

let get_var_token (l : char list) : (token * char list) option =
   match l with
    | [] -> None
    | _ -> let t1 = get_string_token l in Some (VarT (Ident.create (fst t1)), snd t1)

let lex_multi_char_token (l:char list) : (token * char list) option  =
  let rec lex_multi_char_token' l i =
    if i >= Array.length recognized_tokens then get_var_token l 
    else
      match match_string l (Array.get recognized_tokens i) with
	| Some l' -> Some (Array.get token_expressions i, l')
	| None -> lex_multi_char_token' l (i+1)
  in lex_multi_char_token' l 0 ;;

let rec lex' (l:char list) : token list =
  match l with
    | [] -> []
    | ' '::cs -> lex' cs
    | c::cs ->
	let (token, l') =
	  (match c with
	    | '&' -> (BinopT And, cs) 
	    | '|' -> (BinopT Or, cs) 
		| '^' -> (BinopT Xor, cs)
	    | '~' -> (UnopT Neg, cs)
	    | '(' -> (LParen, cs)
	    | ')' -> (RParen, cs)
	    | _ ->
		  (match lex_multi_char_token l with
		    | Some (t, l') -> (t, l')
		    | None -> raise (ParseError "Unrecognized token")))
	in token :: lex' l' ;;

let lex (s:string) : token list =
  lex' (string_to_char_list s) @ [EOF]


(* This function parse the input boolean expression *)
let parse (s:string) : expression =
  let rec parse_toplevel_expression (l:token list) : expression =
    let (e,_,_) = parse_delimited_expression l EOF prec_bound in e

  and parse_expression (l:token list) : expression * token list =
    match l with
      | [] -> raise (ParseError "Unexpected end of string")
      | t::ts ->
          match t with
            | LParen -> let (e,l',_) =
						  parse_delimited_expression ts RParen prec_bound in  (e,l')
            | RParen -> raise (ParseError "Unexpected rparen")
            | UnopT u -> parse_unop ts u
            | VarT v-> (Var v, ts)
            | EOF -> raise (ParseError "Unexpected EOF")
            | BinopT b -> raise (ParseError 
								   ("Unexpected Binop: " ^ token_to_string t))

  and parse_binop (l:token list) (delim:token) (current_prec:int) eq
      : expression * token list * bool =
    match l with
      | [] -> raise (ParseError "Unexpected end of string 2")
      | t::ts ->
          if t = delim then 
			(eq,ts,true) 
		  else
            match t with
              | BinopT b ->
                  let prec = binop_precedence b in
                    if current_prec <= prec then 
					  (eq,l,false)
                    else
					  let (eq2,l',d) = parse_delimited_expression ts delim prec 
					  in if d then 
						  (Binop(b, eq, eq2), l', true)
						else 
						  parse_binop l' delim current_prec (Binop(b,eq,eq2))
              | _ -> raise (ParseError ("Expecting Binop, but found: " 
										  ^ token_to_string t))

  and parse_delimited_expression (l:token list) (delim:token)
      (current_prec:int) : expression * token list * bool =
    match l with
      | [] -> raise (ParseError "Unexpected end of string 3")
      | t::ts ->
          if t = delim then
            raise (ParseError ("Unexpected delim: " ^ token_to_string delim))
          else
	    let (eq,l') = parse_expression l in
              parse_binop l' delim current_prec eq

  and parse_unop (tokens:token list) (u:unop) =
    let (e,t) = parse_expression tokens in (Unop(u,e),t)

  in parse_toplevel_expression (lex s)
;;

(* Example: *)
