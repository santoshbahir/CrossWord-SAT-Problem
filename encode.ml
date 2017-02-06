open Trie_alternate
open Readdict_alternate
open Readpuzzle

open Str
open Ast
open ExpressionLibrary
open Cnf

(* The expected command line to run the program is : *)
(* 	./encode <puzzle name>.puzzle <dictionary name> > <puzzle name>.enc *)

let () = if Array.length Sys.argv - 1 <> 2 then
		   begin
			 print_string "\nCorrect syntax is :\n";
			 print_string "encode <puzzle name>.puzzle <dictionary name> > <puzzle name>.enc\n\n" ;
			 exit 0
		   end

(* let progname = Sys.argv.(0) *)
let puzname = Sys.argv.(1)
let dictname = Sys.argv.(2)

(* #use "use.ml";; *)
(* let puzname = "puzzles/minute.puzzle" *)
(* let dictname = "puzzles/microwords" *)

let jeform ch = 
  match ch with
	| '('
	| ')' -> None
	| '~' -> Some '-'
	| '|' -> Some 'v'
	| _   -> Some ch ;;

let string_map_partial f s =
  let b = Buffer.create (String.length s) in
  let addperhaps c =
        match f c with
          | None -> ()
          | Some c' -> Buffer.add_char b c'
  in
    String.iter addperhaps s;
    Buffer.contents b;;

(* read the puzzle and construct the list boolean variables *)
let p1 = readPuzzle puzname;;
let pl1 = puzzleLines p1;;
let pd1 = fst pl1;;
let lbv1 = lstBoolVar pl1;;
let hl1 = fst (hashDotList lbv1);;
let dl1 = snd (hashDotList lbv1);;

(* If the '/' is present in the dictionary name, either relative or absolute path *)
(* is specified. In this case, last word is the name of the dictionary *)
let dname = 
  try
	let pcharInd = (String.rindex dictname '/') in 
	String.sub dictname (pcharInd + 1) ((String.length dictname) - (pcharInd + 1))
  with 
  Not_found ->  dictname

let dictEnc = dname ^ ".enc"

(* If the '/' is present in the puzzle name, either relative or absolute path *)
(* is specified. In this case, last word is the name of the puzzle. It is used for *)
(* the names of the files in which encoding and debugging information is dumped *)
let pname = 
  let pnamewithdot =
  try
	let pcharInd = (String.rindex dictname '/') in 
	String.sub puzname (pcharInd + 1) ((String.length puzname) - (pcharInd + 1))
  with 
  Not_found ->  puzname
  in let pcharInd = String.rindex pnamewithdot '.' in
	 String.sub pnamewithdot 0 pcharInd

(* Debugging informational log *)
open Printf;;
let debugfile = pname ^ ".debug";;
let debugc = open_out debugfile;;

(* construct the first consraint: Allowed legitimate words at any position in the trie *)
let dictconst1 = 
	(* build in-memory data structure trie *)
	let e1 = emptyTrie in
	let wl1 = readdict dictname in
	let t1 = buildTrie e1 wl1 in
	let ch_dictEnc = open_out dictEnc in
	let dplst1 = encodeDictInPuzzle pd1 hl1 t1 in
	let rec fconList lst = 
	  match lst with
		| [] -> []
		| h::t -> let henc1 = to_string_smart (convert (parse h)) in
				  print_string ((string_map_partial jeform henc1) ^ " & \n") ; (fconList t)
	in close_out ch_dictEnc; fconList dplst1;;

let alphabet = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z'];;
(* let alphabet = ['f'; 'i'; 'h'; 'o'] *)

(* construct the second constraint: for all # at given place, Presence of it is always true *)
let rec fhc1 lst =
  let exchash h1 =
	let ind = String.index h1 '_' in
	let suffix = String.sub h1 ind ((String.length h1) - (ind)) in
	let rec allAlpha la =
	  match la with
		| [] -> ""
		| h::t -> let appendAND = (if List.length t = 0 then "" else " & ") in
				  "~" ^ (Char.escaped h) ^ suffix ^ appendAND ^ (allAlpha t)
	in h1 ^ " & " ^ (allAlpha alphabet)
  in
  let rec hconst1 l =
	match l with
	  | [] -> ""
	  | h::t -> let appendAND = (if (List.length t) = 0 then "" else " & ") in
				(exchash h) ^ appendAND ^ (fhc1 t)
  in hconst1 lst;;

let hc1 = fhc1 hl1;;
let hashconst1 = string_map_partial jeform hc1;;
let () = fprintf debugc "\n\n%s = %s\n" "hashconstraints" hc1;;

(*
 * Construct the third construct: uniqueness and exclusiveness of character
 * which could occur at any '.' in the puzzle
 *)

let fdc1 lst =
  let exclconst1 metaVar =
	let ind = String.index metaVar '_' in
	let suffix = String.sub metaVar ind ((String.length metaVar) - (ind)) in
	let rstAlpha c =
	  let rec rstA la =
		match la with
		  | [] -> ""
		  | h::t ->
			if h = c then
			  rstA t
			else
				let appendAND =
				  (if (List.length t = 0 || ((List.hd t = c) && (List.length (List.tl t) = 0))) then
					  ""
				   else " & ")
				in " ~" ^ (Char.escaped h) ^ suffix ^ appendAND ^ (rstA t)
	  in rstA alphabet
	in
	let exclVar c = " ( " ^ (Char.escaped c) ^ suffix ^  " & " ^ (rstAlpha c) ^ " & ~#" ^ suffix ^ " ) " in
	let rec allLetter la =
	  match la with
		| [] -> ""
		| h::t -> let appendOR = if List.length t = 0 then "" else " | " in
					(exclVar h) ^ appendOR ^ (allLetter t)
	in " ( " ^ 
		 (if String.get metaVar 0 <> '.' then 
			exclVar (String.get metaVar 0) 
		  else
			(allLetter alphabet)
		 )
		^ " ) "
  in
  let rec dconst1 l =
	match l with
	| [] -> ""
	| h::t -> let appendAND = (if (List.length t) = 0 then "" else " & ") in
		(exclconst1 h) ^ appendAND ^ (dconst1 t)
  in dconst1 lst;;

let dc1 = fdc1 dl1;;
let () = fprintf debugc "\n\n%s = %s\n" "dotconstraints" dc1;;
let dc2 = to_string_smart (convert (parse (dc1)));;
let dotconst1 = string_map_partial jeform dc2;;


(* combine the three consraints: Uniqueness, exclusivity and legitimacy of the word at
 * give positiion
 *)
let encPuzzle = hashconst1 ^ " & " ^ dotconst1;;

(*
 * Measure the execution time of any function
 *)
let time f x =
  let t = Sys.time() in
  let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx;;

(*
 * Write to encoded string in the file for debugging.
 * Also write to the terminal as this is expected way to show the results.
 *)

let file = pname ^ ".enc";;
let () = let oc = open_out file in
		 fprintf oc "%s\n" encPuzzle;
		 close_out oc;;

let () = print_string encPuzzle;;
(*
 * Close the debug file channel
 *)
close_out debugc;;
