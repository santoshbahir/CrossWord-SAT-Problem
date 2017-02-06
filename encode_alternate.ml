(* Import trie and directory reading and reading puzzle modules *)
#use "trie_alternate.ml";;
#use "readdict_alternate.ml";;
#use "readpuzzle.ml";;

(* Import cnf project *)
#load "str.cma";;
#use "ast.ml";;
#use "expressionLibrary.ml";;
#use "cnf.ml";;

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

(* build in-memory data structure trie *)
let e1 = emptyTrie;;
let wl1 = readdict "puzzles/microwords";;
let t1 = buildTrie e1 wl1;;

(* read the puzzle and construct the list boolean variables *)
let p1 = readPuzzle "puzzles/micro.puzzle";;
let pl1 = puzzleLines p1;;
let lbv1 = lstBoolVar pl1;;
let cbv1 = comBoolVarLst lbv1;;
let hl1 = fst (hashDotList lbv1);;
let dl1 = snd (hashDotList lbv1);;

(* construct the first consraint: Allowed legitimate words at any position in the trie *)
let dp1 = encodeDictInPuzzle cbv1 t1;;
let f1 = to_string_smart (convert (parse(dp1)));;
let dictconst1 = string_map_partial jeform f1;;

(* construct the second constraint: for all # at given place, Presence of it is always true *)
let rec fhc1 lst =
  match lst with
  | [] -> ""
  | h::t -> let appendAND = (if (List.length t) = 0 then "" else " & ") in
			h ^ appendAND ^ (fhc1 t);;

let hashconst1 = fhc1 hl1;;

(*
 * Construct the third construct: uniqueness and exclusiveness of character
 * which could occur at any '.' in the puzzle
 *)

let fdc1 lst =
  let exclconst1 metaVar =
	let ind = String.index metaVar '_' in
	let suffix = String.sub metaVar ind ((String.length metaVar) - (ind)) in
	let rstAlpha c =
	  let rec rstA i =
		let ch = char_of_int (i + 123) in
		let nch = char_of_int ((i + 1) + 123) in
		if i = 0 then ""
		else if ch = c then rstA (i + 1)
		else let appendOR = (
			   if ((i + 1) = 0) || ((nch = c) && (i + 2 = 0)) then "" else " | "
			 )
			 in " ~" ^ (Char.escaped ch) ^ suffix ^ appendOR ^ (rstA (i + 1))
	  in rstA (-26)
	in
	let exclVar c = " ( " ^ (Char.escaped c) ^ suffix ^  " & " ^ (rstAlpha c) ^ " ) " in
	let rec allLetter i =
	  if i = 0 then ""
	  else let appendOR = (if (i + 1) = 0 then "" else " & ") in
		(exclVar (char_of_int (i + 123)))  ^ appendOR ^ (allLetter (i + 1))
	in " ( " ^ (allLetter (-26)) ^ " ) "
  in
  let rec dconst1 l =
	match l with
	| [] -> ""
	| h::t -> let appendAND = (if (List.length t) = 0 then "" else " & ") in
		(exclconst1 h) ^ appendAND ^ (dconst1 t)
  in dconst1 lst;;

let dc1 = fdc1 dl1;;
let dc2 = to_string_smart (convert (parse (dc1)));;
let dotconst1 = string_map_partial jeform dc2;;

(* Fourth constraint : At least 1 variable at '.' *)
let foc1 lst =
  let atLeastConst1 metaVar =
	let ind = String.index metaVar '_' in
	let suffix = String.sub metaVar ind ((String.length metaVar) - (ind)) in
	let rec allLetter i =
	  if i = 0 then ""
	  else let appendOR = (if (i + 1) = 0 then "" else " | ") in
		Char. escaped (char_of_int (i + 123))  ^ suffix ^ appendOR ^ (allLetter (i + 1))
	in " ( " ^ (allLetter (-26)) ^ " ) "
  in
  let rec oconst1 l =
	match l with
	| [] -> ""
	| h::t -> let appendAND = (if (List.length t) = 0 then "" else " & ") in
		(atLeastConst1 h) ^ appendAND ^ (oconst1 t)
  in oconst1 lst;;

let oc1 = foc1 dl1;;
let oc2 = to_string_smart (convert (parse (oc1)));;
let atLeastOneconst1 = string_map_partial jeform oc2;;

(* combine the three consraints: Uniqueness, exclusivity and legitimacy of the word at
 * give positiion
 *)
let encPuzzle = hashconst1 ^ " & " ^ dotconst1 ^ " & " ^ atLeastOneconst1 ^ " & " ^ dictconst1;;
(* let encPuzzle = hashconst1 ^ " & " ^ dotconst1;; *)


(*
 * Measure the execution time of any function
 *)
let time f x =
  let t = Sys.time() in
  let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx;;

(*
 * Write to encoded string in the file
 *)

open Printf;;

let file = "first.enc";;
let () = let oc = open_out file in
		 fprintf oc "%s\n" encPuzzle;
		 close_out oc;;
