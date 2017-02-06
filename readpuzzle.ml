(**
 * read a puzzle from input file. Example puzzle file is as below:	
 *					4 4
 *					####
 *					#..#
 *					#..#
 *					####
 * 
 *)

open Trie_alternate
open Readdict_alternate
let readPuzzle (filename : string) : string list =
  let ic = open_in filename in
  let input_line_option ip_ch =
	try
	  Some (input_line ip_ch) 
	with End_of_file -> None in
  let rec read_file input_chan =
	match input_line_option input_chan with 
	  | Some line -> line::(read_file input_chan)
	  | None -> close_in input_chan; []
  in read_file ic
;;

let puzzleLines lst = 
  let strDimen = List.hd lst in
  let strPuzzle = List.tl lst in
  let spaceIndex = String.index strDimen ' ' in
  let strWidth = String.sub strDimen 0 spaceIndex in
  let strHeight = String.sub strDimen (spaceIndex + 1) ((String.length strDimen) - (spaceIndex + 1)) in
  let intWidth = int_of_string strWidth in
  let intHeigh = int_of_string strHeight in
  let explode s =
	let rec exp i l =
	  if i < 0 then l else exp (i - 1) ((Char.escaped s.[i]) :: l) 
	in exp (String.length s - 1) [] 
  in
  let rec lstPuzzle p oplist = 
	match p with
	| [] -> oplist
	| h::t -> (explode h)::(lstPuzzle t oplist) 
  in (intWidth, intHeigh), (lstPuzzle strPuzzle []);;


let lstBoolVar t =
  let tupDim = fst t in
  let width = fst tupDim in
  let len = snd tupDim in
  let lstPuzzle = snd t in
  let rec lLoop w l lt olt =
	if l = 0 then olt
	else lLoop w 
			   (l - 1) 
			   (List.tl lt) 
			   (
				 (
				   (List.hd lt) ^ "_" ^
					 (string_of_int w) ^ "_" ^
					   (string_of_int l)
				 )::olt
			   )
  in
  let rec wLoop w l lt olt =
	if w = 0 then olt
	else wLoop (w - 1) l (List.tl lt) (lLoop w l (List.rev (List.hd lt)) olt)
  in wLoop width len (List.rev lstPuzzle) [];;


let comBoolVarLst lst = 
  let  alphaBoolVar strVar = 
	let ind = String.index strVar '_' in
	let suffix = String.sub strVar ind ((String.length strVar) - (ind)) in 
	let rec letterList i lst = 
	  if i = 0 then lst
	  else letterList (i - 1) (((Char.escaped (char_of_int (i + 96))) ^ suffix)::lst)
	in letterList 26 [] 
  in													
  let rec comBoolVar l1 l2 = 
	  match l1 with 
	  | [] -> l2
	  | h::t -> if (String.get h 0) = '.' then
				  comBoolVar t (List.rev_append (List.rev (alphaBoolVar h)) l2)
			else
				  comBoolVar t (h::l2)
in comBoolVar lst [];;  

(* Separate the '#'list and '.'list for future usage *)
let hashDotList lst = 
  let f bv = if (String.get bv 0) = '#' then true else false in
  List.partition f lst;;
  
(*
 * Encode the dicotionary for each #_d1_d2
 * For each element in the list -
 * 	Check if it is begins with #, if yes -
 *		- find out the dimensions
 *		- Encode the dictionary once horizontally and once vertically  
 *)

let encodeDictInPuzzle dim l dictTrie =
  let getDim hbv =
	let us1 = String.index hbv '_' in
	let us2 = String.rindex hbv '_' in 
	let ns1 = String.sub hbv (us1 + 1) (us2 - us1 - 1) in
	let ns2 = String.sub hbv (us2 + 1) ((String.length hbv) - us2 - 1) in
	((int_of_string ns1), (int_of_string ns2))  
  in
  let hashDimList = List.rev_map getDim l in  
  let adjHash hd dir =
	let adjDim = 
	if 'H' = dir then 
	  ((fst hd) + 1, (snd hd)) 
	else 
	  ((fst hd), (snd hd) + 1)
	in
	let isHashAdj elem = if elem = adjDim then true else false in
	  List.exists isHashAdj hashDimList
  in 
  let fh hdim pdim dir = 
	let hdimh = fst hdim in
	let hdimv = snd hdim in
	let pdimh = fst pdim in
	let pdimv = snd pdim in
	if 'H' = dir then
	  if hdimv = 1 || hdimv = pdimv || hdimh = pdimh 
		 || (adjHash hdim 'H') then false 
	  else true
	else 
	  if hdimh = 1 || hdimh = pdimh || hdimv = pdimv 
		 || (adjHash hdim 'V') then false
	  else true
  in
  let rec encodeDict lst dictTrie = 
	match lst with
	  | [] -> []
	  | h::t -> 
		let us1 = String.index h '_' in
		let us2 = String.rindex h '_' in 
		let ns1 = String.sub h (us1 + 1) (us2 - us1 - 1) in
		let ns2 = String.sub h (us2 + 1) ((String.length h) - us2 - 1) in
		let d1 = int_of_string ns1 in
		let d2 = int_of_string ns2 in
		let henc = 
		  if (fh (d1, d2) dim 'H') then 
			(TrieM.fencpuz getKeyRep '#' ('H', d1, d2) dictTrie) 
		  else
			"" 
		in
		let venc = 
		  if (fh (d1, d2) dim 'V') then
			(TrieM.fencpuz getKeyRep '#' ('V', d1, d2) dictTrie) 
		  else 
			""
		in 
		henc::venc::(encodeDict t dictTrie)
  in 
  let encodedList0 = encodeDict l dictTrie in
  let encodedList1 = List.filter (fun x -> if String.length x = 0 then false else true) encodedList0 in
  (* let rec conjectClause enclist = *)
  (* match enclist with *)
  (* 	| [] -> "" *)
  (* 	| h::t -> h ^ (if List.length t = 0 then "" else " & ") ^ (conjectClause t) *)
  (* in conjectClause encodedList1 *)
  encodedList1
