(**
 * read a dictionary in memory in trie dataststructure.
 *)

open Trie_alternate
let readdict (filename : string) : string list =
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

module CharMap = Map.Make(Char);;
module TrieM = TrieMake(CharMap);;
let emptyTrie = TrieM.empty;;

let rec str_to_char_list (s : string) : char list =
	match s with
	  | "" -> ['#'] (* Words should be ended with '#' *)
	  | ch -> (String.get ch 0)::(str_to_char_list (String.sub s 1 ((String.length s) - 1)))    
;;

let rec buildTrie (trie : 'a TrieM.t) (lines : string list) : 'a TrieM.t = 
  	match lines with
	  | [] -> trie
	  | h::tl -> buildTrie (TrieM.put trie (str_to_char_list h) true) (tl) 
;;

let getKeyRep = function k -> Char.escaped k;;
let getOpenParenRep = function k -> Char.escaped '(' ;;
let getCloseParenRep  = function k -> Char.escaped ')';;
let getOrRep = function k -> Char.escaped '|';;
let getImplyRep = function k -> Char.escaped '-';;
