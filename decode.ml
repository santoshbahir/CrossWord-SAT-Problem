(* Decode program to decode the puzzle output *)

open Readpuzzle

(* The expected command line to run the program is : *)
(* 	./decode <puzzle name>.puzzle <puzzle name>.ans *)

let () = if Array.length Sys.argv - 1 <> 2 then
		   begin
			 print_string "\nCorrect syntax is :\n";
			 print_string "decode <puzzle name>.puzzle <puzzle name>.ans\n\n" ;
			 exit 0
		   end

(* let progname = Sys.argv.(0) *)
let puzname = Sys.argv.(1)
let ansname = Sys.argv.(2)

(* #use "use.ml";; *)
(* let puzname = "puzzles/micro.puzzle";; *)
(* let ansname = "micro.puzzle.ans";; *)

(* read the puzzle and get the dimensions of it *)
let p1 = readPuzzle puzname;;
let pl1 = puzzleLines p1;;
let pd1 = fst pl1;;

let readAns (filename : string) : string list =
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

let anslst1 = readAns ansname;;

(* Filter out the boolean variables which are either 'false' or 'swithching variables' *)
let anslst2 = 
  let f s =
	let regex = Str.regexp "\\(-\\|xxxx\\)" in
	not (Str.string_match regex s 0)
  in List.filter f anslst1;;

(* convert the string represented boolean variables in tuple of (char, (d1, d2)) *)
let anslst3 = 
  let f s =
	let getDim bv =
	  let us1 = String.index bv '_' in
	  let us2 = String.rindex bv '_' in 
	  let ns1 = String.sub bv (us1 + 1) (us2 - us1 - 1) in
	  let ns2 = String.sub bv (us2 + 1) ((String.length bv) - us2 - 1) in
	  ((int_of_string (String.trim ns1)), (int_of_string (String.trim ns2)))  
	in 
	let getChar bv = String.get bv 0 in
	((getChar s), (getDim s))
  in List.rev (List.rev_map f anslst2);;

(* Filter our all boolean variables lying outside the puzzle dimension *)
let anslst4 = 
  let f t =
	let ph1 = fst pd1 in
	let pv1 = snd pd1 in
	let vh1 = fst (snd t) in
	let vv1 = snd (snd t) in
	(vh1 <= ph1 && vv1 <=pv1)
  in List.filter f anslst3;;

(* Create the list of list where each element list represents the row in the 
puzzle *)
let anslst5 =
  let fcmp d1 d2 =
	let a = snd (snd d1) in
	let b = snd (snd d2) in
	if a < b then -1
	else if a > b then 1
	else 0
  in
  let rec f rn lst =
	let frowmem d1 =
	  if (fst (snd d1)) = rn then true
	  else false
	in 
	if rn = 0 then 
	  []
	else 
	  let part = List.partition frowmem lst in 
	  (List.stable_sort fcmp (fst part))::(f (rn - 1) (snd part))
  in List.rev (f (fst pd1) anslst4);;
  
(* convert the bv in the puzzle string *)
let anslst6 =
  let rec f l =
	match l with
	| [] -> ""
	| h::t -> (Char.escaped (fst h)) ^ (f t)
  in 
  let rec g l =
	match l with
	| [] -> []
	| h::t -> (f h)::(g t)
  in g anslst5;;
 
let rec printAns lst =
  match lst with
  | [] -> ()
  | h::t -> print_string h; print_newline(); printAns t;;

let () = print_int (fst pd1); 
		 print_string " "; 
		 print_int (snd pd1); 
		 print_newline();
  		 printAns anslst6;;

