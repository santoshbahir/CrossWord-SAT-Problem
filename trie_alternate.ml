(**
 *  overview: trie, sometimes called prefix tree, is an efficient data structure 
 *  for storing dictionary. It is map with ordered pair (hashkey, value) with
 *  common prefix of all hashkeys stored only once. Another way to think of trie
 *  is to think of it as a tree with common prefix at root and all the different
 *  suffix resulting in branches.
 *)
module type TRIE = sig
	
	(**
	 *  Type representing hashkey.
     * 
	 *  Note: Better way to handle hashkey is to have two types.
     *  1) For hashkey itself say, hashkey.
	 *  2) For the unit from which hashkey is made up of.
	 *     Example: digit is key if hashkey is integer. Character is k if hashkey
	 *              is string.
	 *)
	type key
	  
	(**
	 * ADT representing TRIE. Each node can have optional value part and 
	 * necessary (key,value) part. The value in (key, value) part is of type
	 * 'a t
	 *)
	type 'a t 
	exception NotFound

	val empty: 'a t
	val put: 'a t -> key list -> 'a -> 'a t
	val get: 'a t -> key list -> 'a 
	val size: 'a t -> int

	(**
	 * cursor identifies the current node in the t. It helps to navigate
	 * through t.
	 *)
	type 'a cursor
	exception ValueExists

	val cursor: 'a t -> key list -> 'a cursor
	val advance: 'a cursor -> key list -> 'a cursor option
	val getc: 'a cursor -> 'a option
	val putc: 'a t -> 'a cursor -> key list -> 'a -> 'a t

	val getKeyRep : (key -> 'b) -> key -> 'b

	val f : (key -> string) -> key -> (char * int * int) -> 'a t -> string

	val fencpuz : (key -> string) -> key -> (char * int * int) -> 'a t -> string
end

(**
 * A functor which takes Map module as input and returns the Trie modules as
 * output.
 *)

module type TRIEFUNCTOR = functor (M : Map.S) -> TRIE 
  with type key = M.key;;

module TrieMake : TRIEFUNCTOR =  functor (M : Map.S) -> struct
	type key = M.key
	type 'a t = Node of 'a option * 'a t M.t

	exception NotFound
	exception ValueExists

	let empty = Node (None, M.empty)

	let rec put = function tr -> function key -> function value ->
				let Node (v, s) = tr in
				match key with
				  | [] -> Node (Some(value), M.empty)
				  | h::tl -> let mval = ( 
					try 
						M.find h s
					with Not_found ->
						empty
				) in Node(v, M.add h (put mval tl value) s)

	let rec get = function tr -> function key ->
		let Node(v, s) = tr in
		match key with
		    [] -> (
			  match v with 
				| (Some value) -> value
				| None -> raise NotFound
			)
		  | h::tl ->
		  		if M.mem h s then get (M.find h s) tl
				else raise NotFound  

	let size tr =
		let Node (ft,sd) = tr in
			let rec fcnt = function key -> function tval -> function ac ->
				let Node (f, s) = tval in
				let cnt = (match f with | Some v -> ac + 1 | None -> ac) in
				if s = M.empty then cnt
				else M.fold fcnt s cnt 
			in M.fold fcnt sd 0

	type 'a cursor = key list * 'a t

	(* val cursor: 'a t -> key list -> 'a cursor *)
	let cursor = function tr -> function k ->
	  let rec cur = function tr -> function k1 ->
		let Node (f, s) = tr in
		if s = M.empty then tr
		else
			match k with
			[] -> tr
			| h::tl ->
				try cur (M.find h s) tl
				with Not_found -> Node(None, M.empty)
	  in (k, (cur tr k))

	(* val advance: 'a cursor -> key -> 'a cursor option *)
	let advance = function c -> function k ->
	  let (k1, tr) = c in 
	  let Node (f, s) = tr in
		try Some ((List.append k1 k), (M.find (List.hd k) s))
		with Not_found -> None

	(* val getc: 'a cursor -> 'a option *)									
	let getc = function c -> 
	  let (k, tr) = c in
	  let Node (f, s) = tr in
	  f

	(* val putc: 'a t -> 'a cursor -> key -> 'a -> 'a t *)
	let putc = function tr -> function cur -> function k -> function v ->
	  let (k1, t1) = cur in
	  let v1 = 
		(try let v2 = get t1 k  in Some v2
		 with NotFound -> None)
	  in match v1 with
		| Some (-) -> raise ValueExists
		| None -> put tr (List.append k1 k) v

	(* val keyOp : (key -> 'b) -> key -> 'b *)
	let getKeyRep = function f -> function k ->  f k

	let f = function h -> function k -> function (dir, xc, yc) -> function tr ->
      let constructKey = function h -> function k -> function (dir, xc, yc) ->
	    getKeyRep h k ^ "_" ^ (string_of_int xc) ^ "_" ^ (string_of_int yc)
	 in	
	 let incDim = function (dir, xc, yc) ->
	   if dir = 'H' then (dir, xc + 1, yc)
	   else (dir, xc, yc + 1)
	 in
	 let wordOnPath = function h -> function k -> function (dir, xc, yc) -> function elem ->
       let e = (match elem with | Some (_) -> true | None -> false) in
	   if e then " ( " ^ (constructKey h k (dir, xc, yc)) ^ " ) | " 
	   else ""
	 in
	  let Node (f, s) = tr in  
	  let rec g = function k -> function (dir, xc, yc) -> function tr ->
		let  Node (f, s) = tr in 
		if M.is_empty s then 
		  constructKey h k (dir, xc, yc)
		else 
		  let keyval = M.bindings s in
		  let rec getList listOfTup = 
			match listOfTup with
			| [] -> ""
			| h1 :: [] -> (g (fst h1) (incDim (dir, xc, yc)) (snd h1))
			| h1::t -> (g (fst h1) (incDim(dir, xc, yc)) (snd h1)) ^ " | " ^ 
						   (getList t)
		  in (wordOnPath h k (dir, xc, yc) f) 
			 ^ " ( " ^ (constructKey h k (dir, xc, yc)) 
			 ^ " & ( "  ^ (getList keyval) ^ " ) ) "
	  in 
	  if M.is_empty s then
		""
	  else g k (dir, xc, yc) tr 

(*
 * Alternate implementation of the encoding of the puzzle in the dictionary.
 *)
	let fencpuz = function fkeyRep -> function k -> function (dir, xc, yc) -> function tr ->
      let constructKey = function fkeyRep -> function k -> function (dir, xc, yc) ->
	    getKeyRep fkeyRep k ^ "_" ^ (string_of_int xc) ^ "_" ^ (string_of_int yc)
	 in	
	 let incDim = function (dir, xc, yc) ->
	   if dir = 'H' then (dir, xc + 1, yc)
	   else (dir, xc, yc + 1)
	 in
	 (* let wordOnPath = function fkeyRep -> function k -> function (dir, xc, yc) -> function elem -> *)
     (*   let e = (match elem with | Some (_) -> true | None -> false) in *)
	 (*   if e then " ( " ^ (constructKey fkeyRep k (dir, xc, yc)) ^ " ) | "  *)
	 (*   else "" *)
	 (* in *)
	 let rec getStrKeyDisjunct = function keyList -> function (dir, xc, yc) ->
	   match keyList with
		 | [] -> ""
		 | h::t -> let appendOR = if List.length t = 0 then "" else " | " in
					(constructKey fkeyRep h (dir, xc, yc)) ^ appendOR ^ (getStrKeyDisjunct t (dir, xc, yc))
	 in 
	  let Node (f, s) = tr in  
	  let rec fencpuzlocal = function prefix -> function k -> function (dir, xc, yc) -> function tr ->
		let  Node (f, s) = tr in 
		if M.is_empty s then 
		  ""
		else 
		  let keyval = M.bindings s in
		  let keyList = fst (List.split keyval) in
		  let strKeyList = getStrKeyDisjunct keyList (incDim (dir, xc, yc)) in
		  let strConst = let appendAND1 = if String.length prefix = 0 then "" else " & "  in

						 let isLeaf = if List.length keyList = 1 &&
									  (fkeyRep (List.hd keyList)) = "#" then
										true
									  else 
										false 
						 in
						 let appendAND2 = if isLeaf then "" else "&" in
						 
						 let strConstLocal = 
						 " ( " ^ prefix ^ appendAND1 ^ 
						   (constructKey fkeyRep k (dir, xc, yc)) ^
						   " -> ( " ^ strKeyList ^ " ) " ^ " ) " ^ appendAND2 
						 in
						 (strConstLocal, isLeaf) in
		  let rec getList listOfTup = 
			match listOfTup with
			| [] -> ""
			| h1::t -> let key1 = fkeyRep (fst h1) in
			   let appendAND1 = if List.length t = 0 || key1 = "#" then "" else " & " in
					   let appendAND2 = if String.length prefix = 0 then "" else " & " 
					   in
					   let appPrefix = prefix ^ appendAND2 ^ (constructKey fkeyRep k (dir, xc, yc)) in
			   (fencpuzlocal appPrefix (fst h1) (incDim(dir, xc, yc)) (snd h1)) ^ 
				 appendAND1 ^ (getList t)
		  in 
		  let str = fst strConst in
		  let isLeaf = snd strConst in
		  let goDeep = if isLeaf then "" else " ( " ^ getList keyval ^ " ) " in
		  str ^ goDeep 
	  in 
	  if M.is_empty s then
		""
	  else fencpuzlocal  "" k (dir, xc, yc) tr 
end;;
