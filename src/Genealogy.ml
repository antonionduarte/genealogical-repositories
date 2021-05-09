(* Genealogy module body *)

(* 
Student 1: Goncalo Virginia 56773 
Student 2: Antonio Duarte 58278 

All the methods in the project have been made.
*)

(* Compilation - How to build this module (used by Mooshak))
         ocamlc -c Genealogy.mli Genealogy.ml *)

(* Auxiliary Basic Functions - you can add more *)

let rec uniq l =
	match l with
	| [] -> []
	| [x] -> [x]
	| x::y::xs ->
		if x = y then uniq (y::xs)
		else x::uniq (y::xs)
;;

let clean l = (* removes repetitions *)
	uniq (List.sort compare l)
;;

let len =
	List.length
;;

let map =
	List.map
;;

let filter =
	List.filter
;;

let mem =
	List.mem
;;

let flatMap f l =
	List.flatten (map f l)
;;

let partition =
	List.partition
;;

let exists =
	List.exists
;;

let for_all =
	List.for_all
;;

let cFlatMap f l =
	clean (flatMap f l)
;;

let union l1 l2 =
	clean (l1 @ l2)
;;

let inter l1 l2 =
	filter (fun x -> mem x l2) l1
;;

let diff l1 l2 =
	filter (fun a -> not (mem a l2)) l1
;;


(* Types *)

type item = string * string list
type repository = item list

(*
	Ancestors tree,
	A binary tree, which contains all the ancestors of a specific element.
 *)
type aTree = ANil | ANode of string * aTree * aTree

(*
	Descendants tree,
	A N-Nary tree, which contains all the descendants of a specific element. 
*)
type dTree = DNil | DNode of string * dTree list


(* Example Repositories - you can add more *)

let example = [
		("a", ["f";"g"]); 
		("b", ["f";"h"]);
   	("c", ["h";"i"]);
   	("f", ["j"; "g"]);
  	("g", ["j"]);
   	("h", []);
   	("i", []);
    ("j", [])
]											

(* Basic repository functions - you can add more *)

let size rep = (* number of individuals *)
	len rep
;;

let all1 rep = (* all the individuals *)
	map fst rep
;;

let all2 rep = (* all the children (of anyone) *)
	cFlatMap snd rep
;;

let roots rep = (* individuals without any parents *)
	diff (all1 rep) (all2 rep)
;;

let inners rep = (* individuals with children *)
	let xs = filter (fun (p,cs) -> cs <> []) rep in
		all1 xs
;;

let leaves rep = (* individuals without any children *)
	let xs = filter (fun (p,cs) -> cs = []) rep in
		all1 xs
;;

let cut1 rep l = (* partition based on first component of the repository *)
	partition (fun (p,cs) -> mem p l) rep
;;

let cut2 rep l = (* partition based on second component of the repository *)
	partition (fun (p,cs) -> inter cs l <> []) rep
;;
	
let cut rep = (* partition -> (root pairs, rest pairs) *)
	cut1 rep (roots rep)
;;

let children rep l = (* get all the children of the list l *)
	let (a,b) = cut1 rep l in
		all2 a
;;

let rec parents rep l = (* get all the parents of the list l *)
	let (a,b) = cut2 rep l in
		all1 a
;;

let rec aTreeToList t = (* converts an atree to a list *)
	match t with
		| ANil -> []
		| ANode (x, p1, p2) -> (x::aTreeToList p1) @ (aTreeToList p2)
;;

let rec distToRoots rep elem = (* determines the distance between an elem and the roots *)
	if mem elem (roots rep) then 0
	else 1 + distToRoots (snd (cut rep)) elem
;;

let rec maxDistRoots rep lst = (* returns max distance to roots of elems in list *)
	match lst with
	| [] -> 0
	| [x] -> distToRoots rep x 
	| x::xs -> let mxs = maxDistRoots rep xs in
									let dx = distToRoots rep x in
											if dx >= mxs then dx
											else mxs
;;

(* Primary Functions *)

(* Function - height *)

let rec height rep =
	match rep with
	| [] -> 0
	| x -> 1 + height (snd (cut x))
;;

(* Function - merge *)

let rec mergeList rep1 rep2 ml = 
	match ml with
	| [] -> []
	| x::xs -> (x, union (children rep1 [x]) (children rep2 [x])) :: mergeList rep1 rep2 xs
;;

let rec merge rep1 rep2 = 
	mergeList rep1 rep2 (union (all1 rep1) (all1 rep2))
;;

(* Function - makeATree *)

let rec makeATree rep a =
	match parents rep [a] with
	| [] -> ANode (a, ANil, ANil)
	| [p] -> ANode (a, makeATree rep p, ANil)
	| [p1; p2] -> ANode (a, makeATree rep p1, makeATree rep p2)
	| _ -> failwith "ERROR: Node has more than two parents."
;;

(* Function - repOfATree *)

let rec repOfATree t =
	match t with
	| ANil -> []
	| ANode (c, p1, p2) -> 
			match p1, p2 with
			| ANil, ANil -> [(c, [])]
			| ANode (n, _, _), ANil -> merge [(c, []); (n, [c])] (repOfATree p1)
			| ANil, ANode (n, _, _) -> merge [(c, []); (n, [c])] (repOfATree p2)
			| ANode (n1, _, _), ANode (n2, _, _) -> 
					merge ([(c, []); (n1, [c]); (n2, [c])]) (merge (repOfATree p1) (repOfATree p2))
;;

(* Function - makeDTree *)

let rec makeDTree rep a =
	match children rep [a] with
	| [] -> DNode (a, [])
	| x::xs -> DNode (a, (makeDTree rep x) :: (processChildren rep xs)) 
and processChildren rep cl =
	match cl with 
	| [] -> [] 
	| y::ys -> (makeDTree rep y) :: (processChildren rep ys)
;;

(* Function - repOfDTree *)

let rec concatChildren tl =  (* concatenates children list into a list *)
	match tl with
	| [] -> []
	| DNil::_ -> []
	| DNode (x, _) :: xs -> x :: concatChildren xs 
;;

let rec repOfDTree t =
	match t with
	| DNil -> []
	| DNode (x, y) -> (x, concatChildren y) :: (repChildren y)
and repChildren cl = 
	match cl with
	| [] -> []
	| dt::dts -> merge (repOfDTree dt) (repChildren dts)
;;

(* Function - descendantsN *)

let rec descendantsN rep n lst =
	if n = 0 then lst
	else descendantsN rep (n - 1) (children rep lst)
;;

(* Function - siblings *)

let siblings rep lst =
	let sl = children rep (parents rep lst) in
		union lst sl	
;;

(* Function - siblingsInbreeding *)

let rec cleanTuples lst = (* cleans repeated tuples *)
	match lst with
	| [] -> []
	| (x, y)::xs -> if mem (y, x) xs then cleanTuples xs
									else (x, y) :: (cleanTuples xs)
;;

let rec joinPairs rep sib elem = (* joins an element with it's siblings in tuples *)
	match sib with
	| [] -> []
	| x::xs -> if x = elem then joinPairs rep xs elem
						 else (elem, x) :: (joinPairs rep xs elem)
;;

let rec generatePairs rep possible_ib = (* generates all pairs of siblings in rep *)
	match possible_ib with
	| [] -> []
	| x::xs -> (joinPairs rep (siblings rep [x]) x) @ (generatePairs rep xs)
;;

let rec verifyIfBreeding rep pairs = (* verifies if pairs of siblings have common children *)
	match pairs with
	| [] -> []
	| (x, y)::xs -> if (inter (children rep [x]) (children rep [y])) <> []
								  then (x, y) :: verifyIfBreeding rep xs
									else verifyIfBreeding rep xs
;;

let siblingsInbreeding rep =
	let possible_ib = diff (inners rep) (roots rep) in
		verifyIfBreeding rep (cleanTuples (generatePairs rep possible_ib))
;;

(* Function - waveN *)

let rec waveN rep n lst =
	match n with
	| 0 -> lst
	| 1 -> waveN rep 0 (diff (union (parents rep lst) (children rep lst)) lst)
	| x -> waveN rep (n - 1) (union lst (union (parents rep lst) (children rep lst)))
;;

(* Function - supremum *)

let rec getAncestors rep elem = (* gets all the ancestors of an element *)
	diff (clean (aTreeToList (makeATree rep elem))) [elem]
;;

let rec sharedAncestors rep lst =  (* gets a list of all shared ancestors of all elems in lst *)
	match lst with
	| [] -> []
	| [x] -> getAncestors rep x 
	| x::y::xs -> inter (getAncestors rep x) (sharedAncestors rep (y::xs)) 
;;

let rec elemsAtDist rep lst dist = (* filters elements that are at dist from roots *)
	filter (fun x -> (distToRoots rep x) = dist) lst 
;;

let supremum rep s = 
	let sa = sharedAncestors rep s in
		elemsAtDist rep sa (maxDistRoots rep sa)
;; 

(* Function - validStructural *)

let rec noDuplicates lst = (* verifies no duplicates condition in the first component of rep *)
	match lst with 
	| [] -> true
	| x::xs -> (not (mem x xs)) && noDuplicates xs
;;

let rec occursInFirst all lst = (* verifies if all elements occur in first component of rep *)
	match lst with
	| [] -> true
	| x::xs -> (mem x (all)) && occursInFirst all xs
;;

let validStructural rep = 
	let all_elems = all1 rep in
		(noDuplicates (all_elems)) && (occursInFirst (all_elems) (all2 rep))
;; 

(* Function - validSemantic *)

(*
	This function was specifically difficult, because there are a few edge cases
	where the verification for ancestors causes a looping recursion.
	So we need to keep a list of all the already visited parents, in order to check
	if we already visited them.
*)

let rec checkElem rep visited parent = (* checks if there is a loop for an element *)
	if [parent] = [] then true
	else if ((inter visited [parent]) <> []) then false
	else for_all (fun x -> checkElem rep (parent::visited) x) (parents rep [parent])
;;

let rec ancestorsItself rep lst = (* verifies if an element is not ancestor of itself *)
	match lst with
	| [] -> true 
	| x::xs -> (for_all (fun x -> checkElem rep [] x) (parents rep [x]))
						 && ancestorsItself rep xs
;;

let twoParents rep = (* verifies if an element does not have more than two parents *)
	for_all (fun x -> (len (parents rep [x])) <= 2) (all1 rep)
;;

let validSemantic rep =
	(ancestorsItself rep (all1 rep)) && (twoParents rep)
;;
