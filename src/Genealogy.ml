(* Genealogy module body *)

(* 
Aluno 1: Goncalo Virginia 56773 
Aluno 2: Antonio Duarte 58278 

Comment:

?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????
?????????????????????????

*)

(*
0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
   100 columns
*)


(* Compilation - How to build this module (used by Mooshak))
         ocamlc -c Genealogy.mli Genealogy.ml
*)


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

let example2 = [
    ("a",["d";"e"]);
    ("b",["e";"f"]);
    ("c",["g";"h"]);
    ("d",["i"]);
    ("e", ["i";"j";"d"]);
    ("f", ["g";"m"]);
    ("g", ["h";"m"]);
    ("h", ["n"]);
    ("i", ["j"]);
    ("j", ["k"]);
    ("k", []);
    ("m", ["k"; "n"]);
    ("n", [])
]

let example3 = [
    ("a", ["e"]);
    ("b", ["e"; "f"]);
    ("c", ["f"; "g"]);
    ("d", ["g"; "m"]);
    ("e", ["j"; "h"]);
    ("f", []);
    ("g", ["i"; "l"]);
    ("m", ["l"]);
    ("j", []);
    ("h", []);
    ("i", []);
    ("l", [])
]

let ultimateexample = [
		("a",["g";"h"]);
		("b",["h";"i"]);
		("c",["i";"j";"k"]);
		("d",["k";"l"]);
		("e",["l";"m"]);
		("f",["m";"n"]);
		("g",["o";"p"]);
		("h",["q";"r"]);
		("i",["s";"t"]);
		("j",["u"]);
		("k",["v";"w"]);
		("l",["x";"y"]);
		("m",["y";"z"]);
		("n",["z"]);
		("o",["p";"2";"1"]);
		("p",["q";"2"]);
		("q",["3"]);
		("r",["4";"5"]);
		("s",["r";"6";"t";"4"]);
		("t",["6";"u"]);
		("u",["v";"8"]);
		("v",[]);
		("w",["9";"19"]);
		("x",["10"]);
		("y",["11";"x"]);
		("z",["11"]);
		("1",["12";"13"]);
		("2",["1";"12";"13"]);
		("3",["15"]);
		("4",["3";"5";"16"]);
		("5",["17"]);
		("6",["7";"17"]);
		("7",["18"]);
		("8",["7";"19"]);
		("9",["20"]);
		("10",["9";"21"]);
		("11",["10";"21"]);
		("12",["14"]);
		("13",["14"]);
		("14",["15"]);
		("15",["16"]);
		("16",[]);
		("17",[]);
		("18",[]);
		("19",["18";"20"]);
		("20",["22"]);
		("21",["22"]);
		("22",[]);
]

let example5 = [
    ("o",[]);
    ("a",["d";"e"]);
    ("d",["i"]);
    ("e",["d";"i"]);
    ("i",["l";"o"]);
    ("l",["a"]);
]    

let example4 = [
    ("a",["d";"e"]);
    ("b",["e";"f"]);
    ("c",["g";"h"]);
    ("d",["i"]);
    ("e", ["i";"j";"d"]);
    ("loop trigger", []);
    ("f", ["g";"m";"loop trigger"]);
    ("g", ["h";"m"]);
    ("h", ["n"]);
    ("i", ["j"]);
    ("j", ["k"]);
    ("k", ["f"]);
    ("m", ["k"; "n"]);
    ("n", [])
]

let example6 = [
    ("g",[]);
    ("a",["f";"g"]);
    ("f",["a"])
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

(* Function height *)

let rec height rep =
	match rep with
	| [] -> 0
	| x -> 1 + height (snd (cut x))
;;

(* Function merge *)

let rec mergeList rep1 rep2 ml = 
	match ml with
	| [] -> []
	| x::xs -> (x, union (children rep1 [x]) (children rep2 [x])) :: mergeList rep1 rep2 xs
;;

let rec merge rep1 rep2 = 
	mergeList rep1 rep2 (union (all1 rep1) (all1 rep2))
;;

(* Function makeATree *)

let rec makeATree rep a =
	match parents rep [a] with
	| [] -> ANode (a, ANil, ANil)
	| [p] -> ANode (a, makeATree rep p, ANil)
	| [p1; p2] -> ANode (a, makeATree rep p1, makeATree rep p2)
	| _ -> failwith "ERROR: Node has more than two parents."
;;

(* Function repOfATree *)

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

(* Function makeDTree *)

let rec makeDTree rep a =
	match children rep [a] with
	| [] -> DNode (a, [])
	| x::xs -> DNode (a, (makeDTree rep x) :: (processChildren rep xs)) 
and processChildren rep cl =
	match cl with 
	| [] -> [] 
	| y::ys -> (makeDTree rep y) :: (processChildren rep ys)
;;

(* Function repOfDTree *)

let rec concatChildren tl = 
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


(* Function descendantsN *)

let rec descendantsN rep n lst =
	match lst with
	| [] -> []
	| [x] -> if n = 1 then children rep [x] else descendantsN rep (n - 1) (children rep [x])
	| x::xs -> clean ((descendantsN rep n [x]) @ (descendantsN rep n xs))
;;

(* Function siblings *)

let siblings rep lst =
	let sl = children rep (parents rep lst) in
		union lst sl	
;;

(* Function siblingsInbreeding *)

let rec cleanTuples lst =
	match lst with
	| [] -> []
	| (x, y)::xs -> if mem (y, x) xs then cleanTuples xs
									else (x, y) :: (cleanTuples xs)
;;

let rec joinPairs rep sib elem = 
	match sib with
	| [] -> []
	| x::xs -> if x = elem then joinPairs rep xs elem
						 else (elem, x) :: (joinPairs rep xs elem)
;;

let rec generatePairs rep possible_ib =
	match possible_ib with
	| [] -> []
	| x::xs -> (joinPairs rep (siblings rep [x]) x) @ (generatePairs rep xs)
;;

let rec verifyIfBreeding rep pairs = 
	match pairs with
	| [] -> []
	| (x, y)::xs -> if (inter (children rep [x]) (children rep [y])) <> []
								  then (x, y) :: verifyIfBreeding rep xs
									else verifyIfBreeding rep xs
;;

let siblingsInbreeding rep =
	let possible_ib = diff (diff (all1 rep) (leaves rep)) (roots rep) in
		verifyIfBreeding rep (cleanTuples (generatePairs rep possible_ib))
;;

(* Function waveN *)

let rec waveN rep n lst =
	match n with
	| 0 -> lst
	| 1 -> waveN rep 0 (diff (union (parents rep lst) (children rep lst)) lst)
	| x -> waveN rep (n - 1) (union lst (union (parents rep lst) (children rep lst)));;

(* Function supremum *)

let rec getAncestors rep elem =
	diff (clean (aTreeToList (makeATree rep elem))) [elem]
;;

let rec sharedAncestors rep lst = 
	match lst with
	| [] -> []
	| [x] -> getAncestors rep x 
	| x::y::xs -> inter (getAncestors rep x) (sharedAncestors rep (y::xs)) 
;;

let rec elemsAtDist rep lst dist =
	match lst with
	| [] -> []
	| x::xs -> if (distToRoots rep x) = dist then x :: elemsAtDist rep xs dist
						 else elemsAtDist rep xs dist
;;

let supremum rep s = 
	let sa = sharedAncestors rep s in
		elemsAtDist rep sa (maxDistRoots rep sa)
;; 

(* Function validStructural *)

(* let validStructural rep = *)
(* 	rep = clean (map (fun (x, _) -> x) rep) && all1 rep = all2 rep *)
(* ;; *)

(* Function validSemantic *)

let rec checkLoop elem curr rep = 
	match curr with
	| [] -> true
	| x::xs -> let ps = parents rep [x] in
					if mem elem ps then false
					else checkLoop elem ps rep && checkLoop elem xs rep

let rec toCheck elems rep =
	match elems with
	| [] -> true
	| x::xs -> if (not (checkLoop (fst x) [fst x] rep)) || len (parents rep [fst x]) > 2 then false
				else toCheck xs rep

let rec validSemantic rep = toCheck rep rep;;
