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


(* COMPILATION - How to build this module (used by Mooshak))
         ocamlc -c Genealogy.mli Genealogy.ml
*)


(* AUXILIARY BASIC FUNCTIONS - you can add more *)

let rec uniq l =
	match l with
	| [] -> []
	| [x] -> [x]
	| x::y::xs ->
		if x = y then uniq (y::xs)
		else x::uniq (y::xs)

let clean l = (* removes repetitions *)
	uniq (List.sort compare l)

let len =
	List.length

let map =
	List.map

let filter =
	List.filter

let mem =
	List.mem

let flatMap f l =
	List.flatten (map f l)

let partition =
	List.partition

let exists =
	List.exists

let for_all =
	List.for_all

let cFlatMap f l =
	clean (flatMap f l)

let union l1 l2 =
	clean (l1 @ l2)

let inter l1 l2 =
	filter (fun x -> mem x l2) l1

let diff l1 l2 =
	filter (fun a -> not (mem a l2)) l1



(* TYPES *)

type item = string * string list
type repository = item list

type aTree = ANil | ANode of string * aTree * aTree
type dTree = DNil | DNode of string * dTree list


(* EXAMPLES - you can add more *)

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


(* BASIC REPOSITORY FUNCTIONS - you can add more *)

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

(* FUNCTION height *)

let rec height rep =
	match rep with
	| [] -> 0
	| x::xs -> 1 + height (snd (cut xs))
;;

(* FUNCTION makeATree *)

let rec makeATree rep a =
	match parents rep [a] with
	| [] -> ANode (a, ANil, ANil)
	| [p] -> ANode (a, makeATree rep p, ANil)
	| [p1; p2] -> ANode (a, makeATree rep p1, makeATree rep p2)
	| _ -> failwith "ERROR: Has two parents."
;;

(* FUNCTION repOfATree *)

let repOfATree t =
	[]


(* FUNCTION makeDTree *)

let rec makeDTree rep a =
	match children rep [a] with
	| [] -> DNode (a, [])
	| x::xs -> DNode (a, (makeDTree rep x) :: (processChildren rep xs)) 
and processChildren rep cl =
	match cl with 
	| [] -> [] 
	| y::ys -> (makeDTree rep y) :: (processChildren rep ys)
;;

(* FUNCTION repOfDTree *)

let repOfDTree t =
	[]


(* FUNCTION descendantsN *)

let rec descendantsN rep n lst =
	match lst with
	| [] -> []
	| [x] -> if n = 1 then children rep [x] else descendantsN rep (n - 1) (children rep [x])
	| x::xs -> clean ((descendantsN rep n [x]) @ (descendantsN rep n xs))
;;


(* FUNCTION siblings *)

let siblings rep lst =
	let sl = children rep (parents rep lst) in
		match sl with 
		| [] -> lst
		| x -> sl
;;

(* FUNCTION siblingsInbreeding *)

let siblingsInbreeding rep =
	[]


(* FUNCTION waveN *)

let rec waveN rep n lst =
	match n with
	| 0 -> lst
	| 1 -> waveN rep 0 (diff (union (parents rep lst) (children rep lst)) lst)
	| x -> waveN rep (n - 1) (union lst (union (parents rep lst) (children rep lst)))
;;

(* FUNCTION merge *)

let rec mergeList rep1 rep2 ml = 
	match ml with
	| [] -> []
	| x::xs -> (x, union (children rep1 [x]) (children rep2 [x])) :: mergeList rep1 rep2 xs
;;

let rec merge rep1 rep2 = 
	mergeList rep1 rep2 (union (all1 rep1) (all1 rep2))
;;

(* FUNCTION supremum *)

let supremum rep s =
	match s with 
	| [] -> []
	| x::xs -> x 
;;

(* FUNCTION validStructural *)

let validStructural rep =
 false


(* FUNCTION validSemantic *)

let validSemantic rep =
	false


