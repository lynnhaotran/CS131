type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*Helper Functions*)

let rec contains e l = 
	match l with
	[] -> false
	| h1::t -> if (compare e h1 = 0) 
				then true 
			   else 
			   	contains e t
;;

let rec delete_element e lst = 
	match lst with
	[] -> [] 
	| h::t -> if (compare h e = 0) then
					delete_element e t
				else
					h::(delete_element e t)
;;

(*Problem 1*)
let rec subset a b = 
	match a with 
	[] -> true
	| h::t -> if (contains h b = false)
				then false
			  else
			  	subset t b
;;

(*Problem 2*)
let equal_sets a b =
	if compare (List.sort_uniq compare a) (List.sort_uniq compare b) = 0 
		then true
	else 
		false
;;

(*Problem 3*)
let rec set_union a b = 
	match a with
	[] -> List.sort_uniq compare b
	| h::t -> if (contains h b = false) 
				then h::set_union t b
			  else
			  	set_union t b
;;

(*Problem 4*)
let rec set_intersection a b = 
	match a with
	[] -> []
	| h::t -> if (contains h b = true) 
				then h::set_intersection (delete_element h t) b
			  else
			  	set_intersection t b
;;

(*Problem 5*)
let rec set_diff a b =
	match a with
	[] -> []
	| h::t -> if (contains h b = false)
					then h::set_diff (delete_element h t) b
			  else
			  		set_diff t b
;;

(*Problem 6*)
let rec computed_fixed_point eq f x =
	if (eq (f x) x)
		then x
	else
		computed_fixed_point eq f (f x)
;;

let rec compute f p x =
match p with
1 -> f x
| _ -> f (compute f (p-1) x)
;;

(*Problem 7*)
let rec computed_periodic_point eq f p x =
match p with
0 -> x
| _ -> let new_x = compute f p x in 
		if (eq new_x x)
			then new_x
		else
			computed_periodic_point eq f p (f x)
;;

(*Problem 8*)
let rec while_away s p x =
	if (p x = false)
		then []
	else
		x::while_away s p (s x)
;;

(*Problem 9*)
let rec rle_decode lp =
	match lp with
	[] -> []
	| (a,b)::t -> if (a <= 0)
					then rle_decode t
				  else
				  	b::rle_decode (List.cons (a-1, b) t)
;;

(*Helpers for Problem 10*)
(*returns a list of terminal symbols*)
let rec find_terminals lst = 
	match lst with
	[] -> []
	| h::t -> let tail = find_terminals t in
				match snd h with
					[] -> h::tail
					| h1::dc -> match h1 with
									N _ -> tail
									| _ -> if dc = [] 
											then h::tail
										   else tail
;;

(*Return the rules without their N/T types in a list*)
let rec return_types rls =
	match rls with
		[] -> []
		| h::t -> match h with
					T _ -> return_types t
					| N (a) -> a::return_types t
;;

(*Check against the valid rules to see if a nonterminal rule is also valid*)
let rec is_terminal tmls e =
	let nt_discovered = fst (List.split tmls) in
		if subset (return_types (snd e)) nt_discovered = true
			then true
		else
			false
;;

(*If the valid list increased, there's still a chance of finding more valid nonterminals;
 *Otherwise, we've found all the possible non-blind alley rules.
 *)
let rec filtering tmls g =
	let added_terms = List.filter ((is_terminal) tmls) g in
		if List.length added_terms = List.length tmls
			then added_terms
		else 
			filtering added_terms g
;;

(*Problem 10*)
let rec filter_blind_alleys g =
	((fst g), filtering (find_terminals (snd g)) (snd g))
;;