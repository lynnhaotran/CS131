type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(*Problem 1: convert_grammar returns the start symbol and 
 *an anonymous function (production function) which will take a nonterminal value
 *and pass it to group_rules, which extracts the rules for that value.
 *)

let rec group_rules nt rules_list =
	match rules_list with
	[] -> []
	| (nt1, rule)::t -> if nt = nt1
							then rule::group_rules nt t
						else
							group_rules nt t
;;

let rec convert_grammar gram1 =
	match gram1 with
	(startsymbol, rules_list) -> (startsymbol, (fun x -> group_rules x rules_list))
;;

(*Problem 2
 *1. keep track of rules consumed - recursion
  2. create derivation for the prefix - matcher passes down to check_rule
  3. check if acceptor accepts - matcher
	  if no:
		move to the next rule
	  stop
		when accept accepts - curried
		when the rule list is empty
 *)

(*Matcher takes the rules of a nonterminal and passes them left-to-right to
 *check_rule. It is responsible for keeping track of the derivation list and
 *backtracking, and returns the output of the acceptor.*)
let rec matcher nt gr rlist acc dlist frag =
	match rlist with
	[] -> None 
	| rule::rest -> match check_rule gr rule acc (dlist @ [(nt, rule)]) frag with
					| None -> matcher nt gr rest acc dlist frag
					| x -> x

(*Will recursively call Matcher until a terminal is found for a nonterminal. Has
 *internal acceptors for when we are still checking mid-rule.*)
and check_rule gr rule acc dlist frag =
	match rule with
	[] -> acc dlist frag
	| hd::rst -> match hd with
						| N nxt -> matcher nxt gr (gr nxt) (check_rule gr rst acc) dlist frag 
						| T tmnl -> match frag with
										| [] -> None
										| fhd::frst -> if fhd = tmnl
														then check_rule gr rst acc dlist frst
													   else
													   	None
;;

let parse_prefix (s_symbol, gr) acc frag =
	matcher s_symbol gr (gr s_symbol) acc [] frag
;;
