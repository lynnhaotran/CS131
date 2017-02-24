type volley_nonterminals =
  | Serve | Mistake | Receive | Set | Spike

let volley_grammar =
  (Serve,
   function
     | Serve ->
         [[N Mistake]; [N Receive]]
     | Mistake ->
	 	[[T "Out"]; [T "Net"]; [T "Flag"]]
     | Receive ->
	 	[[N Set]; [N Spike]]
     | Set ->
		[[N Spike]]
     | Spike ->
	 	[[N Mistake]; [T "Point"]; [N Receive] ])

type sentence_nonterminals =
  | Noun | Animal | Article | Verb | Sentence | Subject | Object | Fixed

let sentence_grammar = 
	(Sentence, function
  | Noun -> 
  		[[T "Cat"];[T "Dog"];[T "House"];[T "Car"]]
  | Animal ->
  		[[T "Cat"];[T "Dog"]]
  | Article -> 
  		[[T "The"]; [T "A"]]
  | Verb -> 
  		[[T "Ate"]; [T "Saw"]]
  | Sentence -> 
  		[[N Subject; N Verb; N Object]; 
  		[N Subject; N Verb]; [N Article; N Fixed; N Sentence]; [N Object]]
  | Subject -> 
  		[[N Article; N Noun]]
  | Object -> 
		[[N Article; N Noun]; [N Article; N Animal]]
  | Fixed -> [[T "Cat"; N Verb]])

(*From the spec*)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None

let rec contains_noun = function
  | [] -> false
  | (Noun,_)::_ -> true
  | _::rules -> contains_noun rules

let accept_only_non_noun rules frag =
  if contains_noun rules
  then None
  else Some (rules, frag)

 (*Test where the infinite loop was found originally*)
 let test_0 = (parse_prefix volley_grammar accept_empty_suffix ["Point"]) = Some
	([(Serve, [N Receive]); (Receive, [N Set]); (Set, [N Spike]);
   		(Spike, [T "Point"])], [])

(*Does not prematurely accept; i.e., does not take first rule of Sentence, and
 properly backtracks*)
let test_1 = (parse_prefix sentence_grammar accept_empty_suffix ["The"; "House"; "Ate"]) = 
	Some ([(Sentence, [N Subject; N Verb]); (Subject, [N Article; N Noun]);
   (Article, [T "The"]); (Noun, [T "House"]); (Verb, [T "Ate"])], [] )

(*Avoids rule, but works on an alternative path; nt successfully calls itself*)
let test_2 = (parse_prefix sentence_grammar accept_only_non_noun ["The"; "Cat"; "Ate"; "A"; "Dog"; "House"])
 = Some ([(Sentence, [N Article; N Fixed; N Sentence]); (Article, [T "The"]);
   (Fixed, [T "Cat"; N Verb]); (Verb, [T "Ate"]); (Sentence, [N Object]);
   (Object, [N Article; N Animal]); (Article, [T "A"]); (Animal, [T "Dog"])],
  ["House"])
