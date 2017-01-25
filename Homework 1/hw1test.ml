let my_subset_test0 = subset [(2, [1; 2; 3]); (2, [1; 2; 3])] [(1, []); (2, [1; 2; 3]); (3, [1; 3])]
let my_subset_test1 = subset [] []
let my_subset_test3 = not (subset [3; 3; 3; 3] [1; 2])

let my_equal_sets_test0 = equal_sets [(1, 3); (3, 5); (1, 3)] [(3, 5); (1, 3)]
let my_equal_sets_test1 = equal_sets [] []
let my_equal_sets_test2 = not (equal_sets [(2, 1)] [(1, 2)])

let my_set_union_test0 = equal_sets (set_union [3; 1; 2; 3] [4; 3; 4]) [1; 2; 3; 4]

let my_set_intersection_test0 = equal_sets (set_intersection [1; 3] [10; 12]) []
let my_set_intersection_test1 = equal_sets (set_intersection [(1, 2); (2, 3); (1, 2); (2, 3)] [(2, 3); (2, 3); (1, 2)]) [(1, 2); (2, 3)]
let my_set_intersection_test2 = equal_sets (set_intersection [] []) []

let my_set_diff_test0 = equal_sets (set_diff [1; 4; 7; 2; 1] [3; 4; 7]) [1; 2]
let my_set_diff_test1 = equal_sets (set_diff [] [3; 4; 7]) []
let my_set_diff_test2 = equal_sets (set_diff [1] []) [1]

let computed_fixed_point_test0 =
  ((computed_fixed_point (fun x y -> x + y < 0)
			 (fun x -> x - 2)
			 100)
   = 0)

let computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x * -1) 2 (-1) = -1;;

let my_while_away_test0 = equal_sets (while_away ((-) 0) ((>) 0) 10) []

let my_rle_decode_test0 = equal_sets (rle_decode [3, (1, 2); 0, (2, 3)]) [(1, 2); (1, 2); (1, 2)]
let my_rle_decode_test1 = equal_sets (rle_decode [-1, "hello"]) []


type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

let my_filter_blind_alleys_test0 =
	filter_blind_alleys (Conversation, [Snore, []]) = (Conversation, [Snore, []])
let my_filter_blind_alleys_test1 =
	filter_blind_alleys (Conversation, []) = (Conversation, [])