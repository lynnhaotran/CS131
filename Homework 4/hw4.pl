% signal_morse(X, Y) takes a list X of 1s and 0s and converts them to a list Y
% corresponding morse symbols.
% stransfer() is a helper function that counts the number of occurrences of the 
% same symbol for parsing.
% convert outputs each possible option from noisy signals

signal_morse([],[]).
signal_morse([B|BT], [M|MT]) :- translate(B, BT, NBT, M), 
									M \= 0, 
									signal_morse(NBT, MT).
signal_morse([B|BT], MT) :- translate(B, BT, NBT, 0), 
							signal_morse(NBT, MT).

convert(1, '.', N) :- N = 1; N = 2.
convert(1, '-', N) :- N = 2; N >= 3.
convert(0, 0, N) :- N = 1; N = 2.
convert(0, '^', N) :- N = 2; N = 3; N = 4; N = 5.
convert(0, '#', N) :- N >= 5.

translate(B, BT, NBT, M) :- stransfer(B, BT, NBT, N), convert(B, M, N).

stransfer(_, [], [], 1).
stransfer(X, [Y|Ys], [Y|Ys], 1) :- X \= Y.
stransfer(X, [X|Xs], Ys, N) :- stransfer(X, Xs, Ys, N1), N is N1+1.

% signal_message(X, Y) takes a list X of 1s/0s and converts them to a list Y
% corresponding to the decoded morse equivalent, outputting all options
% from ambiguous signals. It first converts to morse, decodes, then filters
% for possible error cases.

signal_message(B, D) :- signal_morse(B, M), 
						morse_interpret(M, ED), 
						filter_errors(ED, D).

% morse_interpret() divides the list by letter to decode; words are divided by
% a hash symbol.
morse_interpret([], []).
morse_interpret(['#'|MT], ['#'|DT]) :- morse_interpret(MT, DT).
morse_interpret([M|MT], [D|DT]) :- letter_find([M|MT], R, X2), 
									morse(D, R), 
									morse_interpret(X2, DT).

letter_find([], [], []).
letter_find(['^'|MT], [], MT).
letter_find(['#'|MT], [], ['#'|MT]).
letter_find([M|MT], [M|DT], R) :- M \= (^), M \= '#', letter_find(MT, DT, R).

% filter_errors() cuts words that are followed by an error token.
% The list is consumed by build_words, which creates a list of the current 
% partition, which is then checked by if_error to see if it needs to be
% cut from the final solution. rebuild appends all of the remaining sequences.

filter_errors([], []).
filter_errors([M|MT], NM) :- build_word([M|MT], L, R),
						if_error(R, L, NM1, R2),
						filter_errors(R2, NM2),
						rebuild(NM1, NM2, NM).

build_word([], [], []).
build_word([error|MT], [], [error|MT]).
build_word(['#'|MT], ['#'], MT).
build_word([M|MT], [M|DT], R) :- M \= '#', M \= error, build_word(MT, DT, R).

if_error([], L, L, []).
if_error([error| T], [], [error], T).
if_error([error| T], ['#'], ['#', error], T). %special case - no word
if_error([error| T], L, [], T) :- L \= [], L \= ['#'].
if_error([R|RT], L, L, [R|RT]) :- R \= error.

rebuild([], L, L).
rebuild([H|T], L, [H|LT]) :- rebuild(T, L, LT).

morse(a, [.,-]).           % A
morse(b, [-,.,.,.]).	   % B
morse(c, [-,.,-,.]).	   % C
morse(d, [-,.,.]).	   % D
morse(e, [.]).		   % E
morse('e''', [.,.,-,.,.]). % É (accented E)
morse(f, [.,.,-,.]).	   % F
morse(g, [-,-,.]).	   % G
morse(h, [.,.,.,.]).	   % H
morse(i, [.,.]).	   % I
morse(j, [.,-,-,-]).	   % J
morse(k, [-,.,-]).	   % K or invitation to transmit
morse(l, [.,-,.,.]).	   % L
morse(m, [-,-]).	   % M
morse(n, [-,.]).	   % N
morse(o, [-,-,-]).	   % O
morse(p, [.,-,-,.]).	   % P
morse(q, [-,-,.,-]).	   % Q
morse(r, [.,-,.]).	   % R
morse(s, [.,.,.]).	   % S
morse(t, [-]).	 	   % T
morse(u, [.,.,-]).	   % U
morse(v, [.,.,.,-]).	   % V
morse(w, [.,-,-]).	   % W
morse(x, [-,.,.,-]).	   % X or multiplication sign
morse(y, [-,.,-,-]).	   % Y
morse(z, [-,-,.,.]).	   % Z
morse(0, [-,-,-,-,-]).	   % 0
morse(1, [.,-,-,-,-]).	   % 1
morse(2, [.,.,-,-,-]).	   % 2
morse(3, [.,.,.,-,-]).	   % 3
morse(4, [.,.,.,.,-]).	   % 4
morse(5, [.,.,.,.,.]).	   % 5
morse(6, [-,.,.,.,.]).	   % 6
morse(7, [-,-,.,.,.]).	   % 7
morse(8, [-,-,-,.,.]).	   % 8
morse(9, [-,-,-,-,.]).	   % 9
morse(., [.,-,.,-,.,-]).   % . (period)
morse(',', [-,-,.,.,-,-]). % , (comma)
morse(:, [-,-,-,.,.,.]).   % : (colon or division sign)
morse(?, [.,.,-,-,.,.]).   % ? (question mark)
morse('''',[.,-,-,-,-,.]). % ' (apostrophe)
morse(-, [-,.,.,.,.,-]).   % - (hyphen or dash or subtraction sign)
morse(/, [-,.,.,-,.]).     % / (fraction bar or division sign)
morse('(', [-,.,-,-,.]).   % ( (left-hand bracket or parenthesis)
morse(')', [-,.,-,-,.,-]). % ) (right-hand bracket or parenthesis)
morse('"', [.,-,.,.,-,.]). % " (inverted commas or quotation marks)
morse(=, [-,.,.,.,-]).     % = (double hyphen)
morse(+, [.,-,.,-,.]).     % + (cross or addition sign)
morse(@, [.,-,-,.,-,.]).   % @ (commercial at)

% Error.
morse(error, [.,.,.,.,.,.,.,.]). % error - see below

% Prosigns.
morse(as, [.,-,.,.,.]).          % AS (wait A Second)
morse(ct, [-,.,-,.,-]).          % CT (starting signal, Copy This)
morse(sk, [.,.,.,-,.,-]).        % SK (end of work, Silent Key)
morse(sn, [.,.,.,-,.]).          % SN (understood, Sho' 'Nuff)