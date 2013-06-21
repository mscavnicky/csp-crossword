:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(codesio)).

% Model 1

letter(97, 'a').  
letter(98, 'b').  
letter(99, 'c').  
letter(100, 'd'). 
letter(101, 'e'). 
letter(102, 'f'). 
letter(103, 'g'). 
letter(104, 'h'). 
letter(105, 'i'). 
letter(106, 'j'). 
letter(107, 'k'). 
letter(108, 'l'). 
letter(109, 'm'). 
letter(110, 'n'). 
letter(111, 'o'). 
letter(112, 'p'). 
letter(113, 'q'). 
letter(114, 'r'). 
letter(115, 's'). 
letter(116, 't'). 
letter(117, 'u'). 
letter(118, 'v'). 
letter(119, 'w'). 
letter(120, 'x'). 
letter(121, 'y'). 
letter(122, 'z'). 

% matrix(+M, +N, -Matrix) 
%   Generates matrix of MxN variables.
matrix(M, N, Matrix):- 
    length(Matrix, M), 
    maplist(length_rev(N), Matrix).

% length_rev(?Length, ?List)
%   Reverted length predicate to conform to maplist predicate.
length_rev(Length, List) :- 
    length(List, Length).

% concat_all(+Lists, -List)
%   Concatenates nested lists into a single list.
concat_all([], []).
concat_all([H|T], ConcatedList):-
    concat_all(T, ConcatedTail),
    append(H, ConcatedTail, ConcatedList).

% lists_different(+List1, +List2, -Constraint)
%   Produces constraint constraining the difference of two lists of the same 
%   length. Uses reification to build disjunction of inequalities.
lists_different([], [], Cons) :-
    Cons #= 0.
lists_different([H1|T1], [H2|T2], Cons) :-
    lists_different(T1, T2, SubCons),
    Cons #<=> (H1 #\= H2) #\/ SubCons.

% list_different_from(+List, +ListOfLists)
%   Constraint specifying single list should be different from all given lists.
list_different_from([], _).
list_different_from([H|T], X):-
    lists_different(H, X, DiffConstraint),
    DiffConstraint #= 1, !,
    list_different_from(T, X).

% all_lists_different(+ListOfLists)
%   Implementation of all_different constraint for lists.
all_lists_different([]).
all_lists_different([H|T]) :-
    list_different_from(T, H), !,
    all_lists_different(T).

% crossword_model1(+M, +N, +WordsM, +WordsN, -Crossword)
%   Given dimension of a crossword and lists of words of size M an N
%   produces valid crosswords. Model is based on representing crossword
%   as a matrix of letters.
crossword_model1(M, N, WordsM, WordsN, Crossword):-
    % Generate matrix of variables representing letters in a crossword
    matrix(M, N, Crossword),
    % Flatten crossword
    concat_all(Crossword, FlatCrossword),
    % Set domain for each letter in a crossword
    domain(FlatCrossword, 97, 122),

    % Constrain horizontal words
    table(Crossword, WordsN),
    % By transposing table we get vertical words
    transpose(Crossword, TransposedCrossword),
    % Constrain vertical words
    table(TransposedCrossword, WordsM),

    % Makes sure each word used in the crossword is different
    % When dimensions are equal we have to make sure horizontal
    % words are different from vertical ones.
    (
        M = N ->
        append(Crossword, TransposedCrossword, AllWords),
        all_lists_different(AllWords)
    ;
        all_lists_different(Crossword),
        all_lists_different(TransposedCrossword)
    ),   

    % Look for solution in a flattened crossword.
    labeling([], FlatCrossword).

% I/O Code

write_line([]):-nl.
write_line([H|T]):-
    letter(H, L), 
    write(L),
    write_line(T).

write_crossword([]):-nl.
write_crossword([H|T]):-
    write_line(H),
    write_crossword(T).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,Term),
    write_to_codes(Term, X),
    read_file(Stream,L).

read_words(FileName, Words):-
    open(FileName, read, Stream),
    read_file(Stream, Words),
    close(Stream).

model1(M, N, File, Crossword):-
    write('Reading words...'),
    read_words(File, AllWords),
    write('Done!'),nl,
    write('Filtering words...'),
    include(length_rev(M), AllWords, WordsM),
    include(length_rev(N), AllWords, WordsN),
    write('Done!'),nl,!,
    crossword_model1(M, N, WordsM, WordsN, Crossword),
    write_crossword(Crossword).

profile_model1(M, N, File, MaxCount, Time):-
    bb_put(count, 0),
    write('Reading words...'),
    read_words(File, AllWords),
    write('Done!'),nl,
    write('Filtering words...'),
    include(length_rev(M), AllWords, WordsM),
    include(length_rev(N), AllWords, WordsN),
    write('Done!'),nl,!,

    statistics(runtime, _),
    crossword_model1(M, N, WordsM, WordsN, _),
    bb_get(count, Count),
    NewCount is Count + 1,
    bb_put(count, NewCount),
    ( 
        (Count < MaxCount, fail) 
    ;
        statistics(runtime, Time2),
        nth1(2, Time2, Time)
    ).
    


% Model2

% Precomputed powers of a number 27
pow27(0,1).
pow27(1,27).
pow27(2,729).
pow27(3,19683).
pow27(4,531441).
pow27(5,14348907).
pow27(6,387420489).

% word_to_num(+Word, +Exponent, -Num)
%   Converts words made from letters into a number. Understands letters
%   as coefficients of the number in 27th base. Converts number to decadic.
word_to_num([], _, 0).
word_to_num([H|T], Exp, Num):-
    NewExp is Exp + 1,
    word_to_num(T, NewExp, SubNum),
    pow27(Exp, Pow),
    Num is (H-96) * Pow + SubNum.

% words_to_nums(+Words, -Nums)
%   Converts all words of a given length to decadic numbers. 
words_to_nums([], []).
words_to_nums([Word|Words], [Num|Nums]):-
    word_to_num(Word, 0, Num),
    words_to_nums(Words, Nums).

% all_in_fdset(+Vars, +Set)
%   Assigns given FDSet to all variables.
all_in_fdset([], _).
all_in_fdset([H|T], Set):-
    H in_set Set, all_in_fdset(T, Set).

%same_letter(+Row, +Col, +NumRow, +NumCol)
%   Constraint constraining that NumRow-th letter in Col is the same
%   as the NumCol-th letter in Row. Compares cofficients of the Row and Col
%   in the 27-th base.
same_letter(Row, Col, NumRow, NumCol):-
    ModRowExp is NumCol,
    DivRowExp is NumCol-1,
    pow27(ModRowExp, ModRowPow),
    pow27(DivRowExp, DivRowPow),

    ModColExp is NumRow,
    DivColExp is NumRow-1,
    pow27(ModColExp, ModColPow),
    pow27(DivColExp, DivColPow),

    ((Row mod ModRowPow) / DivRowPow) #= ((Col mod ModColPow) / DivColPow).

match_word_with(_, [], _, _).
match_word_with(Row, [Col|Cols], NumRow, NumCol):-
    same_letter(Row, Col, NumRow, NumCol),
    NewNumCol is NumCol + 1, !,
    match_word_with(Row, Cols, NumRow, NewNumCol).

% match_all_words(+Rows, +Cols, +NumRow, +NumCol)
%   Makes sure that all word pairs (Row, Col) have same matching letter.
match_all_words([], _, _, _).
match_all_words([Row|Rows], Cols, NumRow, NumCol):-
    match_word_with(Row, Cols, NumRow, NumCol),
    NewNumRow is NumRow + 1, !,    
    match_all_words(Rows, Cols, NewNumRow, NumCol).


%crossword_model2(+M, +N, +SetM, +SetN, -Rows)
%   Given the dimension of a crossword and FDSets with words in decadic format
%   returns words selected for Rows.
crossword_model2(M, N, SetM, SetN, Rows):-
    % Initialized arrays where Rows and Columns are stored.
    length(Rows, M),
    length(Cols, N),
    append(Rows, Cols, AllWords),
    
    % Specify domains for each word
    all_in_fdset(Rows, SetN),
    all_in_fdset(Cols, SetM),

    % All words have to be different
    all_different(AllWords),

    % Constraint each two words
    match_all_words(Rows, Cols, 1, 1),

    % Look for solution on flattened crossword    
    labeling([], AllWords).

% num_to_word(+Num, -Word, +N)
%   Converts decadic number into word by converting into a number in a 27 base.
num_to_word(0, [], 0).
num_to_word(Num, Word, N):-
    NewN is N - 1,
    pow27(NewN, PowNewN),
    Letter is 96 + Num // PowNewN,
    NewNum is Num mod PowNewN,
    num_to_word(NewNum, SubWord, NewN),
    append(SubWord, [Letter], Word).

% write_nums(+Nums, N)
%   Given decadic numbers representing words coverts each number into a word
%   and prints it on the screen.
write_nums([], _).
write_nums([Num|Nums], N):-
    num_to_word(Num, Word, N),
    write_line(Word),
    write_nums(Nums, N).    

model2(M, N, File, Rows):-
    write('Reading words...'),
    read_words(File, AllWords),
    write('Done!'),nl,
    write('Filtering words...'),
    include(length_rev(M), AllWords, WordsM),
    include(length_rev(N), AllWords, WordsN),
    write('Done!'),nl,
    write('Converting words to numbers...'),
    words_to_nums(WordsM, NumbersM),
    words_to_nums(WordsN, NumbersN),
    write('Done!'),nl,
    write('Generating fdsets...'),
    list_to_fdset(NumbersM, SetM),
    list_to_fdset(NumbersN, SetN),
    write('Done!'),nl,!,
    crossword_model2(M, N, SetM, SetN, Rows),
    write_nums(Rows, N),nl.

profile_model2(M, N, File, MaxCount, Time):-
    bb_put(count, 0),

    write('Reading words...'),
    read_words(File, AllWords),
    write('Done!'),nl,
    write('Filtering words...'),
    include(length_rev(M), AllWords, WordsM),
    include(length_rev(N), AllWords, WordsN),
    write('Done!'),nl,
    write('Converting words to numbers...'),
    words_to_nums(WordsM, NumbersM),
    words_to_nums(WordsN, NumbersN),
    write('Done!'),nl,
    write('Generating fdsets...'),
    list_to_fdset(NumbersM, SetM),
    list_to_fdset(NumbersN, SetN),
    write('Done!'),nl,!,

    statistics(runtime, _),
    crossword_model2(M, N, SetM, SetN, _),
    bb_get(count, Count),
    NewCount is Count + 1,
    bb_put(count, NewCount),
    ( 
        (Count < MaxCount, fail) 
    ;
        statistics(runtime, Time2),
        nth1(2, Time2, Time)
    ).