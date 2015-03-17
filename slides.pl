%Slide-engnie based on Michael Hendricks Strangeloop 2014 talk
%https://github.com/mndrix/StrangeLoop2014/blob/master/slides/sessions/ProductionProlog-MichaelHendricks.pl

:- use_module(library(sweet), except([in/2])).
:- op(200, fy, (#)).

:- use ansi_term.
:- use clpfd.
:- use list_util.
:- use tty.
:- use www_browser.

:- discontiguous slide/1.

go :-
  once(clause(slide(Name),_)),
  go(Name).

go(Start) :-
  nb_setval(previous_slide,no_previous_slide),
  bagof(Name, Body^clause(slide(Name),Body), Names),
  drop_while(\=(Start), Names, Slides),
  member(Slide,Slides),
  nb_setval(previous_slide, Slide),
  slide(Slide).

resume :-
  nb_getval(previous_slide,Slide),
  Slide \= none,
  go(Slide).

slide(cover) :-
  # "Constraint Programming".

slide(constraints) :-
  # "Solving problems by satisfying constraints".

slide(domains) :-
  # "Problems are posed as constraints over domains".

slide(demo) :-
  # "Examples".

demo1(X) :-
  writeln("X #> 3."),
  X #> 3.

demo2(X) :-
  writeln("X*X #= 144."),
  X*X #= 144.

demo3(X, Y) :-
  writeln("4*X + 2*Y #= 24, X + Y #= 9, [X,Y] ins 0..sup."),
  4*X + 2*Y #= 24, X + Y #= 9, [X,Y] ins 0..sup.

% intro to constraint programming
slide(cp1) :-
  # "Constraint *programming*?".

slide(cp2) :-
  # "Constraints are relations between values and domains".

slide(cp3) :-
  # "Relations as first class values".

% segue to constraint logic programming
slide(clp1) :-
  # "Where else do we have constraints?".

slide(clp2) :-
  # "sort([3,1,2], A).".

slide(prolog) :-
  # "Prolog!".

slide(clp3) :-
  # "Constraint logic programming".

slide(clpfd) :-
  # "use_module(library(clpfd)).".

slide(sudoku0) :-
  # "Sudoku is a constraint satisfaction problem".

slide(sudoku1) :-
  # "Every row, column and block must be unique in 1..9".

slide(sudoku2) :-
  # "Let's solve it!".

printsrc :- writeln("sudoku(Rows) :-"),
  writeln("  append(Rows, Vs), Vs ins 1..9,"),
  writeln("  maplist(all_distinct, Rows),"),
  writeln("  transpose(Rows, Columns),"),
  writeln("  maplist(all_distinct, Columns),"),
  writeln("  Rows = [A,B,C,D,E,F,G,H,I],"),
  writeln("  blocks(A, B, C), blocks(D, E, F), blocks(G, H, I),"),
  writeln("  maplist(label, Rows)."),
  writeln(""),
  writeln("blocks([], [], [])."),
  writeln("blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-"),
  writeln("  all_distinct([A,B,C,D,E,F,G,H,I]),"),
  writeln("  blocks(Bs1, Bs2, Bs3).").

printpuzzle :- writeln("["),
  writeln(" [8,_,_,_,_,_,_,_,_],"),
  writeln(" [_,_,3,6,_,_,_,_,_],"),
  writeln(" [_,7,_,_,9,_,2,_,_],"),
  writeln(" [_,5,_,_,_,7,_,_,_],"),
  writeln(" [_,_,_,_,4,5,7,_,_],"),
  writeln(" [_,_,_,1,_,_,_,3,_],"),
  writeln(" [_,_,1,_,_,_,_,6,8],"),
  writeln(" [_,_,8,5,_,_,_,1,_],"),
  writeln(" [_,9,_,_,_,_,4,_,_]"),
  writeln("]").

sudoku(Rows) :-
  append(Rows, Vs), Vs ins 1..9,
  maplist(all_distinct, Rows),
  transpose(Rows, Columns),
  maplist(all_distinct, Columns),
  Rows = [A,B,C,D,E,F,G,H,I],
  blocks(A, B, C), blocks(D, E, F), blocks(G, H, I),
  maplist(label, Rows).

blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
  all_distinct([A,B,C,D,E,F,G,H,I]),
  blocks(Bs1, Bs2, Bs3).

puzzle([A,B,C,D,E,F,G,H,I]) :-
  [A,B,C,D,E,F,G,H,I] = [
   [8,_,_,_,_,_,_,_,_],
   [_,_,3,6,_,_,_,_,_],
   [_,7,_,_,9,_,2,_,_],
   [_,5,_,_,_,7,_,_,_],
   [_,_,_,_,4,5,7,_,_],
   [_,_,_,1,_,_,_,3,_],
   [_,_,1,_,_,_,_,6,8],
   [_,_,8,5,_,_,_,1,_],
   [_,9,_,_,_,_,4,_,_]
  ].

solve([A,B,C,D,E,F,G,H,I]) :-
  puzzle([A,B,C,D,E,F,G,H,I]),
  sudoku([A,B,C,D,E,F,G,H,I]).

slide(why) :-
  # "Why?".

slide(fun) :-
  # "Fun!".

slide(educational) :-
  # "New way of thinking".

slide(horizons) :-
  # "Expanding horizons".

slide(fin) :-
  # "Fin.".

slide(end) :-
  tty_clear,
  tty_size(Rows,_),
  TopSpace is floor(Rows * 0.4),
  forall(between(1,TopSpace,_),nl),
  format("?- the_end.~n").

%    # "swipl -o foo -c foo.pl".

#(Message) :-
  tty_clear,
  tty_size(Rows, Cols),

  % white space at the top
  TopSpace is floor((Rows-2)/2),
  n(TopSpace, nl),

  % the message itself
  string_length(Message, N),
  SpaceCount is floor((Cols - N) / 2),
  n(SpaceCount, write(" ")),
  ansi_format([], Message, []),
  nl,

  % white space at the bottom (pushing "true" downwards)
  BottomSpace is Rows - TopSpace - 2,
  n(BottomSpace,nl).

url(Url) :-
  # "",
  www_open_url(Url).

:- meta_predicate n(+,0).
n(N, Goal) :-
  forall(between(1,N,_),Goal).
