getOnTop(L,[A1|[A2|[A3|[A4]]]]):-
	getItem(0,L,T1),getItem(1,L,T2),getItem(2,L,T3),getItem(3,L,T4),
	getLast(T1,A1),getLast(T2,A2),getLast(T3,A3),getLast(T4,A4).
showRows(_,5,_):-
	writeln('').
showRows([H|T],I,J):-
	 (H=[]->Tmp is I+J,write(Tmp),write('		  '),I1 is I+1,showRows(T,I1,J);
	 H\=[]->write(H),write('		  '),I1 is I+1,showRows(T,I1,J)).
showBoard(B):-
	getItem(0,B,T1),showRows(T1,1,0),
	getItem(1,B,T2),showRows(T2,1,4),
	getItem(2,B,T3),showRows(T3,1,8),
	getItem(3,B,T4),showRows(T4,1,12).
start:- retractall(mainBoard(_)),retractall(count0(_)),retractall(count1(_)),
	findall([[],[],[],[]],between(1,4,_),Y),
        assert(mainBoard(Y)),
	assert(count0(20)),
	assert(count1(20)),
	game(0).
chooseRandom:-
	mainBoard(B),
	random(1,4,M),random(1,4,N),N1 is N-1,M1 is M-1,checkEight(N1/M1,B),put(N1/M1,1);chooseRandom.

checkEight(X/Y,B):-
	getItem(X,B,M1),getItem(Y,M1,N1),len(N1,L),L<8.
bigOnTop(B,[A1|[A2|[A3|[A4]]]]):-
	getItem(0,B,T1),getItem(1,B,T2),getItem(2,B,T3),getItem(3,B,T4),
	getOnTop(T1,A1),getOnTop(T2,A2),getOnTop(T3,A3),getOnTop(T4,A4).
input(M/N, B, Flag):-
read(Inp), len(Inp,Lu),
((Lu=1->count0(C0),C0>0,Flag is 0, getItem(0,Inp,U);Lu=3->getItem(0,Inp,U),getItem(1,Inp,W),getItem(2,Inp,Slice),R1 is W-1,M2 is div(R1,4),N2 is R1 mod 4,Flag is 1),X is U-1,
M1 is div(X,4),N1 is X mod 4,checkEight(M1/N1,B),(Flag=0->M is M1, N is N1;Flag=1->remove(M1/N1,M2/N2,Slice)));
(writeln('Index Full!'),input(M/N,B,Flag)).
game(Turn):-
	mainBoard(B),
	(Turn=0->write('Turn:Player    '),write('remaining: '),count0(C0),writeln(C0),showBoard(B),input(M/N,B,Flag),play(0,M/N,G,Flag);
	Turn=1->play(1,G),write('Turn:Computer    '),write('remaining: '),count1(C1),writeln(C1)),
	(Turn=0->T is 1;Turn=1-> T is 0),
	(G=1->write(Turn),write(' to won!');G=0->game(T)).
toIndexes([],[],[],_,_).
toIndexes([H|T],Board0,Board1,I,J):-
	I1 is I+1,
	(H=0->toIndexes(T,Tmp1,Board1,I1,J),addFirst(J/I1,Tmp1,Board0);
	 H=1->toIndexes(T,Board0,Tmp1,I1,J),addFirst(J/I1,Tmp1,Board1);
	H=[]->toIndexes(T,Board0,Board1,I1,J)).
decCount1:-
	retract(count1(X)),
	(X>0->Y is X-1,assert(count1(Y));
	X=0->assert(count1(X))
	).
decCount0:-
	retract(count0(X)),
	(X>0->Y is X-1,assert(count0(Y));
	X=0->assert(count0(X))
	).

play(0,X/Y,G,Flag):-
	  (   Flag=0->decCount0,put(X/Y,0);Flag=1->JK is 1),
          mainBoard(B),bigOnTop(B,A),
	  getItem(0,A,T1),toIndexes(T1,B10,B11,0,1),
	  getItem(1,A,T2),toIndexes(T2,B20,B21,0,2),
	  getItem(2,A,T3),toIndexes(T3,B30,B31,0,3),
	  getItem(3,A,T4),toIndexes(T4,B40,B41,0,4),
	  append(B10,B20,Tmp1),append(Tmp1,B30,Tmp2),append(Tmp2,B40,B0),
	  append(B11,B21,Tmp3),append(Tmp3,B31,Tmp4),append(Tmp4,B41,_),
	  ((solved(B0,_),G is 1);G is 0).
sliceZeros([],1).
sliceZeros([H|_],I):-
	H=1,I is 1.
sliceZeros([H|T],I):-
	H=0,sliceZeros(T,I1),I is I1+1.

play(1,G):-
	mainBoard(B),bigOnTop(B,A),
	  getItem(0,A,T1),toIndexes(T1,B10,B11,0,1),
	  getItem(1,A,T2),toIndexes(T2,B20,B21,0,2),
	  getItem(2,A,T3),toIndexes(T3,B30,B31,0,3),
	  getItem(3,A,T4),toIndexes(T4,B40,B41,0,4),
	  append(B10,B20,Tmp1),append(Tmp1,B30,Tmp2),append(Tmp2,B40,B0),
	  append(B11,B21,Tmp3),append(Tmp3,B31,Tmp4),append(Tmp4,B41,B1),
	((preventLose(B0,B1),G is 0);(tryWin(B1),G is 1);len(B0,P),P1 is P-1,heuristic(B0,P1,Score,I/J),Score>0,I1 is I-1,J1 is J-1,checkEight(I1/J1,B),put(I1/J1,1),decCount1,G is 0;(chooseRandom,G is 0,decCount1)).
transfer(L,X/Y,M/N):-
	mainBoard(B),
	X1 is X-1,X2 is X+1,Y1 is Y-1,Y2 is Y+1,
	(member(X/Y1,L),N1 is X-1,M1 is Y1-1,checkEight(N1/M1,B),M is X,N is Y1;
	 member(X/Y2,L),N1 is X-1,M1 is Y2-1,checkEight(N1/M1,B),M is X,N is Y2;
	 member(X1/Y,L),N1 is X1-1,M1 is Y-1,checkEight(N1/M1,B),M is X1,N is Y;
         member(X2/Y,L),N1 is X2-1,M1 is Y-1,checkEight(N1/M1,B),M is X2,N is Y).

brute(I,O,N/M):-
	I>(-1),mainBoard(B),
	(getItem(I,O,N/M),N1 is N-1,M1 is M-1,checkEight(N1/M1,B);I1 is I-1,brute(I1,O,N/M)).
preventLose(B0,B1):-
	do(B0,I/J,O),(member(I/J,B1),delFirst(O,O1),transfer(O1,I/J,N/M),N2 is N1-1,M2 is M1-1,N3 is N-1,M3 is M-1,remove2(N2/M2,N3/M3);
		     neighbour(B1,O,N1/M1),transfer(O,N1/M1,N/M),N2 is N1-1,M2 is M1-1,N3 is N-1,M3 is M-1,remove2(N2/M2,N3/M3);
		     len(O,P),P1 is P-1,brute(P1,O,N/M),N1 is N-1,M1 is M-1,put(N1/M1,1)).
tryWin(B1):-
	do(B1,N1/M1,O),(neighbour(B1,O,N1/M1),transfer(O,N1/M1,N/M),N2 is N1-1,M2 is M1-1,N3 is N-1,M3 is M-1,remove2(N2/M2,N3/M3);put(N2/M2,1)).
do(L,N/M,O):-
	solution(L,N/M,O).


give([],[]).
give([H1|T1],Ans):-
	between(0,1,H2),
	(H2=1->give(T1,Ans2),addFirst(H1,Ans2,Ans);
	H2=0->give(T1,Ans)).
len([],0).
len([_|T],N):-
	len(T,N1),
	N is N1+1.
member(X,[X|_]).
member(X,[H|T]):-
	X\=H,
	member(X,T).
notMember(_,[]).
notMember(X,[H|T]):-
            X\=H,
	    notMember(X,T).

neighbour([X/Y|Others],L,I/J):-
	(   ( check(X/Y,L,M),M>0,notMember(X/Y,L)),I is X,J is Y;neighbour(Others,L,I/J)).
solution(L,N/M,O):-
	between(1,4,N),
	between(1,4,M),
	addFirst(N/M,L,U),
	solved(U,O).
solved(U,O):-
	give(U,O),
	len(O,P),
	P1 is P-1,
	checkMain(O,P1),
	(doneX(O);doneY(O)).
remove(X/Y,M/N,S):-
retract(mainBoard(B)),
getItem(X,B,Tmp1),getItem(Y,Tmp1,Tmp2),len(Tmp2,P),P1 is P-S, P2 is P-1,popItems(P1, P2, Tmp2, Tmp3, Poped), changeItem(Y, Tmp3, Tmp1,C1),changeItem( X,C1, B, B2),
getItem(M, B2, T1),getItem(N,T1, T2), append(T2, Poped, T3), changeItem(N,T3, T1, R1), changeItem(M, R1, B2,C), assert(mainBoard(C)).
remove2(X/Y,M/N):-
retract(mainBoard(B)),
getItem(X,B,Tmp1),getItem(Y,Tmp1,Tmp2),delLast(Tmp2,Amir1),reverse(Amir1,Amir),sliceZeros(Amir,S),len(Tmp2,P),P1 is P-S, P2 is P-1,popItems(P1, P2, Tmp2, Tmp3, Poped), changeItem(Y, Tmp3, Tmp1,C1),changeItem( X,C1, B, B2),
getItem(M, B2, T1),getItem(N,T1, T2), append(T2, Poped, T3), changeItem(N,T3, T1, R1), changeItem(M, R1, B2,C), assert(mainBoard(C)).

put(X/Y,I):-
	retract(mainBoard(B)),
	getItem(X,B,Tmp1),getItem(Y,Tmp1,Tmp2),addLast(I,Tmp2,Tmp3),changeItem(Y,Tmp3,Tmp1,C1),changeItem(X,C1,B,C2),assert(mainBoard(C2)).
minX([X/Y],X/Y).
minX([X/Y|T],M/N):-
	minX(T,M1/M2),
	(X<M1->M is X,N is Y;
	 X>=M1->M is M1,N is M2).
minY([X/Y],X/Y).
minY([X/Y|T],M/N):-
	minY(T,M1/M2),
	(Y<M2->M is X,N is Y;
	 Y>=M2->M is M1,N is M2).
maxX([X/Y],X/Y).
maxX([X/Y|T],M/N):-
	maxX(T,M1/M2),
	(X>M1->M is X,N is Y;
	 X=<M1->M is M1,N is M2).
maxY([X/Y],X/Y).
maxY([X/Y|T],M/N):-
	maxY(T,M1/M2),
	(Y>M2->M is X,N is Y;
	 Y=<M2->M is M1,N is M2).

doneX(L):-
	minX(L,X1/_),
	maxX(L,X2/_),
	X1=1,
	X2=4.
doneY(L):-

	minY(L,_/Y1),
	maxY(L,_/Y2),
	Y1=1,
	Y2=4.
addFirst(X,[],[X]).
addFirst(X,[H|T],[X|[H|T]]).
addLast(X,[],[X]).
addLast(X,[H|T],[H|Y]):-addLast(X,T,Y).
getLast([H],H).
getLast([],[]).
getLast([_|T],Y):-getLast(T,Y).
delLast([_],[]).
delLast([],[]).
delLast([H|T],[H|L]):-
	delLast(T,L).
delFirst([],[]).
delFirst([_|T],T).
checkMain(_,-1).
checkMain(L,K):-

	popItem(K,L,L1,It0/It1),
	It0>1,
	It1>1,
	It1<4,
	It0<4,
	check(It0/It1,L1,M),
	M>1,
	S is K-1,
	checkMain(L,S).
checkMain(L,K):-

	popItem(K,L,L1,It0/It1),
	(   It0=1;
	It1=1;
	It1=4;
	It0=4),
	check(It0/It1,L1,M),

	M>0,
	S is K-1,
	checkMain(L,S).
getItem(0,[H|_],H).
getItem(S,[_|T],Y):-
	S>0,
	N is S-1,
	getItem(N,T,Y).

changeItem(0,U,[_|T],[U|T]):-!.
changeItem(I,U,[H|T],[H|Y]):-
	I>0,
	N is I-1,
	changeItem(N,U,T,Y).
popItems(0,0,[H|T], T,[H]).
popItems(0, E, [H|T],Y,O):-
	E1 is E-1, popItems(0,E1, T, Y,O1),
	addFirst(H,O1,O).
popItems(I, E, [H|T], [H|Y],O):-
I>0, N is I-1, E1 is E-1, popItems(N, E1,T,Y,O).
popItem(0,[H|T],T,H):-!.
popItem(I,[H|T],[H|Y],O):-
	I>0,
	N is I-1,
	popItem(N,T,Y,O).
check(_/_,[],0).
check(X/Y,[X2/Y2|Others],M):-
	X>X2,
	Tmp is X-X2,
	Y=Y2,
	check(X/Y,Others,N),
	( Tmp=1->
	M is N+1;
	Tmp \=1->
	M is N).

check(X/Y,[X2/Y2|Others],M):-
	X2>X,
	Tmp is X2-X,
	Y=Y2,
	check(X/Y,Others,N),
	( Tmp=1->
	M is N+1;
	Tmp \=1->
	M is N).

check(X/Y,[X2/Y2|Others],M):-
	X=X2,
	Y2>Y,
	Tmp is Y2-Y,
	check(X/Y,Others,N),
	( Tmp=1->
	M is N+1;
	Tmp \=1->
	M is N).

check(X/Y,[X2/Y2|Others],M):-
	X=X2,
	Y>Y2,
	Tmp is  Y-Y2,
	check(X/Y,Others,N),
	( Tmp=1->
	M is N+1;
	Tmp \=1->
	M is N).

check(X/Y,[X2/Y2|Others],M):-
	X\=X2,
	Y\=Y2,
	check(X/Y,Others,M).

heuristic(_,-1,-10,_/_).
heuristic(L,K,Score,X/Y):-

	popItem(K,L,L1,It0/It1),
	It0>1,
	It1>1,
	It1<4,
	It0<4,
	check(It0/It1,L1,M),
	S is K-1,
	heuristic(L,S,Score1,I/J),
	(M>Score1->Score is M,X is It0,Y is It1;M=<Score1->Score is Score1,X is I,Y is J).
heuristic(L,K,Score,X/Y):-

	popItem(K,L,L1,It0/It1),
	(   It0=1;
	It1=1;
	It1=4;
	It0=4),
	check(It0/It1,L1,M),
	S is K-1,
	heuristic(L,S,Score1,I/J),
	(M>Score1->Score is M,X is It0,Y is It1;M=<Score1->Score is Score1,X is I,Y is J).























