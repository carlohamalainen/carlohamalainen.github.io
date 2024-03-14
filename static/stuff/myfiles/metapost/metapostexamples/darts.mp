%&latex 
verbatimtex
\documentclass[12pt]{article}

% \usepackage[mathlf,minionint]{MinionPro} \usepackage[T1]{fontenc} \usepackage{textcomp}

\begin{document}
etex

def whiteCircle(expr p, b) =
begingroup
	fill fullcircle scaled b shifted p withcolor white;
	draw fullcircle scaled b shifted p;
endgroup
enddef;

def blackCircle(expr p, b) =
begingroup
	draw p withpen pencircle scaled b;
endgroup
enddef;

beginfig(1)
	u := 0.9cm;
	pair A,B,C,D,E,F;

	A := (0, 0);
	B := (2u, 0);
	C := (4u, 0);
	D := (0, -4u);
	
	blackCircle(A, 4bp);
	blackCircle(B, 4bp);
	blackCircle(C, 4bp);
	blackCircle(D, 4bp);

	path AB, BC, AD, BD, CC, DBoffset;

	AB := A -- B;
	BC := B -- C;
	AD := A -- D;
	BD := B ..  D;
	CC := C .. (C + (u, u)) .. (C + (u, -u)) .. C;
	DBoffset := D{right} .. B{up};
	BD := B .. ((point 0.5*length(DBoffset) of DBoffset + (1u, -1u))) .. D;

	draw AB;
	draw BC;
	draw AD;
	draw BD;
	draw CC;
	draw DBoffset;

	label.top(btex $e_1$ etex, point 0.5*length(AB) of AB);
	label.top(btex $e_2$ etex, point 0.5*length(BC) of BC);
	label.rt(btex $e_4$ etex, point 0.5*length(AD) of AD);
	label.lft(btex $e_6$ etex, point 0.5*length(BD) of BD);
	label.ulft(btex $e_3$ etex, point 0.5*length(CC) of CC);
	label.lrt(btex $e_5$ etex, point 0.5*length(DBoffset) of DBoffset);
	
	%label.ulft(btex $1$ etex, A) shifted (-0.05u, 0.1u);
	%label.llft(btex $2$ etex, A) shifted (-0.05u, -0.1u);
	%label.urt(btex $3$ etex, A) shifted (0, 0.35u);
	%label.lrt(btex $4$ etex, A) shifted (0.1u, 0);

	%label.bot(btex $5$ etex, B) shifted (0, -0.15u);
	%label.top(btex $6$ etex, C) shifted (-0.2u, -0.15u);

	label.top(btex $v_1$ etex, A);
	label.top(btex $v_2$ etex, B);
	label.ulft(btex $v_3$ etex, C);
	label.lft(btex $v_4$ etex, D);
endfig;


beginfig(2)
	blackCircle(A, 4bp);
	blackCircle(B, 4bp);
	blackCircle(C, 4bp);
	blackCircle(D, 4bp);

	path AB, BC, AD, BD, CC, DBoffset;

	AB := A -- B;
	BC := B -- C;
	AD := A -- D;
	BD := B ..  D;
	CC := C .. (C + (u, u)) .. (C + (u, -u)) .. C;
	DBoffset := D{right} .. B{up};
	BD := B .. ((point 0.5*length(DBoffset) of DBoffset + (1u, -1u))) .. D;

	draw AB;
	draw BC;
	draw AD;
	draw BD;
	draw CC;
	draw DBoffset;

	label.top(btex $1$ etex, point 0.2*length(AB) of AB);
	label.bot(btex $2$ etex, point 0.8*length(AB) of AB);

	label.top(btex $3$ etex, point 0.2*length(BC) of BC);
	label.bot(btex $4$ etex, point 0.8*length(BC) of BC);

	label.ulft(btex $5$ etex, point 0.2*length(CC) of CC);
	label.urt(btex $6$ etex, point 0.8*length(CC) of CC);

	label.rt(btex $7$ etex, point 0.2*length(AD) of AD);
	label.lft(btex $8$ etex, point 0.8*length(AD) of AD);

	label.rt(btex $9$ etex, point 0.2*length(BD) of BD);
	label.top(btex ${10}$ etex, point 0.8*length(BD) of BD);

	label.ulft(btex ${11}$ etex, point 0.2*length(DBoffset) of DBoffset);
	label.rt(btex ${12}$ etex, point 0.8*length(DBoffset) of DBoffset);

	%drawarrow A .. ((point 0.5*length(AB) of AB) + (0, 0.1u)) .. B;
	%drawarrow reverse(A .. ((point 0.5*length(AB) of AB) + (0, -0.1u)) .. B);

	%drawarrow B .. ((point 0.5*length(BC) of BC) + (0, 0.1u)) .. C;
	%drawarrow reverse(B .. ((point 0.5*length(BC) of BC) + (0, -0.1u)) .. C);

	%drawarrow A .. ((point 0.5*length(AD) of AD) + (-0.1u,0)) .. D;
	%drawarrow reverse(A .. ((point 0.5*length(AD) of AD) + (+0.1u,0)) .. D);

	%drawarrow B .. ((point 0.5*length(BD) of BD) + (-0.1u,0.1u)) .. D;
	%drawarrow reverse(B .. ((point 0.5*length(BD) of BD) + (0.1u,-0.1u)) .. D);
endfig;

beginfig(3)
	%blackCircle(A, 4bp);
	%blackCircle(B, 4bp);
	%blackCircle(C, 4bp);
	%blackCircle(D, 4bp);

	pair faceOffset;
	faceOffset := (0.5u, -0.5u);

	path AB, BC, AD, BD, CC, DBoffset;

	AB := A -- B;
	BC := B -- C;
	AD := A -- D;
	BD := B ..  D;
	CC := C .. (C + (u, u)) .. (C + (u, -u)) .. C;
	DBoffset := D{right} .. B{up};
	BD := B .. ((point 0.5*length(DBoffset) of DBoffset + (1u, -1u))) .. D;

	drawarrow reverse(AB);
	%draw BC;
	drawarrow AD;
	%draw BD;
	%draw CC;
	drawarrow DBoffset;

	drawarrow reverse(BD shifted faceOffset);
	drawarrow reverse(DBoffset shifted faceOffset);

	%label.top(btex $1$ etex, point 0.2*length(AB) of AB);
	label.bot(btex $2$ etex, point 0.8*length(AB) of AB);

	%label.top(btex $3$ etex, point 0.2*length(BC) of BC);
	%label.bot(btex $4$ etex, point 0.8*length(BC) of BC);

	%label.ulft(btex $5$ etex, point 0.2*length(CC) of CC);
	%label.urt(btex $6$ etex, point 0.8*length(CC) of CC);

	label.rt(btex $7$ etex, point 0.2*length(AD) of AD);
	%label.lft(btex $8$ etex, point 0.8*length(AD) of AD);

	label.top(btex ${10}$ etex, point 0.8*length(BD) of BD) shifted faceOffset;

	label.ulft(btex ${11}$ etex, point 0.2*length(DBoffset) of DBoffset);
	label.rt(btex ${12}$ etex, point 0.8*length(DBoffset) of DBoffset) shifted faceOffset;


endfig;

end







