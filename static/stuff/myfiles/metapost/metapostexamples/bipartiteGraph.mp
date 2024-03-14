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
	u := 4.0cm;
	pair A,B,C,D,E;

	A := (0, 0);
	B := (1u, 0);
	C := (2u, 0);
	D := (0, -1u);
	E := (1u, -1u);

	path AB, BC, AD, BE, BEoffset, DE;

	AB := A -- B;
	BC := B -- C;
	AD := A -- D;
	BE := B -- E;
	BEoffset := B .. (1/2[B,E] + (0.3u, 0)) .. E;
	DE := D -- E;

	draw AB;
	draw BC;
	draw AD;
	draw BE;
	draw BEoffset;
	draw DE;

	blackCircle(A, 4bp);
	whiteCircle(B, 4bp);
	blackCircle(C, 4bp);
	whiteCircle(D, 4bp);
	blackCircle(E, 4bp);

	%label.top(btex $1$ etex, 1/2[A,B]);
	%label.bot(btex $2$ etex, 1/2[B,C]);
	%label.rt(btex $3$ etex, 1/2[A,D]);
	%label.lft(btex $4$ etex, 1/2[B,E]);
	%label.lft(btex $5$ etex, point 0.5*length(BEoffset) of BEoffset);
	%label.bot(btex $6$ etex, 1/2[D,E]);
	label.top(btex $(b_1,w_2)_1$ etex, 1/2[A,B]);
	label.bot(btex $(b_3,w_2)_2$ etex, 1/2[B,C]);
	label.rt(btex $(b_1,w_1)_3$ etex, 1/2[A,D]);
	label.lft(btex $(b_2,w_2)_4$ etex, 1/2[B,E]);
	%label.lft(btex $(b_3,w_1)_5$ etex, point 0.5*length(BEoffset) of BEoffset);
	label.lft(btex $(b_2,w_2)_5$ etex, (0,0)) rotated 90 shifted (1/2[B,E] + (0.2u, 0.20u));
	label.bot(btex $(b_2,w_1)_6$ etex, 1/2[D,E]);

	label.top(btex $b_1$ etex, A);
	label.top(btex $b_3$ etex, C);
	label.bot(btex $b_2$ etex, E);

	label.top(btex $w_2$ etex, B);
	label.bot(btex $w_1$ etex, D);

endfig;

beginfig(2)
	u := 4.0cm;
	pair A,B,C,D,E;

	A := (0, 0);
	B := (1u, 0);
	C := (2u, 0);
	D := (0, -1u);
	E := (1u, -1u);

	path AB, BC, AD, BE, BEoffset, DE;

	AB := A -- B;
	BC := B -- C;
	AD := A -- D;
	BE := B -- E;
	BEoffset := B .. (1/2[B,E] + (0.3u, 0)) .. E;
	DE := D -- E;

	draw AB;
	draw AD;
	draw BE;
	%draw BEoffset;
	draw DE;

	blackCircle(A, 4bp);
	whiteCircle(B, 4bp);
	whiteCircle(D, 4bp);
	blackCircle(E, 4bp);

	label.bot(btex $x_2$ etex, 0.8[A,B]);

	label.rt(btex $x_6$ etex, 0.2[A,D]);

	%label.rt(btex $x_{10}$ etex, 0.2[B,E]);
	label.rt(btex $x_{10}$ etex, 0.2[B,E] shifted (0.2u, 0));
	label.lft(btex $x_9$ etex, 0.8[B,E]);

	%label.lft(btex $x_{11}$ etex, point 0.8*length(BEoffset) of BEoffset);
	label.lft(btex $x_{11}$ etex, point 0.8*length(BEoffset) of BEoffset shifted (0.2u,0));

	label.top(btex $x_7$ etex, 0.2[D,E]);

	draw BE shifted (0.2u,0);
	draw BEoffset shifted (0.2u,0);

	whiteCircle(B shifted (0.2u,0), 4bp);
	blackCircle(E shifted (0.2u,0), 4bp);
endfig;

beginfig(3)
	u := 4.0cm;
	pair A,B,C,D,E;

	A := (0, 0);
	B := (1u, 0);
	C := (2u, 0);
	D := (0, -1u);
	E := (1u, -1u);

	path AB, BC, AD, BE, BEoffset, DE;

	AB := A -- B;
	BC := B -- C;
	AD := A -- D;
	BE := B -- E;
	BEoffset := B .. (1/2[B,E] + (0.3u, 0)) .. E;
	DE := D -- E;

	draw AB;
	draw BC;
	draw AD;
	draw BE;
	draw BEoffset;
	draw DE;

	blackCircle(A, 4bp);
	whiteCircle(B, 4bp);
	blackCircle(C, 4bp);
	whiteCircle(D, 4bp);
	blackCircle(E, 4bp);

	label.top(btex $x_1$ etex, 0.2[A,B]);
	label.bot(btex $x_2$ etex, 0.8[A,B]);

	label.top(btex $x_3$ etex, 0.2[B,C]);
	label.bot(btex $x_4$ etex, 0.8[B,C]);

	label.lft(btex $x_5$ etex, 0.8[A,D]);
	label.rt(btex $x_6$ etex, 0.2[A,D]);

	label.rt(btex $x_{10}$ etex, 0.2[B,E]);
	label.lft(btex $x_9$ etex, 0.8[B,E]);

	label.rt(btex $x_{12}$ etex, point 0.2*length(BEoffset) of BEoffset);
	label.lft(btex $x_{11}$ etex, point 0.8*length(BEoffset) of BEoffset);

	label.top(btex $x_7$ etex, 0.2[D,E]);
	label.bot(btex $x_8$ etex, 0.8[D,E]);

	label.top(btex $1$ etex, 1/2[A,B]);
	label.bot(btex $2$ etex, 1/2[B,C]);
	label.rt(btex $3$ etex, 1/2[A,D]);
	label.lft(btex $4$ etex, 1/2[B,E]);
	label.lft(btex $5$ etex, point 0.5*length(BEoffset) of BEoffset);
	label.bot(btex $6$ etex, 1/2[D,E]);


	%label.top(btex $b_1$ etex, A);
	%label.top(btex $b_2$ etex, C);
	%label.bot(btex $b_3$ etex, E);
	%label.top(btex $w_1$ etex, B);
	%label.bot(btex $w_2$ etex, D);
endfig;

beginfig(4)
	u := 2.0cm;
	pair A,B,C,D,E;

	A := (0, 0);
	B := (1u, 0);
	C := (2u, 0);
	D := (0, -1u);
	E := (1u, -1u);

	path AB, BC, AD, BE, BEoffset, DE;

	AB := A -- B;
	BC := B -- C;
	AD := A -- D;
	BE := B -- E;
	BEoffset := B .. (1/2[B,E] + (0.3u, 0)) .. E;
	DE := D -- E;

	draw AB;
	draw BC;
	draw AD;
	draw BE;
	draw BEoffset;
	draw DE;

	blackCircle(A, 4bp);
	whiteCircle(B, 4bp);
	blackCircle(C, 4bp);
	whiteCircle(D, 4bp);
	blackCircle(E, 4bp);

	label.top(btex $1$ etex, 1/2[A,B]);
	label.bot(btex $2$ etex, 1/2[B,C]);
	label.rt(btex $3$ etex, 1/2[A,D]);
	label.lft(btex $4$ etex, 1/2[B,E]);
	label.lft(btex $5$ etex, point 0.5*length(BEoffset) of BEoffset);
	label.bot(btex $6$ etex, 1/2[D,E]);
endfig;

beginfig(5)
	u := 2.0cm;
	pair A,B,C,D,E;

	A := (0, 0);
	B := (1u, 0);
	C := (2u, 0);
	D := (0, -1u);
	E := (1u, -1u);

	path AB, BC, AD, BE, BEoffset, DE;

	AB := A -- B;
	BC := B -- C;
	AD := A -- D;
	BE := B -- E;
	BEoffset := B .. (1/2[B,E] + (0.3u, 0)) .. E;
	DE := D -- E;

	draw AB;
	draw BC;
	draw AD;
	draw BE;
	draw BEoffset;
	draw DE;

	blackCircle(A, 4bp);
	whiteCircle(B, 4bp);
	blackCircle(C, 4bp);
	whiteCircle(D, 4bp);
	blackCircle(E, 4bp);

	label.top(btex $1$ etex, 1/2[A,B]);
	label.bot(btex $2$ etex, 1/2[B,C]);
	label.rt(btex $3$ etex, 1/2[A,D]);
	label.lft(btex $4$ etex, 1/2[B,E]);
	label.lft(btex $5$ etex, point 0.5*length(BEoffset) of BEoffset);
	label.bot(btex $6$ etex, 1/2[D,E]);

	%label.top(btex $b_1$ etex, A);
	%label.top(btex $b_2$ etex, C);
	%label.bot(btex $b_3$ etex, E);
	%label.top(btex $w_1$ etex, B);
	%label.bot(btex $w_2$ etex, D);

endfig;

end

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




