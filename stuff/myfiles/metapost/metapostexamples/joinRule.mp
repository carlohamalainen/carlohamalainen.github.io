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



	% Makes arrow heads go half way.
	save arrowhead;
	vardef arrowhead expr p =
		save A,u; pair A,u;
		A := point 1/2length(p) of p;
		u := unitvector(direction 1/2length(p) of p);
		A -- (A - ahlength*u rotated 25) --
		(A - ahlength*u rotated -25) -- cycle
	enddef;

beginfig(1)
  u:=1cm;
  pair A,B,C,D;
  A:=(0,0);
  B:=u*dir 60;
  C:=u*dir -60;
  D:=(2u,0);
  draw A withpen pencircle scaled 4bp;
  draw B withpen pencircle scaled 4bp;
  draw C withpen pencircle scaled 4bp;
  draw D withpen pencircle scaled 4bp;
  draw A--D--B--D--C;
  % draw A{up}..B;
  % draw A{down}..C;
  % draw A{dir 30}..B;
  % draw A{dir -30}..C;
endfig;

beginfig(2)
	u := 2cm;

	pair X,A,B,C,D,E,F;

	X := (0,0);
	A := u * dir 0;
	B := u * dir 60;
	C := u * dir 120;
	D := u * dir 180;
	E := u * dir 240;
	F := u * dir 300;

	pickup pencircle scaled 4pt;
	draw X;
	draw A;
	draw B;
	draw C;
	draw D;
	draw E;
	draw F;

	pickup defaultpen;

	draw X--A;
	draw X--B;
	draw X--C;
	draw X--D;
	draw X--E;
	draw X--F;

	draw A--B;
	drawarrow A -- B;

	draw C--D;
	drawarrow C-- D;

	draw E--F;
	drawarrow E-- F;
	
	drawarrow origin -- A;
	drawarrow B -- origin;
	drawarrow origin -- C;
	drawarrow D -- origin;
	drawarrow origin -- E;
	drawarrow F -- origin;

	%label.urt(btex $t_1$ etex, 1/2[X, 1/2[A,B]]);
	%label.ulft(btex $t_2$ etex, 1/2[X, 1/2[C,D]]);
	%label.bot(btex $t_3$ etex, 1/2[X, 1/2[E,F]]);

	label.top(btex $\tau_1$ etex, 1/2[X,A]);
	label.urt(btex $\tau_2$ etex, 1/2[A,B]);
	label.ulft(btex $\tau_3$ etex, 1/2[B,X]);

	%label.rt(btex $x \tau_1$ etex, A);
	%label.top(btex $\tau^{-1}_2(x)$ etex, B);
	%label.top(btex $\tau_2(x)$ etex, C);
	%label.lft(btex $\tau^{-1}_3(x)$ etex, D);
	%label.bot(btex $\tau_3(x)$ etex, E);
	%label.bot(btex $\tau^{-1}_1(x)$ etex, F);
endfig;

beginfig(3)
	u := 2cm;

	pair X,A,B,C,D,E,F;

	X := (0,0);
	A := u * dir 0;
	B := u * dir 60;
	C := u * dir 120;
	D := u * dir 180;
	E := u * dir 240;
	F := u * dir 300;

	pickup pencircle scaled 4pt;
	draw X;
	draw A;
	draw B;
	draw C;
	draw D;
	draw E;
	draw F;

	pickup defaultpen;

	draw X--A;
	draw X--B;
	draw X--C;
	draw X--D;
	draw X--E;
	draw X--F;

	draw A--B;
	drawarrow A -- B;

	draw C--D;
	drawarrow C-- D;

	draw E--F;
	drawarrow E-- F;
	
	drawarrow origin -- A;
	drawarrow B -- origin;
	drawarrow origin -- C;
	drawarrow D -- origin;
	drawarrow origin -- E;
	drawarrow F -- origin;

	label.urt(btex $t_1$ etex, 1/2[X, 1/2[A,B]]);
	label.ulft(btex $t_2$ etex, 1/2[X, 1/2[C,D]]);
	label.bot(btex $t_3$ etex, 1/2[X, 1/2[E,F]]);

	label.rt(btex $\tau_1(x)$ etex, A);
	label.top(btex $\tau^{-1}_2(x)$ etex, B);
	label.top(btex $\tau_2(x)$ etex, C);
	label.lft(btex $\tau^{-1}_3(x)$ etex, D);
	label.bot(btex $\tau_3(x)$ etex, E);
	label.bot(btex $\tau^{-1}_1(x)$ etex, F);

	% New part is here.
	drawarrow A .. F dashed evenly;
	label.lrt(btex $\sigma_1$ etex, 1/2[X, 1/2[A,F]]);
	
	drawarrow E .. D dashed evenly;
	label.llft(btex $\sigma_3$ etex, 1/2[X, 1/2[E,D]]);
	
	drawarrow C .. B dashed evenly;
	label.top(btex $\sigma_2$ etex, 1/2[X, 1/2[C,B]]);
endfig;

beginfig(4)
	u := 2cm;

	pair A, B, C, D, E, F, G;

	A := u * dir 0;
	B := u * dir 90;
	C := u * dir (90 + 45);
	D := u * dir 180;
	E := u * dir (180 + 45);
	F := u * dir 270;
	G := u * dir (270 + 45);
	
	scale := 1.2;

	% The inner cycle.
	drawarrow A -- B;
	drawarrow B -- D;
	drawarrow D -- E;
	drawarrow E -- F;
	drawarrow F -- G;
	drawarrow G -- A;

	% The outer cycle.
	drawarrow A .. (scale * u * dir (270 + 45)) .. F;
	drawarrow F .. (scale * u * dir (180 + 45)) .. D;
	drawarrow D .. C;
	drawarrow C .. B;
	drawarrow B{right} .. A{down};
	
	pickup pencircle scaled 4pt;
	draw A;
	draw B;
	draw C;
	draw D;
	draw E;
	draw F;
	draw G;
	pickup defaultpen;

	label.rt(btex $v_4-$ etex, A);
	label.top(btex $v_1-$ etex, B);
	label.lft(btex $v_2+$ etex, D);
	label.bot(btex $v_3+$ etex, F);

endfig;

beginfig(5)
	u := 2cm;

	pair A, B, C, D, E, F, G, H;

	A := u * dir 0;
	B := u * dir 90;
	C := u * dir (90 + 45);
	D := u * dir 180;
	E := u * dir (180 + 45);
	F := u * dir 270;
	G := u * dir (270 + 45);
	H := u * dir 45;
	
	scale := 1.2;

	% The inner cycle.
	drawarrow A -- B;
	drawarrow B -- D;
	drawarrow D -- E;
	drawarrow E -- F;
	drawarrow F -- A;

	% The outer cycle.
	drawarrow A .. G;
	drawarrow G .. F;
	drawarrow F .. (scale * u * dir (180 + 45)) .. D;
	drawarrow D .. C;
	drawarrow C .. B;
	drawarrow B .. H;
	drawarrow H .. A;
	
	pickup pencircle scaled 4pt;
	draw A;
	draw B;
	draw C;
	draw D;
	draw E;
	draw F;
	draw G;
	draw H;
	pickup defaultpen;

	label.rt(btex $v_4-$ etex, A);
	label.top(btex $v_1-$ etex, B);
	label.lft(btex $v_2+$ etex, D);
	label.bot(btex $v_3+$ etex, F);

endfig;

beginfig(44)
	u := 2cm;

	pair A, B, C, D, E, F, G;

	A := u * dir 0;
	B := u * dir 90;
	C := u * dir (90 + 45);
	D := u * dir 180;
	E := u * dir (180 + 45);
	F := u * dir 270;
	G := u * dir (270 + 45);
	
	scale := 1.2;

	% The inner cycle.
	drawarrow A -- B;
	drawarrow B -- D;
	drawarrow D -- E;
	drawarrow E -- F;
	drawarrow F -- G;
	drawarrow G -- A;

	% The outer cycle.
	drawarrow A .. (scale * u * dir (270 + 45)) .. F;
	drawarrow F .. (scale * u * dir (180 + 45)) .. D;
	drawarrow D .. C;
	drawarrow C .. B;
	drawarrow B{right} .. A{down};
	
	pickup pencircle scaled 4pt;
	draw A;
	draw B;
	draw C;
	draw D;
	draw E;
	draw F;
	draw G;
	pickup defaultpen;

	label.rt(btex $v_4-$ etex, A);
	label.top(btex $v_1-$ etex, B);
	label.lft(btex $v_2+$ etex, D);
	label.bot(btex $v_3+$ etex, F);

endfig;

beginfig(55)
	u := 2cm;

	pair A, B, C, D, E, F, G, H;

	A := u * dir 0;
	B := u * dir 90;
	C := u * dir (90 + 45);
	D := u * dir 180;
	E := u * dir (180 + 45);
	F := u * dir 270;
	G := u * dir (270 + 45);
	H := u * dir 45;
	
	scale := 1.2;

	% The inner cycle.
	drawarrow A -- B;
	drawarrow B -- D;
	drawarrow D -- E;
	drawarrow E -- F;
	drawarrow F -- A;

	% The outer cycle.
	drawarrow A .. G;
	drawarrow G .. F;
	drawarrow F .. (scale * u * dir (180 + 45)) .. D;
	drawarrow D .. C;
	drawarrow C .. B;
	drawarrow B .. H;
	drawarrow H .. A;
	
	pickup pencircle scaled 4pt;
	draw A;
	draw B;
	draw C;
	draw D;
	draw E;
	draw F;
	draw G;
	draw H;
	pickup defaultpen;

	label.rt(btex $v_4-$ etex, A);
	label.top(btex $v_1-$ etex, B);
	label.lft(btex $v_2+$ etex, D);
	label.bot(btex $v_3+$ etex, F);

endfig;

beginfig(6)

	u := 2cm;
	theta := 80;
	gap := 0.4 u;
	scale := 0.5;
	width := 3 u;

	pair A, B, C, D;
	pair a, b, c, d;

	% Outer curve, left.
	path outerLeft;
	A := u * dir theta;
	B := u * dir -theta;
	outerLeft = A .. origin .. B;
	draw outerLeft;
	drawarrow reverse outerLeft;

	% Inner curve, left.
	path innerLeft;
	C := scale * u * dir theta shifted (gap, 0);
	D := scale * u * dir -theta shifted (gap, 0);
	innerLeft = C .. (origin shifted (gap, 0)) .. D;
	draw innerLeft;
	drawarrow innerLeft;

	% Outer curve, right.	
	path outerRight;
	a := u * dir (180 - theta) shifted (width, 0);
	b := u * dir (180 + theta) shifted (width, 0);
	outerRight = a .. (origin  shifted (width, 0)) .. b;
	draw outerRight;
	drawarrow reverse outerRight;

	% Inner curve, right.
	path innerRight;
	c := scale * u * dir (180 - theta) shifted (width - gap, 0);
	d := scale * u * dir (180 + theta) shifted (width - gap, 0);
	innerRight = c .. (origin shifted (width - gap, 0)) .. d;
	draw innerRight;
	drawarrow innerRight;
	
	% All vertices
	pickup pencircle scaled 4pt;
	draw A;
	draw B;
	draw C;
	draw D;
	draw a;
	draw b;
	draw c;
	draw d;
	pickup defaultpen;

	% label.lft(btex outer arc in trade $A$ etex, origin);
	% label.rt(btex outer arc in trade $B$ etex, (width, 0));

	label.bot(btex $a_1$ etex, B);
	label.top(btex $a_2$ etex, A);
	label.ulft(btex $a_3$ etex, C);
	label.lft(btex $a_4$ etex, D);
	
	label.rt(btex $b_1$ etex, b);
	label.rt(btex $b_2$ etex, a);
	label.top(btex $b_3$ etex, c);
	label.bot(btex $b_4$ etex, d);

	% Dashed arrows.
	drawarrow A{right} .. c{right} dashed evenly;
	drawarrow d{left} .. B{left} dashed evenly;

	drawarrow D{right} .. b{right} dashed evenly;
	drawarrow a{left} .. C{left} dashed evenly;
endfig;

beginfig(9)
	u := 2cm;

	pair X,A,B,C,D,E,F;

	X := (0,0);
	A := u * dir 0;
	B := u * dir 60;
	C := u * dir 120;
	D := u * dir 180;
	E := u * dir 240;
	F := u * dir 300;

	pickup pencircle scaled 4pt;
	draw X;
	draw A;
	draw B;
	draw C;
	draw D;
	draw E;
	draw F;

	pickup defaultpen;

	draw X--A;
	draw X--B;
	draw X--C;
	draw X--D;
	draw X--E;
	draw X--F;

	draw A--B;
	drawarrow A -- B;

	draw C--D;
	drawarrow C-- D;

	draw E--F;
	drawarrow E-- F;
	
	drawarrow origin -- A;
	drawarrow B -- origin;
	drawarrow origin -- C;
	drawarrow D -- origin;
	drawarrow origin -- E;
	drawarrow F -- origin;

	%label.urt(btex $t_1$ etex, 1/2[X, 1/2[A,B]]);
	%label.ulft(btex $t_2$ etex, 1/2[X, 1/2[C,D]]);
	%label.bot(btex $t_3$ etex, 1/2[X, 1/2[E,F]]);

	label.top(btex $\tau_1$ etex, 1/2[X,A]);
	label.urt(btex $\tau_2$ etex, 1/2[A,B]);
	label.ulft(btex $\tau_3$ etex, 1/2[B,X]);

	label.urt(btex $\tau_2$ etex, 1/2[X,C]);
	label.ulft(btex $\tau_3$ etex, 1/2[C,D]);
	label.top(btex $\tau_1$ etex, 1/2[D,X]);
	
	label.ulft(btex $\tau_3$ etex, 1/2[X,E]);
	label.bot(btex $\tau_1$ etex, 1/2[E,F]);
	label.urt(btex $\tau_2$ etex, 1/2[F,X]);

	%label.rt(btex $x \tau_1$ etex, A);
	%label.top(btex $\tau^{-1}_2(x)$ etex, B);
	%label.top(btex $\tau_2(x)$ etex, C);
	%label.lft(btex $\tau^{-1}_3(x)$ etex, D);
	%label.bot(btex $\tau_3(x)$ etex, E);
	%label.bot(btex $\tau^{-1}_1(x)$ etex, F);
endfig;

end







