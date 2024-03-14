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

vardef TEX primary s =
   write "verbatimtex"                    to "mptextmp.mp";
   write "\documentclass[12pt]{article}"  to "mptextmp.mp";
   %write "\usepackage[T1]{fontenc}"       to "mptextmp.mp";
   %write "\usepackage{amsmath,amssymb}"   to "mptextmp.mp";
   write "\begin{document}"               to "mptextmp.mp";
   write "etex"                           to "mptextmp.mp";
   write "btex "&s&" etex"                to "mptextmp.mp";
   write EOF                              to "mptextmp.mp";
   scantokens "input mptextmp"
enddef;

beginfig(3)
	u := 1.7cm;

	picture hexagon;
	hexagon := nullpicture;

	pair A[], B[];
	numeric n; n:=6;

	for i=0 upto n-1:
		A[i] = u * right rotated (i*360/n);
	endfor;
	A[n] = A[0];

	% Shading of positive triangles.
	addto hexagon contour (0,0) -- A[1] -- A[2] -- cycle withcolor 0.9white;
	addto hexagon contour (0,0) -- A[3] -- A[4] -- cycle withcolor 0.9white;
	addto hexagon contour (0,0) -- A[5] -- A[6] -- cycle withcolor 0.9white;

	for i=0 upto n-1:
		addto hexagon doublepath A[i] -- A[i+1];
	endfor;
	
	addto hexagon doublepath A[0] -- A[3] withcolor 0.7white;
	addto hexagon doublepath A[1] -- A[4] withcolor 0.7white;
	addto hexagon doublepath A[2] -- A[5] withcolor 0.7white;

	addto hexagon contour fullcircle scaled 4bp    shifted A[1] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[1];
	addto hexagon contour fullcircle scaled 4bp    shifted A[3] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[3];
	addto hexagon contour fullcircle scaled 4bp    shifted A[5] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[5];

	addto hexagon doublepath (0,0) withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[0] withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[2] withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[4] withpen pencircle scaled 4bp;
	%addto hexagon also TEX("$\star$");

	pair X, Y, U, V, R, S;
	X := (1.5u, 0);
	Y := (1.5u, u*sqrt(3)/2);
	U := (0, -1.5u);
	V := (1/2u, -1.5u);
	R := (0, +1.5u);
	S := (u, +1.5u);

	pair hexUpRight, hexDown, hexRight;
	hexUpRight := A[1] + A[0];
	hexDown := A[4] + A[5];
	hexRight := A[0];

	draw hexagon;

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);

	drawarrow (0,0) -- (2u, 0);
	drawarrow (0,0) -- (0, 2u);

	label.bot(btex $x$ etex, (1.9u, 0));
	label.lft(btex $y$ etex, (0, 1.9u));
endfig;

beginfig(4)
	u := 1.7cm;

	picture hexagon;
	hexagon := nullpicture;

	pair A[], B[];
	numeric n; n:=6;

	for i=0 upto n-1:
		A[i] = u * right rotated (i*360/n);
	endfor;
	A[n] = A[0];

	% Shading of positive triangles.
	addto hexagon contour (0,0) -- A[1] -- A[2] -- cycle withcolor 0.9white;
	addto hexagon contour (0,0) -- A[3] -- A[4] -- cycle withcolor 0.9white;
	addto hexagon contour (0,0) -- A[5] -- A[6] -- cycle withcolor 0.9white;

	for i=0 upto n-1:
		addto hexagon doublepath A[i] -- A[i+1];
	endfor;
	
	addto hexagon doublepath A[0] -- A[3] withcolor 0.7white;
	addto hexagon doublepath A[1] -- A[4] withcolor 0.7white;
	addto hexagon doublepath A[2] -- A[5] withcolor 0.7white;

	addto hexagon contour fullcircle scaled 4bp    shifted A[1] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[1];
	addto hexagon contour fullcircle scaled 4bp    shifted A[3] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[3];
	addto hexagon contour fullcircle scaled 4bp    shifted A[5] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[5];

	addto hexagon doublepath (0,0) withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[0] withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[2] withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[4] withpen pencircle scaled 4bp;
	%addto hexagon also TEX("$\star$");

	pair X, Y, U, V, R, S;
	X := (1.5u, 0);
	Y := (1.5u, u*sqrt(3)/2);
	U := (0, -1.5u);
	V := (1/2u, -1.5u);
	R := (0, +1.5u);
	S := (u, +1.5u);

	pair hexUpRight, hexDown, hexRight;
	hexUpRight := A[1] + A[0];
	hexDown := A[4] + A[5];
	hexRight := A[0];

	draw hexagon;

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);
endfig;

beginfig(5)
	u := 1.7cm;

	picture hexagon;
	hexagon := nullpicture;

	pair A[], B[];
	numeric n; n:=6;

	for i=0 upto n-1:
		A[i] = u * right rotated (i*360/n);
	endfor;
	A[n] = A[0];

	% Shading of positive triangles.
	addto hexagon contour (0,0) -- A[1] -- A[2] -- cycle withcolor 0.9white;
	addto hexagon contour (0,0) -- A[3] -- A[4] -- cycle withcolor 0.9white;
	addto hexagon contour (0,0) -- A[5] -- A[6] -- cycle withcolor 0.9white;

	for i=0 upto n-1:
		addto hexagon doublepath A[i] -- A[i+1];
	endfor;
	
	%fill fullcircle scaled b shifted p withcolor white;
	%draw fullcircle scaled b shifted p;

	%addto hexagon doublepath fullcircle scaled u;
	%for i=0 upto n-1:
		%addto hexagon doublepath fullcircle scaled u shifted A[i];
	%endfor;

	addto hexagon doublepath A[0] -- A[3] withcolor 0.7white;
	addto hexagon doublepath A[1] -- A[4] withcolor 0.7white;
	addto hexagon doublepath A[2] -- A[5] withcolor 0.7white;

	addto hexagon contour fullcircle scaled 4bp    shifted A[1] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[1];
	addto hexagon contour fullcircle scaled 4bp    shifted A[3] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[3];
	addto hexagon contour fullcircle scaled 4bp    shifted A[5] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[5];

	addto hexagon doublepath (0,0) withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[0] withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[2] withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[4] withpen pencircle scaled 4bp;
	%addto hexagon also TEX("$\star$");

	pair X, Y, U, V, R, S;
	X := (1.5u, 0);
	Y := (1.5u, u*sqrt(3)/2);
	U := (0, -1.5u);
	V := (1/2u, -1.5u);
	R := (0, +1.5u);
	S := (u, +1.5u);

	pair hexUpRight, hexDown, hexRight;
	hexUpRight := A[1] + A[0];
	hexDown := A[4] + A[5];
	hexRight := A[0];

	draw hexagon;

	%label.top(T[1 + 4*j + i], (i*hexUpRight + j*hexDown));

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);

	draw X -- Y;
	draw (X shifted (-0.1u, 0)) -- (X shifted (+0.1u, 0));
	draw (Y shifted (-0.1u, 0)) -- (Y shifted (+0.1u, 0));

	%draw U -- V;
	%draw (U shifted (0, -0.1u)) -- (U shifted (0, +0.1u));
	%draw (V shifted (0, -0.1u)) -- (V shifted (0, +0.1u));

	draw R -- S;
	draw (R shifted (0, -0.1u)) -- (R shifted (0, +0.1u));
	draw (S shifted (0, -0.1u)) -- (S shifted (0, +0.1u));

	label.rt(btex $\sqrt{3}/2$ etex, 1/2[X, Y]);
	%label.bot(btex $1/2$ etex, 1/2[U, V]);
	label.top(btex $1$ etex, 1/2[R, S]);
endfig;

beginfig(6)
	u := 1.3cm;

	picture hexagon;
	hexagon := nullpicture;

	pair A[], B[];
	numeric n; n:=6;

	for i=0 upto n-1:
		A[i] = u * right rotated (i*360/n);
	endfor;
	A[n] = A[0];

	% Shading of positive triangles.
	addto hexagon contour (0,0) -- A[1] -- A[2] -- cycle withcolor 0.9white;
	addto hexagon contour (0,0) -- A[3] -- A[4] -- cycle withcolor 0.9white;
	addto hexagon contour (0,0) -- A[5] -- A[6] -- cycle withcolor 0.9white;

	for i=0 upto n-1:
		addto hexagon doublepath A[i] -- A[i+1];
	endfor;
	
	%fill fullcircle scaled b shifted p withcolor white;
	%draw fullcircle scaled b shifted p;

	%addto hexagon doublepath fullcircle scaled u;
	%for i=0 upto n-1:
		%addto hexagon doublepath fullcircle scaled u shifted A[i];
	%endfor;

	addto hexagon doublepath A[0] -- A[3] withcolor 0.7white;
	addto hexagon doublepath A[1] -- A[4] withcolor 0.7white;
	addto hexagon doublepath A[2] -- A[5] withcolor 0.7white;

	addto hexagon contour fullcircle scaled 4bp    shifted A[1] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[1];
	addto hexagon contour fullcircle scaled 4bp    shifted A[3] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[3];
	addto hexagon contour fullcircle scaled 4bp    shifted A[5] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[5];

	addto hexagon doublepath (0,0) withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[0] withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[2] withpen pencircle scaled 4bp;
	%addto hexagon doublepath A[4] withpen pencircle scaled 4bp;
	%addto hexagon also TEX("$\star$");

	pair hexUp, hexDownLeft, hexUpRight, hexDown, hexRight, hexDownRight, hexUpLeft;
	hexUpRight := A[1] + A[0];
	hexDown := A[4] + A[5];
	hexRight := A[0];
	hexDownRight := A[0] + A[5];
	hexUpLeft := A[2] + A[3];
	hexUp := -hexDown;
	hexDownLeft := -hexUpRight;

	draw hexagon;
	draw hexagon shifted hexUp;
	draw hexagon shifted (4*hexUpRight);
	draw hexagon shifted (4*hexUpRight) shifted hexDownLeft;

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);

	label(TEX("$\star$"), A[0]) shifted hexUp;
	label(TEX("$\star$"), A[2]) shifted hexUp;
	label(TEX("$\star$"), A[4]) shifted hexUp;

	label(TEX("$\star$"), A[0]) shifted (4*hexUpRight);
	label(TEX("$\star$"), A[2]) shifted (4*hexUpRight);
	label(TEX("$\star$"), A[4]) shifted (4*hexUpRight);

	label(TEX("$\star$"), A[0]) shifted (4*hexUpRight) shifted hexDownLeft;
	label(TEX("$\star$"), A[2]) shifted (4*hexUpRight) shifted hexDownLeft;
	label(TEX("$\star$"), A[4]) shifted (4*hexUpRight) shifted hexDownLeft;

	label(btex $r$ etex, (0, u*sqrt(3)/4));
	label(btex $s$ etex, (0, u*sqrt(3)/4) shifted hexUp);
	label(btex $r$ etex, (0, u*sqrt(3)/4) rotated (+120) shifted (4*hexUpRight));
	label(btex $s$ etex, (0, u*sqrt(3)/4) rotated (+120) shifted (4*hexUpRight) shifted hexDownLeft);


	label(btex $h_1$ etex, (0, 0) shifted (-1.5u, 0));
	label(btex $h_j$ etex, (0, 0) shifted (-1.5u, 0) shifted hexUp);
	label(btex $h_i$ etex, (0, 0) shifted (4*hexUpRight) shifted (0, 1.5u));
	label(btex $h_k$ etex, (0, 0) shifted (4*hexUpRight) shifted hexDownLeft shifted (0, 1.5u));

endfig;

beginfig(7)
	u := 0.8cm;

	picture hexagon;
	hexagon := nullpicture;

	pair A[], B[];
	numeric n; n:=6;

	for i=0 upto n-1:
		A[i] = u * right rotated (i*360/n);
	endfor;
	A[n] = A[0];

	% Shading of positive triangles.
	%addto hexagon contour (0,0) -- A[1] -- A[2] -- cycle withcolor 0.9white;
	%addto hexagon contour (0,0) -- A[3] -- A[4] -- cycle withcolor 0.9white;
	%addto hexagon contour (0,0) -- A[5] -- A[6] -- cycle withcolor 0.9white;

	for i=0 upto n-1:
		addto hexagon doublepath A[i] -- A[i+1];
	endfor;
	
	%addto hexagon doublepath A[0] -- A[3] withcolor 0.7white;
	%addto hexagon doublepath A[1] -- A[4] withcolor 0.7white;
	%addto hexagon doublepath A[2] -- A[5] withcolor 0.7white;

	%addto hexagon contour fullcircle scaled 4bp    shifted A[1] withcolor white;
	%addto hexagon doublepath fullcircle scaled 4bp shifted A[1];
	%addto hexagon contour fullcircle scaled 4bp    shifted A[3] withcolor white;
	%addto hexagon doublepath fullcircle scaled 4bp shifted A[3];
	%addto hexagon contour fullcircle scaled 4bp    shifted A[5] withcolor white;
	%addto hexagon doublepath fullcircle scaled 4bp shifted A[5];

	%addto hexagon doublepath (0,0) withpen pencircle scaled 4bp;

	pair hexUp, hexDownLeft, hexUpRight, hexDown, hexRight, hexDownRight, hexUpLeft;
	hexUpRight := A[1] + A[0];
	hexDown := A[4] + A[5];
	hexRight := A[0];
	hexDownRight := A[0] + A[5];
	hexUpLeft := A[2] + A[3];
	hexUp := -hexDown;
	hexDownLeft := -hexUpRight;

	for i=0 upto 5:
	for j=0 upto 4:
		draw hexagon shifted (i*hexUp) shifted (3*j*hexRight);
		draw hexagon shifted (i*hexUp) shifted (3*j*hexRight) shifted hexUpRight;
	endfor;
	endfor;

	% x-axis
	pair Or, firstTickX, firstTickY;

	pair fudge;
	fudge := (+0.7u, 0);
	Or := 2.0*hexDownLeft + fudge;

	drawarrow Or -- (Or + 17*hexRight);
	drawarrow Or -- (Or + 8*hexUp);

	firstTickX := Or + 3*hexRight - fudge;
	firstTickY := Or + 1*hexUp;

	label(btex $0$ etex, firstTickX) shifted (0,-u);
	label(btex $0$ etex, firstTickY) shifted (-u,0);

	draw (firstTickX + 0*1.5*A[0] + (0,+0.1u)) -- (firstTickX + 0*1.5*A[0] + (0,-0.1u));
	draw (firstTickY + 0*0.5*hexUp + (+0.1u,0)) -- (firstTickY + 0*0.5*hexUp + (-0.1u,0));

	for i := 1 upto 9:
		label(TEX("$\frac{" & decimal(3*i) & "}{2}$"), firstTickX + i*1.5*A[0]) shifted (0,-u);
		draw (firstTickX + i*1.5*A[0] + (0,+0.1u)) -- (firstTickX + i*1.5*A[0] + (0,-0.1u));
	endfor;
	label(TEX("$x$"), firstTickX + 10*1.5*A[0]) shifted (0,-u);
	
	for i := 1 upto 11:
		label(TEX("$" & decimal(i) & " \sqrt{3}/2$"), firstTickY + i*0.5*hexUp) shifted (-u,0);
		draw (firstTickY + i*0.5*hexUp + (+0.1u,0)) -- (firstTickY + i*0.5*hexUp + (-0.1u,0));
	endfor;
	label(TEX("$y$"), firstTickY + 12*0.5*hexUp) shifted (-u,0);

	%label(btex $h_1$ etex, (0,0));
	%label(btex $h_2$ etex, 2*1.5*A[0]);
	%label(btex $h_3$ etex, 4*1.5*A[0]);

	label(btex $h_1$ etex, (0, 0));
	label(btex $h_2$ etex, 1*hexUpRight);
	label(btex $h_7$ etex, 2*hexUpRight);
	label(btex $h_5$ etex, 1*hexUp);
	label(btex $h_8$ etex, 2*hexUp);
	label(btex $B$ etex,   3*hexUp);
	label(btex $B$ etex,   4*hexUp);
	label(btex $B$ etex,   5*hexUp);

	label(btex $h_6$ etex, hexUp + hexUpRight);

	label(btex $h_3$ etex, 3*hexRight);
	label(btex $h_4$ etex, 3*hexRight + 1*hexUpRight);
	label(btex $A$ etex, 3*hexRight + 2*hexUpRight);
	label(btex $A$ etex, 6*hexRight + 0*hexUpRight);
	label(btex $A$ etex, 6*hexRight + 1*hexUpRight);
	label(btex $A$ etex, 6*hexRight + 2*hexUpRight);

	label(btex $A$ etex, 9*hexRight + 0*hexUpRight);
	label(btex $A$ etex, 9*hexRight + 1*hexUpRight);
	label(btex $A$ etex, 9*hexRight + 2*hexUpRight);

	label(btex $A$ etex, 12*hexRight + 0*hexUpRight);
	label(btex $A$ etex, 12*hexRight + 1*hexUpRight);

	draw (2*hexUp + hexUpRight) -- (3*hexUpRight) dashed evenly;
	draw (2*hexUp + hexUpRight) -- (2*hexUp + hexUpRight + 4*hexUp) dashed evenly;
	draw (3*hexUpRight) -- (3*hexUpRight + 11*hexRight) dashed evenly;

endfig;


end







