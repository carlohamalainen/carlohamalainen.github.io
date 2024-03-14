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

beginfig(1)
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

	addto hexagon contour fullcircle scaled 4bp    shifted A[0] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[0];
	addto hexagon contour fullcircle scaled 4bp    shifted A[2] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[2];
	addto hexagon contour fullcircle scaled 4bp    shifted A[4] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[4];

	addto hexagon doublepath A[1] withpen pencircle scaled 4bp;
	addto hexagon doublepath A[3] withpen pencircle scaled 4bp;
	addto hexagon doublepath A[5] withpen pencircle scaled 4bp;

	pair hexUpRight, hexDown, hexRight, hexUp ;
	hexUpRight := A[1] + A[0];
	hexDown := A[4] + A[5];
	hexUp := -hexDown;
	hexRight := A[0];

	picture T[];
	T[1] = TEX("$" & decimal(1) & "$"); 
	T[2] = TEX("$" & decimal(2) & "$"); 
	T[3] = TEX("$" & decimal(3) & "$"); 
	T[4] = TEX("$" & decimal(4) & "$"); 
	T[5] = TEX("$" & decimal(2) & "$"); 
	T[6] = TEX("$" & decimal(3) & "$"); 
	T[7] = TEX("$" & decimal(4) & "$"); 
	T[8] = TEX("$" & decimal(1) & "$"); 
	T[9] = TEX("$" & decimal(3) & "$"); 
	T[10] = TEX("$" & decimal(4) & "$"); 
	T[11] = TEX("$" & decimal(1) & "$"); 
	T[12] = TEX("$" & decimal(2) & "$"); 
	T[13] = TEX("$" & decimal(4) & "$"); 
	T[14] = TEX("$" & decimal(1) & "$"); 
	T[15] = TEX("$" & decimal(2) & "$"); 
	T[16] = TEX("$" & decimal(3) & "$"); 

	for j = 0 upto 3:
	for i = 0 upto 3:
		draw hexagon shifted (i*hexUpRight + j*hexDown);
	endfor;
	endfor;

	% Extra for torus rollup.
	draw hexagon shifted (0*hexUpRight + 4*hexDown);
	draw hexagon shifted (1*hexUpRight + 4*hexDown);
	draw hexagon shifted (2*hexUpRight + 4*hexDown);
	draw hexagon shifted (3*hexUpRight + 4*hexDown);
	draw hexagon shifted (4*hexUpRight + 4*hexDown);
	draw hexagon shifted (4*hexUpRight + 4*hexDown + 1*hexUp);
	draw hexagon shifted (4*hexUpRight + 4*hexDown + 2*hexUp);
	draw hexagon shifted (4*hexUpRight + 4*hexDown + 3*hexUp);
	draw hexagon shifted (4*hexUpRight + 4*hexDown + 4*hexUp);

	% Fundamental polygon.
draw (0,0) -- (4*hexUpRight) -- (4*hexUpRight + 4*hexDown) -- (4*hexDown) -- cycle withpen pencircle scaled 2bp withcolor 0.7white;

	for j = 0 upto 3:
	for i = 0 upto 3:
		label.top(T[1 + 4*j + i], (i*hexUpRight + j*hexDown));
	endfor;
	endfor;

	label(TEX("$\star$"), (0*hexUpRight + 4*hexDown));
	label(TEX("$\star$"), (1*hexUpRight + 4*hexDown));
	label(TEX("$\star$"), (2*hexUpRight + 4*hexDown));
	label(TEX("$\star$"), (3*hexUpRight + 4*hexDown));
	label(TEX("$\star$"), (4*hexUpRight + 4*hexDown));
	label(TEX("$\star$"), (4*hexUpRight + 4*hexDown + 1*hexUp));
	label(TEX("$\star$"), (4*hexUpRight + 4*hexDown + 2*hexUp));
	label(TEX("$\star$"), (4*hexUpRight + 4*hexDown + 3*hexUp));
	label(TEX("$\star$"), (4*hexUpRight + 4*hexDown + 4*hexUp));

	label.top(btex $3$ etex, 4*hexDown shifted A[3]);
	label.top(btex $4$ etex, 4*hexDown shifted A[4]);
	label.top(btex $1$ etex, 4*hexDown shifted A[5]);
	label.top(btex $1$ etex, 4*hexDown shifted A[6]);
	label.top(btex $1$ etex, 4*hexDown shifted (0,0));

	label.top(btex $2$ etex, (4*hexDown + 1*hexUpRight) shifted A[5]);
	label.top(btex $2$ etex, (4*hexDown + 1*hexUpRight) shifted A[6]);
	label.top(btex $2$ etex, (4*hexDown + 1*hexUpRight) shifted (0,0));

	label.top(btex $3$ etex, (4*hexDown + 2*hexUpRight) shifted A[5]);
	label.top(btex $3$ etex, (4*hexDown + 2*hexUpRight) shifted A[6]);
	label.top(btex $3$ etex, (4*hexDown + 2*hexUpRight) shifted (0,0));

	label.top(btex $4$ etex, (4*hexDown + 3*hexUpRight) shifted A[5]);
	label.top(btex $4$ etex, (4*hexDown + 3*hexUpRight) shifted A[6]);
	label.top(btex $4$ etex, (4*hexDown + 3*hexUpRight) shifted (0,0));

	label.top(btex $1$ etex, (4*hexDown + 4*hexUpRight) shifted A[5]);
	label.top(btex $1$ etex, (4*hexDown + 4*hexUpRight) shifted A[6]);
	label.top(btex $1$ etex, (4*hexDown + 4*hexUpRight) shifted (0,0));

label.top(btex $4$ etex, (4*hexDown + 4*hexUpRight + 1*hexUp) shifted A[5]);
label.top(btex $4$ etex, (4*hexDown + 4*hexUpRight + 1*hexUp) shifted A[6]);
label.top(btex $4$ etex, (4*hexDown + 4*hexUpRight + 1*hexUp) shifted (0,0));

label.top(btex $3$ etex, (4*hexDown + 4*hexUpRight + 2*hexUp) shifted A[5]);
label.top(btex $3$ etex, (4*hexDown + 4*hexUpRight + 2*hexUp) shifted A[6]);
label.top(btex $3$ etex, (4*hexDown + 4*hexUpRight + 2*hexUp) shifted (0,0));

label.top(btex $2$ etex, (4*hexDown + 4*hexUpRight + 3*hexUp) shifted A[5]);
label.top(btex $2$ etex, (4*hexDown + 4*hexUpRight + 3*hexUp) shifted A[6]);
label.top(btex $2$ etex, (4*hexDown + 4*hexUpRight + 3*hexUp) shifted (0,0));

label.top(btex $4$ etex, (4*hexDown + 4*hexUpRight + 4*hexUp) shifted A[1]);
label.top(btex $3$ etex, (4*hexDown + 4*hexUpRight + 4*hexUp) shifted A[2]);
label.top(btex $1$ etex, (4*hexDown + 4*hexUpRight + 4*hexUp) shifted A[5]);
label.top(btex $1$ etex, (4*hexDown + 4*hexUpRight + 4*hexUp) shifted A[6]);
label.top(btex $1$ etex, (4*hexDown + 4*hexUpRight + 4*hexUp) shifted (0,0));

	picture L[];
	L[1] = TEX("$" & decimal(3) & "$");
	L[2] = TEX("$" & decimal(3) & "$");
	L[3] = TEX("$" & decimal(4) & "$");
	L[4] = TEX("$" & decimal(4) & "$");
	L[5] = TEX("$" & decimal(1) & "$");
	L[6] = TEX("$" & decimal(1) & "$");
	L[7] = TEX("$" & decimal(2) & "$");
	L[8] = TEX("$" & decimal(2) & "$");
	L[9] = TEX("$" & decimal(3) & "$");

	L[10] = TEX("$" & decimal(4) & "$");
	L[11] = TEX("$" & decimal(4) & "$");
	L[12] = TEX("$" & decimal(1) & "$");
	L[13] = TEX("$" & decimal(1) & "$");
	L[14] = TEX("$" & decimal(2) & "$");
	L[15] = TEX("$" & decimal(2) & "$");
	L[16] = TEX("$" & decimal(3) & "$");
	L[17] = TEX("$" & decimal(3) & "$");
	L[18] = TEX("$" & decimal(4) & "$");

	L[19] = TEX("$" & decimal(1) & "$");
	L[20] = TEX("$" & decimal(1) & "$");
	L[21] = TEX("$" & decimal(2) & "$");
	L[22] = TEX("$" & decimal(2) & "$");
	L[23] = TEX("$" & decimal(3) & "$");
	L[24] = TEX("$" & decimal(3) & "$");
	L[25] = TEX("$" & decimal(4) & "$");
	L[26] = TEX("$" & decimal(4) & "$");
	L[27] = TEX("$" & decimal(1) & "$");
	
	L[28] = TEX("$" & decimal(2) & "$");
	L[29] = TEX("$" & decimal(2) & "$");
	L[30] = TEX("$" & decimal(3) & "$");
	L[31] = TEX("$" & decimal(3) & "$");
	L[32] = TEX("$" & decimal(4) & "$");
	L[33] = TEX("$" & decimal(4) & "$");
	L[34] = TEX("$" & decimal(1) & "$");
	L[35] = TEX("$" & decimal(1) & "$");
	L[36] = TEX("$" & decimal(2) & "$");

	L[37] = TEX("$" & decimal(3) & "$");
	L[38] = TEX("$" & decimal(4) & "$");
	L[39] = TEX("$" & decimal(4) & "$");
	L[40] = TEX("$" & decimal(1) & "$");
	L[41] = TEX("$" & decimal(1) & "$");
	L[42] = TEX("$" & decimal(2) & "$");
	L[43] = TEX("$" & decimal(2) & "$");
	L[44] = TEX("$" & decimal(3) & "$");
	L[45] = TEX("$" & decimal(3) & "$");




	pair M[];
	M[1] = A[3];
	M[2] = M[1] shifted A[1];
	M[3] = M[2] shifted A[0];
	M[4] = M[3] shifted A[1];
	M[5] = M[4] shifted A[0];
	M[6] = M[5] shifted A[1];
	M[7] = M[6] shifted A[0];
	M[8] = M[7] shifted A[1];
	M[9] = M[8] shifted A[0];
	M[10] = M[9] shifted A[1];  % for the last row, j=4 case.


	for j = 0 upto 3:
	for i = 1 upto 9:
		label.top(L[9*j + i], M[i]) shifted (j*hexDown);
	endfor;
	endfor;

	% last one (j=4) isn't quite the same.
	for i = 1 upto 9:
		label.top(L[9*4 + i], M[i+1]) shifted (4*hexDown);
	endfor;

	% And I stuffed a few on the right, doing 'em manually:
	label.top(TEX("$" & decimal(4) & "$"), 3*hexUpRight) shifted A[0];
	label.top(TEX("$" & decimal(1) & "$"), 3*hexUpRight) shifted A[0] shifted hexDown;
	label.top(TEX("$" & decimal(2) & "$"), 3*hexUpRight) shifted A[0] shifted (2*hexDown);

	% Should move these up a bit (in this file, not graphically).
	for i = 0 upto 3:
	for j = 0 upto 3:
		label(TEX("$\star$"), (i*hexUpRight + j*hexDown));
	endfor;
	endfor;

	pair originAxes;

	originAxes := 5*hexDown - 2*hexRight;

	drawarrow originAxes -- (originAxes + 9*hexRight);
	drawarrow originAxes -- (originAxes + 7*hexUp);

	label.bot(btex $x$ etex, (originAxes + 8.9*hexRight));
	label.lft(btex $y$ etex, (originAxes + 6.9*hexUp));

endfig;



end

beginfig(2)
	u := 1.0cm;

	picture hexagon;
	hexagon := nullpicture;

	pair A[], B[];
	numeric n; n:=6;

	for i=0 upto n-1:
		A[i] = u * right rotated (i*360/n);
	endfor;
	A[n] = A[0];

	pair hexUpRight, hexDown, hexRight;
	hexUpRight := A[1] + A[0];
	hexDown := A[4] + A[5];
	hexRight := A[0];

	addto hexagon doublepath fullcircle scaled (abs(hexUpRight));

	picture T[];
	T[1] = TEX("$" & decimal(1) & "$"); 
	T[2] = TEX("$" & decimal(2) & "$"); 
	T[3] = TEX("$" & decimal(3) & "$"); 
	T[4] = TEX("$" & decimal(4) & "$"); 
	T[5] = TEX("$" & decimal(2) & "$"); 
	T[6] = TEX("$" & decimal(3) & "$"); 
	T[7] = TEX("$" & decimal(4) & "$"); 
	T[8] = TEX("$" & decimal(1) & "$"); 
	T[9] = TEX("$" & decimal(3) & "$"); 
	T[10] = TEX("$" & decimal(4) & "$"); 
	T[11] = TEX("$" & decimal(1) & "$"); 
	T[12] = TEX("$" & decimal(2) & "$"); 
	T[13] = TEX("$" & decimal(4) & "$"); 
	T[14] = TEX("$" & decimal(1) & "$"); 
	T[15] = TEX("$" & decimal(2) & "$"); 
	T[16] = TEX("$" & decimal(3) & "$"); 

	for j = 0 upto 3:
	for i = 0 upto 3:
		draw hexagon shifted (i*hexUpRight + j*hexDown);

		label(T[1 + 4*j + i], (i*hexUpRight + j*hexDown));
	endfor;
	endfor;
endfig;

beginfig(3)
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

	pair hexUpRight, hexDown, hexRight;
	hexUpRight := A[1] + A[0];
	hexDown := A[4] + A[5];
	hexRight := A[0];

	draw hexagon;

	%label.top(T[1 + 4*j + i], (i*hexUpRight + j*hexDown));

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);

endfig;

beginfig(4)
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

	pair hexUpRight, hexDown, hexRight;
	hexUpRight := A[1] + A[0];
	hexDown := A[4] + A[5];
	hexRight := A[0];

	draw hexagon;
	draw hexagon shifted hexUpRight;
	draw hexagon shifted hexUpRight shifted (-xpart(hexUpRight), +ypart(hexUpRight));

	%label.top(T[1 + 4*j + i], (i*hexUpRight + j*hexDown));

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);
	
	label(TEX("$\star$"), A[0] shifted hexUpRight);
	label(TEX("$\star$"), A[2] shifted hexUpRight);
	label(TEX("$\star$"), A[4] shifted hexUpRight);

	pair triangleCenter;

	triangleCenter := 1/2[(0,0), (1/2[A[1], A[2]])];
	drawarrow triangleCenter .. (triangleCenter shifted A[1]) shifted (+0.1*A[1]);
	drawarrow (triangleCenter .. (triangleCenter shifted A[1])) shifted A[4] shifted (-0.1*A[1]);
	drawarrow (triangleCenter .. (triangleCenter shifted A[1])) shifted A[5];
endfig;

beginfig(5)
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

	addto hexagon contour fullcircle scaled 4bp    shifted A[0] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[0];
	addto hexagon contour fullcircle scaled 4bp    shifted A[2] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[2];
	addto hexagon contour fullcircle scaled 4bp    shifted A[4] withcolor white;
	addto hexagon doublepath fullcircle scaled 4bp shifted A[4];

	addto hexagon doublepath A[1] withpen pencircle scaled 4bp;
	addto hexagon doublepath A[3] withpen pencircle scaled 4bp;
	addto hexagon doublepath A[5] withpen pencircle scaled 4bp;

	pair hexUpRight, hexDown, hexRight, hexUp ;
	hexUpRight := A[1] + A[0];
	hexDown := A[4] + A[5];
	hexUp := -hexDown;
	hexRight := A[0];

	for j = 0 upto 3:
	for i = 0 upto 3:
		draw hexagon shifted (i*hexUpRight + j*hexDown);
	endfor;
	endfor;

	% Extra for torus rollup.
	draw hexagon shifted (0*hexUpRight + 4*hexDown);
	draw hexagon shifted (1*hexUpRight + 4*hexDown);
	draw hexagon shifted (2*hexUpRight + 4*hexDown);
	draw hexagon shifted (3*hexUpRight + 4*hexDown);
	draw hexagon shifted (4*hexUpRight + 4*hexDown);
	draw hexagon shifted (4*hexUpRight + 4*hexDown + 1*hexUp);
	draw hexagon shifted (4*hexUpRight + 4*hexDown + 2*hexUp);
	draw hexagon shifted (4*hexUpRight + 4*hexDown + 3*hexUp);
	draw hexagon shifted (4*hexUpRight + 4*hexDown + 4*hexUp);
	label(TEX("$\star$"), (0*hexUpRight + 4*hexDown));
	label(TEX("$\star$"), (1*hexUpRight + 4*hexDown));
	label(TEX("$\star$"), (2*hexUpRight + 4*hexDown));
	label(TEX("$\star$"), (3*hexUpRight + 4*hexDown));
	label(TEX("$\star$"), (4*hexUpRight + 4*hexDown));
	label(TEX("$\star$"), (4*hexUpRight + 4*hexDown + 1*hexUp));
	label(TEX("$\star$"), (4*hexUpRight + 4*hexDown + 2*hexUp));
	label(TEX("$\star$"), (4*hexUpRight + 4*hexDown + 3*hexUp));
	label(TEX("$\star$"), (4*hexUpRight + 4*hexDown + 4*hexUp));

draw (0,0) -- (4*hexUpRight) -- (4*hexUpRight + 4*hexDown) -- (4*hexDown) -- cycle withpen pencircle scaled 2bp;

	% Should move these up a bit (in this file, not graphically).
	for i = 0 upto 3:
	for j = 0 upto 3:
		label(TEX("$\star$"), (i*hexUpRight + j*hexDown));
	endfor;
	endfor;
endfig;




