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

	pair triangleTop, triangleLL, triangleLR;

	triangleTop := 1/2[(0, 0), (1/2[A[1], A[2]])];
	triangleLL := 1/2[(0, 0), (1/2[A[3], A[4]])];
	triangleLR := 1/2[(0, 0), (1/2[A[5], A[6]])];

        pair hexUp, hexDownLeft, hexUpRight, hexDown, hexRight, hexDownRight, hexUpLeft;
        hexUpRight := A[1] + A[0];
        hexDown := A[4] + A[5];
        hexRight := A[0];
        hexDownRight := A[0] + A[5];
        hexUpLeft := A[2] + A[3];
        hexUp := -hexDown;
        hexDownLeft := -hexUpRight;

	draw hexagon;
	%draw hexagon shifted (3*hexRight);
	draw hexagon shifted (3*hexRight);

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);
	%label(TEX("$\star$"), A[0] shifted (3*hexRight));
	%label(TEX("$\star$"), A[2] shifted (3*hexRight));
	%label(TEX("$\star$"), A[4] shifted (3*hexRight));
	label(TEX("$\star$"), A[0] shifted (3*hexRight));
	label(TEX("$\star$"), A[2] shifted (3*hexRight));
	label(TEX("$\star$"), A[4] shifted (3*hexRight));

	label.top(btex $r$ etex, triangleTop);
	label.llft(btex $s$ etex, triangleLL);
	label.lrt(btex $t$ etex, triangleLR);

	%label.top(btex $s$ etex, triangleTop shifted (3*hexRight));
	%label.llft(btex $t$ etex, triangleLL shifted (3*hexRight));
	%label.lrt(btex $r$ etex, triangleLR shifted (3*hexRight));

	label.top(btex $t$ etex, triangleTop shifted (3*hexRight));
	label.llft(btex $r$ etex, triangleLL shifted (3*hexRight));
	label.lrt(btex $s$ etex, triangleLR shifted (3*hexRight));

	label(btex $h$ etex, (0, -1.3u));
	%label(btex $h'$ etex, (0, -1.3u) + 3*hexRight);
	label(btex $k$ etex, (0, -1.3u) + 3*hexRight);
endfig;


beginfig(2)
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

	pair triangleTop, triangleLL, triangleLR;

	triangleTop := 1/2[(0, 0), (1/2[A[1], A[2]])];
	triangleLL := 1/2[(0, 0), (1/2[A[3], A[4]])];
	triangleLR := 1/2[(0, 0), (1/2[A[5], A[6]])];

        pair hexUp, hexDownLeft, hexUpRight, hexDown, hexRight, hexDownRight, hexUpLeft;
        hexUpRight := A[1] + A[0];
        hexDown := A[4] + A[5];
        hexRight := A[0];
        hexDownRight := A[0] + A[5];
        hexUpLeft := A[2] + A[3];
        hexUp := -hexDown;
        hexDownLeft := -hexUpRight;

	draw hexagon;
	%draw hexagon shifted (3*hexRight);
	draw hexagon shifted (3*hexRight);

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);
	%label(TEX("$\star$"), A[0] shifted (3*hexRight));
	%label(TEX("$\star$"), A[2] shifted (3*hexRight));
	%label(TEX("$\star$"), A[4] shifted (3*hexRight));
	label(TEX("$\star$"), A[0] shifted (3*hexRight));
	label(TEX("$\star$"), A[2] shifted (3*hexRight));
	label(TEX("$\star$"), A[4] shifted (3*hexRight));

	label.top(btex $r$ etex, triangleTop);
	label.llft(btex $s$ etex, triangleLL);
	label.lrt(btex $t$ etex, triangleLR);

	%label.top(btex $s$ etex, triangleTop shifted (3*hexRight));
	%label.llft(btex $t$ etex, triangleLL shifted (3*hexRight));
	%label.lrt(btex $r$ etex, triangleLR shifted (3*hexRight));

	label.top(btex $s$ etex, triangleTop shifted (3*hexRight));
	label.llft(btex $t$ etex, triangleLL shifted (3*hexRight));
	label.lrt(btex $r$ etex, triangleLR shifted (3*hexRight));

	label(btex $h$ etex, (0, -1.3u));
	%label(btex $h'$ etex, (0, -1.3u) + 3*hexRight);
	label(btex $h''$ etex, (0, -1.3u) + 3*hexRight);
endfig;

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

	pair triangleTop, triangleLL, triangleLR;

	triangleTop := 1/2[(0, 0), (1/2[A[1], A[2]])];
	triangleLL := 1/2[(0, 0), (1/2[A[3], A[4]])];
	triangleLR := 1/2[(0, 0), (1/2[A[5], A[6]])];

        pair hexUp, hexDownLeft, hexUpRight, hexDown, hexRight, hexDownRight, hexUpLeft;
        hexUpRight := A[1] + A[0];
        hexDown := A[4] + A[5];
        hexRight := A[0];
        hexDownRight := A[0] + A[5];
        hexUpLeft := A[2] + A[3];
        hexUp := -hexDown;
        hexDownLeft := -hexUpRight;

	draw hexagon;
	%draw hexagon shifted (3*hexRight);
	draw hexagon shifted (3*hexRight);

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);
	%label(TEX("$\star$"), A[0] shifted (3*hexRight));
	%label(TEX("$\star$"), A[2] shifted (3*hexRight));
	%label(TEX("$\star$"), A[4] shifted (3*hexRight));
	label(TEX("$\star$"), A[0] shifted (3*hexRight));
	label(TEX("$\star$"), A[2] shifted (3*hexRight));
	label(TEX("$\star$"), A[4] shifted (3*hexRight));

	label.top(btex  $t$ etex, triangleTop);
	label.llft(btex $r$ etex, triangleLL);
	label.lrt(btex  $s$ etex, triangleLR);

	%label.top(btex $s$ etex, triangleTop shifted (3*hexRight));
	%label.llft(btex $t$ etex, triangleLL shifted (3*hexRight));
	%label.lrt(btex $r$ etex, triangleLR shifted (3*hexRight));

	label.top(btex  $s$ etex, triangleTop shifted (3*hexRight));
	label.llft(btex $t$ etex, triangleLL shifted (3*hexRight));
	label.lrt(btex  $r$ etex, triangleLR shifted (3*hexRight));

	label(btex $h$ etex, (0, -1.3u));
	%label(btex $h'$ etex, (0, -1.3u) + 3*hexRight);
	label(btex $k$ etex, (0, -1.3u) + 3*hexRight);
endfig;


end







