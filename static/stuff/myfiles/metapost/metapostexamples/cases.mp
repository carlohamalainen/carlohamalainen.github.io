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

% Draw a hexagon scaled u, at point p
def makeHexagon(expr u, p, labela, labelb, labelc) =
begingroup
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

	draw hexagon shifted p;

	label(TEX("$\star$"), A[0] shifted p);
	label(TEX("$\star$"), A[2] shifted p);
	label(TEX("$\star$"), A[4] shifted p);

	pair triangleTop, triangleLL, triangleLR;
	triangleTop := 1/2[(0, 0), (1/2[A[1], A[2]])];
	triangleLL := 1/2[(0, 0), (1/2[A[3], A[4]])];
	triangleLR := 1/2[(0, 0), (1/2[A[5], A[6]])];

	label(TEX(labela), triangleTop shifted p);
	label(TEX(labelb), triangleLL shifted p);
	label(TEX(labelc), triangleLR shifted p);
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

	%label(btex $r$ etex, (0, u*sqrt(3)/4));
	%label(btex $s$ etex, (0, u*sqrt(3)/4) shifted hexUp);
	%label(btex $r$ etex, (0, u*sqrt(3)/4) rotated (+120) shifted (4*hexUpRight));
	%label(btex $s$ etex, (0, u*sqrt(3)/4) rotated (+120) shifted (4*hexUpRight) shifted hexDownLeft);
	pair triangleTop, triangleLL, triangleLR;
	triangleTop := 1/2[(0, 0), (1/2[A[1], A[2]])];
	triangleLL := 1/2[(0, 0), (1/2[A[3], A[4]])];
	triangleLR := 1/2[(0, 0), (1/2[A[5], A[6]])];

	label(btex $r$ etex, triangleTop);
	label(btex $s$ etex, triangleLL);
	label(btex $t$ etex, triangleLR);

	label(btex $t$ etex, triangleTop shifted (4*hexUpRight));
	label(btex $r$ etex, triangleLL shifted (4*hexUpRight));
	label(btex $s$ etex, triangleLR shifted (4*hexUpRight));

	label(btex $r'$ etex, triangleTop shifted hexUp);
	label(btex $r'$ etex, triangleLL shifted (4*hexUpRight shifted hexDownLeft));

	label(btex $h$ etex, (0, 0) shifted (-1.5u, 0));
	label(btex $h'$ etex, (0, 0) shifted (-1.5u, 0) shifted hexUp);
	label(btex $k$ etex, (0, 0) shifted (4*hexUpRight) shifted (0, 1.5u));
	label(btex $k'$ etex, (0, 0) shifted (4*hexUpRight) shifted hexDownLeft shifted (0, 1.5u));

endfig;

beginfig(2)
	makeHexagon(u, (0,0), "$r$", "$s$", "$t$");
	makeHexagon(u, (0,0) shifted (1*hexDownLeft), "", "$s'$", "");
	makeHexagon(u, (0,0) shifted (4*hexDownLeft), "$t$", "$r$", "$s$");
	makeHexagon(u, (0,0) shifted (4*hexDownLeft + hexDownRight), "", "", "$s'$");
endfig;

beginfig(3)
	makeHexagon(u, (0,0), "$r$", "$s$", "$t$");
	makeHexagon(u, (0,0) shifted (1*hexDownRight), "", "", "$t'$");
	makeHexagon(u, (0,0) shifted (4*hexDownRight), "$t$", "$r$", "$s$");
	makeHexagon(u, (0,0) shifted (4*hexDownRight + hexUp), "", "", "$t'$");
endfig;

beginfig(4)
	makeHexagon(u, (0,0), "$r$", "$s$", "$t$");
	makeHexagon(u, (0,0) shifted (1*hexDownLeft), "", "$s'$", "");
	makeHexagon(u, (0,0) shifted (4*hexUpLeft), "$t$", "$r$", "$s$");
	makeHexagon(u, (0,0) shifted (4*hexUpLeft + hexDownRight), "", "", "$s'$");
endfig;


end
