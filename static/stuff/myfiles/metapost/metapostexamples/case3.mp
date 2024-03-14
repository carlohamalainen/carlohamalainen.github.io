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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
u := 1.3cm;
pair A[];
numeric n; n:=6;

for i=0 upto n-1:
	A[i] = u * right rotated (i*360/n);
endfor;
A[n] = A[0];

pair hexUp, hexDownLeft, hexUpRight, hexDown, hexRight, hexLeft, hexDownRight, hexUpLeft;
hexUpRight := A[1] + A[0];
hexDown := A[4] + A[5];
hexRight := A[0];
hexLeft := -hexRight;
hexDownRight := A[0] + A[5];
hexUpLeft := A[2] + A[3];
hexUp := -hexDown;
hexDownLeft := -hexUpRight;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Case 3a
beginfig(1)
	makeHexagon(u, (0,0), "$r$", "$s$", "$t$");
	makeHexagon(u, (0,0) shifted (hexUpRight), "$w$", "", "");

	makeHexagon(u, (0,0) shifted (4*hexDownRight), "$t$", "$r$", "$s$");
	makeHexagon(u, (0,0) shifted (4*hexDownRight + hexUpLeft), "", "$w$", "");

	label(btex $h$ etex, (0,0) shifted (1*hexDown));
	label(btex $h'$ etex, (0,0) shifted (hexUpRight + 1*hexDown));
	label(btex $k$ etex, (0,0) shifted (4*hexDownRight + 1*hexDown));
	label(btex $k'$ etex, (0,0) shifted (4*hexDownRight + hexUpLeft + 1*hexDown));
endfig;

% Case 3b
beginfig(2)
	makeHexagon(u, (0,0), "$r$", "$s$", "$t$");
	makeHexagon(u, (0,0) shifted (hexDownRight), "", "", "$w$");

	makeHexagon(u, (0,0) shifted (4*hexDownRight), "$t$", "$r$", "$s$");
	makeHexagon(u, (0,0) shifted (4*hexDownRight + hexUp), "$w$", "", "");

	label(btex $h$ etex, (0,0) shifted (1*hexDown));
	label(btex $h'$ etex, (0,0) shifted (hexDownRight + 1*hexDown));
	label(btex $k$ etex, (0,0) shifted (4*hexDownRight + 1.5*hexRight));
	label(btex $k'$ etex, (0,0) shifted (4*hexDownRight + hexUp + 1.5*hexRight));
endfig;

% Case 3c
beginfig(3)
	makeHexagon(u, (0,0), "$r$", "$s$", "$t$");
	makeHexagon(u, (0,0) shifted (hexDown), "$t$", "$r$", "$s$");

	label(btex $h$ etex, (0,0) shifted (1.5*hexLeft));
	label(btex $k$ etex, (0,0) shifted (hexDown + 1.5*hexLeft));
endfig;

% Case 3d
beginfig(4)
	makeHexagon(u, (0,0), "$r$", "$s$", "$t$");
	makeHexagon(u, (0,0) shifted (hexDownRight), "$t$", "$r$", "$s$");

	label(btex $h$ etex, (0,0) shifted (1.5*hexLeft));
	label(btex $k$ etex, (0,0) shifted (hexDownRight + 1.5*hexRight));
endfig;

% Case 3e
beginfig(5)
	makeHexagon(u, (0,0), "$r$", "$s$", "$t$");
	makeHexagon(u, (0,0) shifted (hexDownRight), "$w$", "", "$w$");
	makeHexagon(u, (0,0) shifted (hexDownRight + hexDown), "$t$", "$r$", "$s$");

	label(btex $h$ etex, (0,0) shifted (1.5*hexLeft));
	label(btex $k$ etex, (0,0) shifted (hexDownRight + hexDown + 1.5*hexRight));
endfig;



end












