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

	for j = 0 upto 1:
	for i = 0 upto 2:
		draw hexagon shifted (i*hexUpRight + j*hexDown);
	endfor;
	endfor;

	% Should move these up a bit (in this file, not graphically).
	for i = 0 upto 1:
	for j = 0 upto 1:
		label(TEX("$\star$"), (i*hexUpRight + j*hexDown));
	endfor;
	endfor;

	pair triangleTop, triangleLL, triangleLR;
	triangleTop := 1/2[(0, 0), (1/2[A[1], A[2]])];
	triangleLL := 1/2[(0, 0), (1/2[A[3], A[4]])];
	triangleLR := 1/2[(0, 0), (1/2[A[5], A[6]])];

	drawarrow triangleTop .. (triangleTop + 3*hexRight);
	drawarrow (triangleTop + 1.0*hexDown) .. (triangleTop - (0, 0.1u));

endfig;

end

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

	draw hexagon;
	draw hexagon shifted (2*hexRight);
	
	% draw fullcircle scaled 4bp shifted (0,0) withcolor black;
	draw (0,0) withpen pencircle scaled 4bp;
	draw (0,0) withpen pencircle scaled 4bp shifted (2*hexRight + A[0]);
	draw (0,0) withpen pencircle scaled 4bp shifted (2*hexRight + A[2]);
	draw (0,0) withpen pencircle scaled 4bp shifted (2*hexRight + A[4]);

	picture picA;
	picA := nullpicture;
	addto picA contour fullcircle scaled 4bp    shifted A[1] withcolor white;
	addto picA doublepath fullcircle scaled 4bp shifted A[1];
	addto picA contour fullcircle scaled 4bp    shifted A[3] withcolor white;
	addto picA doublepath fullcircle scaled 4bp shifted A[3];
	addto picA contour fullcircle scaled 4bp    shifted A[5] withcolor white;
	addto picA doublepath fullcircle scaled 4bp shifted A[5];
	draw picA;

	picture picB;
	picB := nullpicture;
	addto picB contour fullcircle scaled 4bp    shifted (2*hexRight) withcolor white;
	addto picB doublepath fullcircle scaled 4bp shifted (2*hexRight);
	draw picB;

	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);

	label(TEX("$\star$"), A[1]) shifted (2*hexRight);
	label(TEX("$\star$"), A[3]) shifted (2*hexRight);
	label(TEX("$\star$"), A[5]) shifted (2*hexRight);

	pair triangleTop, triangleLL, triangleLR;
	triangleTop := 1/2[(0, 0), (1/2[A[1], A[2]])];
	triangleLL := 1/2[(0, 0), (1/2[A[3], A[4]])];
	triangleLR := 1/2[(0, 0), (1/2[A[5], A[6]])];

	drawarrow triangleLL .. triangleLR;
	drawarrow triangleLR .. (triangleLL + 2*hexRight);
	drawarrow (triangleLL + 2*hexRight) .. (triangleLR + 2*hexRight);
endfig;



