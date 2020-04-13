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
	u := 1.0cm;

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
	draw hexagon shifted (1*hexUpRight);

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);
	label(TEX("$\star$"), A[0] shifted (1*hexUpRight));
	label(TEX("$\star$"), A[2] shifted (1*hexUpRight));
	label(TEX("$\star$"), A[4] shifted (1*hexUpRight));

	label.top(btex $r$ etex, triangleTop);
	%label.llft(btex $s$ etex, triangleLL);
	%label.lrt(btex $t$ etex, triangleLR);

	%label.top(btex $s$ etex, triangleTop shifted (1*hexUpRight));
	label.llft(btex $r$ etex, triangleLL shifted (1*hexUpRight));
	%label.lrt(btex $r$ etex, triangleLR shifted (1*hexUpRight));

	%label(btex $h$ etex, (0, -1.3u));
	%label(btex $h'$ etex, (0, -1.3u) + 3*hexRight);
	%label(btex $h''$ etex, (0, -1.3u) + 6*hexRight);

	label(btex $h_1$ etex, (0, -1.3u));
	label(btex $h_2$ etex, (0, -1.3u) + hexUpRight);
endfig;

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
	draw hexagon shifted (hexUpRight);
	draw hexagon shifted (hexUpRight + hexDownRight);

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);
	label(TEX("$\star$"), A[0] shifted (1*hexUpRight));
	label(TEX("$\star$"), A[2] shifted (1*hexUpRight));
	label(TEX("$\star$"), A[4] shifted (1*hexUpRight));

	label.top(btex $r$ etex, triangleTop);
	%label.llft(btex $s$ etex, triangleLL);
	%label.lrt(btex $t$ etex, triangleLR);

	label.top(btex $s$ etex, triangleTop + hexUpRight);
	label.llft(btex $s$ etex, triangleLL + hexUpRight);

	%label.top(btex $s$ etex, triangleTop shifted (1*hexUpRight));
	label.llft(btex $r$ etex, triangleLL shifted (hexUpRight + hexDownRight));
	%label.lrt(btex $r$ etex, triangleLR shifted (1*hexUpRight));

	%label(btex $h$ etex, (0, -1.3u));
	%label(btex $h'$ etex, (0, -1.3u) + 3*hexRight);
	%label(btex $h''$ etex, (0, -1.3u) + 6*hexRight);

	label(btex $h_1$ etex, (0, -1.3u));
	label(btex $h_3$ etex, (0, -1.3u) + hexUpRight + hexDownRight);

endfig;

beginfig(3)
	u := 1.0cm;

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
	draw hexagon shifted (1*hexUpRight);
	draw hexagon shifted (2*hexUpRight);
	draw hexagon shifted (2*hexUpRight + hexDownRight);

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);
	label(TEX("$\star$"), A[0] shifted (1*hexUpRight));
	label(TEX("$\star$"), A[2] shifted (1*hexUpRight));
	label(TEX("$\star$"), A[4] shifted (1*hexUpRight));
	label(TEX("$\star$"), A[0] shifted (2*hexUpRight));
	label(TEX("$\star$"), A[2] shifted (2*hexUpRight));
	label(TEX("$\star$"), A[4] shifted (2*hexUpRight));
	label(TEX("$\star$"), A[0] shifted (2*hexUpRight + hexDownRight));
	label(TEX("$\star$"), A[2] shifted (2*hexUpRight + hexDownRight));
	label(TEX("$\star$"), A[4] shifted (2*hexUpRight + hexDownRight));

	label.top(btex $r$ etex, triangleTop);
	label.top(btex $s$ etex, triangleTop + hexUpRight);
	label.llft(btex $s$ etex, triangleLL + 2*hexUpRight);
	label.llft(btex $r$ etex, triangleLL + 2*hexUpRight + hexDownRight);

	%label.llft(btex $s$ etex, triangleLL);
	%label.lrt(btex $t$ etex, triangleLR);

	%label.top(btex $s$ etex, triangleTop + hexUpRight);
	%label.llft(btex $s$ etex, triangleLL + hexUpRight);

	%label.top(btex $s$ etex, triangleTop shifted (1*hexUpRight));
	%label.llft(btex $r$ etex, triangleLL shifted (hexUpRight + hexDownRight));
	%label.lrt(btex $r$ etex, triangleLR shifted (1*hexUpRight));

	%label(btex $h$ etex, (0, -1.3u));
	%label(btex $h'$ etex, (0, -1.3u) + 3*hexRight);
	%label(btex $h''$ etex, (0, -1.3u) + 6*hexRight);

	label(btex $h_1$ etex, (0, -1.3u));
	label(btex $h_4$ etex, (0, -1.3u) + 2*hexUpRight + hexDownRight);
endfig;

beginfig(4)
	u := 1.0cm;

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
	draw hexagon shifted (hexUp);

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);
	label(TEX("$\star$"), A[0] shifted (1*hexUp));
	label(TEX("$\star$"), A[2] shifted (1*hexUp));
	label(TEX("$\star$"), A[4] shifted (1*hexUp));

	label.top(btex $r$ etex, triangleTop);
	label.llft(btex $r$ etex, triangleLL + hexUp);

	label(btex $h_1$ etex, (-1.3u, 0));
	label(btex $h_5$ etex, (-1.3u, 0) + 1*hexUp);

	% Nasty hack to get some more whitespace into this figure,
	% otherwise the subfigure caption in LaTeX is too narrow.
	label(btex \phantom{x} etex, hexUp + hexUpRight);
endfig;

beginfig(5)
	u := 1.0cm;

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
	draw hexagon shifted (hexUp);
	draw hexagon shifted (2*hexUp + hexDownRight);

	label(TEX("$\star$"), A[0]);
	label(TEX("$\star$"), A[2]);
	label(TEX("$\star$"), A[4]);
	label(TEX("$\star$"), A[0] shifted (1*hexUp));
	label(TEX("$\star$"), A[2] shifted (1*hexUp));
	label(TEX("$\star$"), A[4] shifted (1*hexUp));
	label(TEX("$\star$"), A[0] shifted (2*hexUp) + hexDownRight);
	label(TEX("$\star$"), A[2] shifted (2*hexUp) + hexDownRight);
	label(TEX("$\star$"), A[4] shifted (2*hexUp) + hexDownRight);

	label.top(btex $r$ etex, triangleTop);
	label.llft(btex $r$ etex, triangleLL + hexUp + hexUpRight);

	label.llft(btex $s$ etex, triangleLL + hexUp);
	label.top(btex $s$ etex, triangleTop + hexUp);

	label(btex $h_1$ etex, (-1.3u, 0));
	label(btex $h_6$ etex, (+1.3u, 0) + 2*hexUp + hexDownRight);
endfig;


end







