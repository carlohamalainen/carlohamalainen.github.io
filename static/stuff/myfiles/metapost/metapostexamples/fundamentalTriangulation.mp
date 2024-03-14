verbatimtex
\documentclass[12pt]{article}

% \usepackage[mathlf,minionint]{MinionPro} \usepackage[T1]{fontenc} \usepackage{textcomp}
\usepackage{amssymb}

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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	pair X,A,B,C,D,E,F;

	A := u * dir (0*360/6);
	B := u * dir (1*360/6);
	C := u * dir (2*360/6);
	D := u * dir (3*360/6);
	E := u * dir (4*360/6);
	F := u * dir (5*360/6);
	X := (0, 0);

	fill ((X -- A -- B -- cycle) shifted p) withcolor 0.9white;
	fill ((X -- C -- D -- cycle) shifted p) withcolor 0.9white;
	fill ((X -- E -- F -- cycle) shifted p) withcolor 0.9white;

	draw ((X .. A) shifted p) dashed withdots;
	draw ((X .. C) shifted p) dashed withdots;
	draw ((X .. E) shifted p) dashed withdots;

	%path XB, XD, XF;

	draw ((B .. (point 0.9 of (B..X))) shifted p) dashed evenly scaled 1.3;
	draw ((D .. (point 0.9 of (D..X))) shifted p) dashed evenly scaled 1.3;
	draw ((F .. (point 0.9 of (F..X))) shifted p) dashed evenly scaled 1.3;

	draw A -- B -- C -- D -- E -- F -- cycle shifted p;

	blackCircle(A shifted p, 4bp);
	blackCircle(C shifted p, 4bp);
	blackCircle(E shifted p, 4bp);
	whiteCircle(B shifted p, 4bp);
	whiteCircle(D shifted p, 4bp);
	whiteCircle(F shifted p, 4bp);
	label(btex $\star$ etex, X shifted p);
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
u := 2cm;

pair A[], B[];
numeric n; n:=6;

for i=0 upto n-1:
	A[i] = u * right rotated (i*360/n);
endfor;
A[n] = A[0];

pair triangleTopRight, triangleTopLeft, triangleBottom; 
% triangleTop := 1/2[(0, 0), (1/2[A[1], A[2]])];
% triangleLL := 1/2[(0, 0), (1/2[A[3], A[4]])];
% triangleLR := 1/2[(0, 0), (1/2[A[5], A[6]])];
triangleTopRight := 1/2[(0, 0), (1/2[A[0], A[1]])];
triangleTopLeft := 1/2[(0, 0), (1/2[A[2], A[3]])];
triangleBottom := 1/2[(0, 0), (1/2[A[4], A[5]])];

pair hexUp, hexDownLeft, hexUpRight, hexDown, hexRight, hexLeft, hexDownRight, hexUpLeft;
hexUpRight := A[1] + A[0];
hexDown := A[4] + A[5];
hexRight := A[0];
hexLeft := -hexRight;
hexDownRight := A[0] + A[5];
hexUpLeft := A[2] + A[3];
hexUp := -hexDown;
hexDownLeft := -hexUpRight;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

beginfig(1)
	makeHexagon(u, (0,0), "", "", "");
	makeHexagon(u, (0,0)+hexUpRight, "", "", "");
	makeHexagon(u, (0,0)+hexDownRight, "", "", "");
	makeHexagon(u, (0,0)+hexDown, "", "", "");

	drawarrow triangleTopRight .. (triangleTopRight rotated (360/6)) .. triangleTopLeft;
	label.top(TEX("$\varphi$"), (triangleTopRight rotated (360/6)));

	drawarrow ((triangleTopRight .. (triangleTopRight rotated (360/6)) .. triangleTopLeft) shifted (u,0));
	label.top(TEX("$\sigma$"), (triangleTopRight rotated (360/6)) shifted (u,0));

	drawarrow ((triangleTopRight .. (triangleTopRight rotated (360/6)) .. triangleTopLeft) shifted ((-u,0)+hexDownRight));
	label.top(TEX("$\alpha$"), (triangleTopRight rotated (360/6)) shifted ((-u,0)+hexDownRight));

endfig;

end








