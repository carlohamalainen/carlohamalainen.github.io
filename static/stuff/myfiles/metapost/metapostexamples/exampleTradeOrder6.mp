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
	u := 1.4cm;
	offset := 0.2 * u;

	pair right, down;
	right := (u, 0);
	down := (0, -u);

	pair A, B, C, D, E, F, G, H;

	A := (0, 0);
	B := (0, 0) shifted right;
	C := (0, 0) shifted right shifted down;
	D := (0, 0) shifted (2*right) shifted down;
	E := (0, 0) shifted (0*right) shifted (3*down);
	F := (0, 0) shifted (2*right) shifted (3*down);
	G := (0, 0) shifted (0*right) shifted (4*down);
	H := (0, 0) shifted (1*right) shifted (4*down);

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

	% Got them all the wrong way! Ooops!

	drawarrow (A .. (1/2[A,B] + (0, offset)) .. B);
	drawarrow reverse(A .. (1/2[A,B] + (0, -offset)) .. B);

	drawarrow (C .. (1/2[C,D] + (0, offset)) .. D);
	drawarrow reverse(C .. (1/2[C,D] + (0, -offset)) .. D);

	drawarrow (E .. (1/2[E,F] + (0, offset)) .. F);
	drawarrow reverse(E .. (1/2[E,F] + (0, -offset)) .. F);
	
	drawarrow (G .. (1/2[G,H] + (0, offset)) .. H);
	drawarrow reverse(G .. (1/2[G,H] + (0, -offset)) .. H);
	
	drawarrow (B .. (1/2[B,H] + (offset, 0)) .. H);
	drawarrow (H .. (1/2[H,C] + (-offset, 0)) .. C);
	drawarrow (C .. (1/2[C,B] + (-offset, 0)) .. B);

	drawarrow (A .. (1/2[A,E] + (offset, 0)) .. E);
	drawarrow (E .. (1/2[E,G] + (offset, 0)) .. G);
	drawarrow (G .. (1/2[G,A] + (-offset, 0)) .. A);

	drawarrow (D .. (1/2[D,F] + (offset,0)) .. F);
	drawarrow reverse(D .. (1/2[D,F] + (-offset,0)) .. F);
endfig;

end
