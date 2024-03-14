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

	pair A, B, C, D, E, F, G, H, I, J;

	A := 3u * dir 90;
	B := A + (u, 0);
	C := B + (u, 0);
	D := C + (u, 0);
	
	E := 2u * dir 90;
	F := E + (u, 0);
	
	G := (1u * dir 90) + (2u, 0);
	H := G + (u, 0);
	
	I := 0u * dir 90;
	J := I + (2u, 0);

        pickup pencircle scaled 4pt;
        draw A;
        draw B;
        draw C;
        draw D;
        draw E;
        draw F;
        draw G;
        draw H;
        draw I;
        draw J;
        pickup defaultpen;

	% Got them all the wrong way! Ooops!

	drawarrow reverse(A .. (1/2[A,B] + (0, offset)) .. B);
	drawarrow reverse(B .. (1/2[B,C] + (0, offset)) .. C);
	drawarrow reverse(C .. (1/2[C,D] + (0, offset)) .. D);
	drawarrow reverse(D .. (1/2[D,A] + (0, -offset)) .. A);

	drawarrow reverse(E .. (1/2[E,F] + (0, offset)) .. F);
	drawarrow reverse(F .. (1/2[F,E] + (0, -offset)) .. E);

	drawarrow reverse(G .. (1/2[G,H] + (0, offset)) .. H);
	drawarrow reverse(H .. (1/2[H,G] + (0, -offset)) .. G);

	drawarrow reverse(I .. (1/2[I,J] + (0, offset)) .. J);
	drawarrow reverse(J .. (1/2[J,I] + (0, -offset)) .. I);
	
	drawarrow reverse(A .. (1/2[A,I] + (-offset, 0)) .. I);
	drawarrow reverse(I .. (1/2[I,E] + (+offset, 0)) .. E);
	drawarrow reverse(E .. (1/2[E,A] + (+offset, 0)) .. A);
	
	drawarrow reverse(B .. (1/2[B,F] + (-offset, 0)) .. F);
	drawarrow reverse(F .. (1/2[F,B] + (+offset, 0)) .. B);
	
	drawarrow reverse(C .. (1/2[C,J] + (-offset, 0)) .. J);
	drawarrow reverse(J .. (1/2[J,G] + (+offset, 0)) .. G);
	drawarrow reverse(G .. (1/2[G,C] + (+offset, 0)) .. C);
	
	drawarrow reverse(D .. (1/2[D,H] + (-offset, 0)) .. H);
	drawarrow reverse(H .. (1/2[H,D] + (+offset, 0)) .. D);

endfig;

beginfig(2)
	u := 1.4cm;
	offset := 0.2 * u;

	pair A, B, C, D, E, F, G, H, I, J;

	A := 3u * dir 90;
	B := A + (u, 0);
	C := B + (u, 0);
	D := C + (u, 0);
	
	E := 2u * dir 90;
	F := E + (u, 0);
	
	G := (1u * dir 90) + (2u, 0);
	H := G + (u, 0);
	
	I := 0u * dir 90;
	J := I + (2u, 0);

        pickup pencircle scaled 4pt;
        draw A;
        draw B;
        draw C;
        draw D;
        draw E;
        draw F;
        draw G;
        draw H;
        draw I;
        draw J;
        pickup defaultpen;

	drawarrow reverse(B .. (1/2[B,E] + (offset * dir (90+45))) .. E);
	drawarrow reverse(E .. (1/2[E,B] + (offset * dir (-45))) .. B);
	
	drawarrow reverse(C .. (1/2[C,F] + (offset * dir (90+45))) .. F);
	drawarrow reverse(F .. (1/2[F,I] + (offset * dir (90+45))) .. I);
	drawarrow reverse(I .. (1/2[I,C] + (offset * dir (-45))) .. C);
	
	drawarrow reverse(D .. (1/2[D,G] + (offset * dir (90+45))) .. G);
	drawarrow reverse(G .. (1/2[G,D] + (offset * dir (-45))) .. D);

	drawarrow reverse(A{dir 45} .. (D + (1 * offset * dir(45))) .. H{dir (270-45)});
	drawarrow reverse(H .. J);
	drawarrow reverse(J .. (I + (1 * offset * dir(270-45))) .. A);

endfig;

beginfig(3)
	u := 1.4cm;
	offset := 0.2 * u;

	pair A, B, C, D, E, F, G, H, I, J;

	A := 3u * dir 90;
	B := A + (u, 0);
	C := B + (u, 0);
	D := C + (u, 0);
	
	E := 2u * dir 90;
	F := E + (u, 0);
	
	G := (1u * dir 90) + (2u, 0);
	H := G + (u, 0);
	
	I := 0u * dir 90;
	J := I + (2u, 0);

	label(btex $1$ etex, A);
	label(btex $4$ etex, B);
	label(btex $3$ etex, C);
	label(btex $2$ etex, D);
	label(btex $4$ etex, E);
	label(btex $3$ etex, F);
	label(btex $2$ etex, G);
	label(btex $1$ etex, H);
	label(btex $3$ etex, I);
	label(btex $1$ etex, J);

endfig;

end
