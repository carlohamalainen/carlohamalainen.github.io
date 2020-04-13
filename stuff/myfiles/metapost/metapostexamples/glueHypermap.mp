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

%def drawit (suffix p)(expr pattern) =
  %draw p dashed pattern;
  %p := p shifted (0,-13);
%enddef;
% drawit(p, withdots);
% drawit(p, withdots scaled 2);
% drawit(p, evenly);
% drawit(p, evenly scaled 2);
% drawit(p, evenly scaled 4);
% drawit(p, evenly scaled 6);

% Drawn like a bipartite graph:

beginfig(1)
	u := 1cm;
	pair A,B,C,D;

	A := (0, 0u);
	B := (0, -2u);
	C := (2u, 0);
	D := (2u, -2u);

	path AB, AD, CB, CD;

	AB := A .. B;
	AD := A .. D;
	CB := C .. B;
	CD := C .. D;

	draw AB; draw AD; draw CB; draw CD;

	whiteCircle(B, 4bp);
	whiteCircle(D, 4bp);

	blackCircle(A, 4bp);
	blackCircle(C, 4bp);

	%label.bot(btex $x$ etex, B);

	label.llft(btex $1$ etex, point 0.5*length(AB) of AB);
	label.llft(btex $2$ etex, point 0.3*length(AD) of AD);
	label.ulft(btex $3$ etex, point 0.3*length(CB) of CB);
	label.rt(btex $4$ etex, point 0.5*length(CD) of CD);
	%label.urt(btex $4$ etex, point 0.8*length(BA) of BA);
	%
	%label.llft(btex $2$ etex, point 0.8*length(CD) of CD);
	%label.urt(btex $3$ etex, point 0.8*length(DC) of DC);
endfig;

% Drawn as a cycle, showing the embedding on the sphere.

beginfig(2)
	u := 1cm;
	pair A,B,C,D;

	A := (0, 0u);
	B := (0, -2u);
	C := (2u, -2u);
	D := (2u, 0);

	path AB, AD, CB, CD;

	AB := A .. B;
	AD := A .. D;
	CB := C .. B;
	CD := C .. D;

	draw AB; draw AD; draw CB; draw CD;

	whiteCircle(B, 4bp);
	whiteCircle(D, 4bp);

	blackCircle(A, 4bp);
	blackCircle(C, 4bp);

	%label.bot(btex $x$ etex, B);

	label(btex $\circlearrowleft$ etex, 1/2[A,C]);

	label.rt(btex $1$ etex, point 0.5*length(AB) of AB);
	label.top(btex $2$ etex, point 0.5*length(AD) of AD);
	label.bot(btex $3$ etex, point 0.5*length(CB) of CB);
	label.lft(btex $4$ etex, point 0.5*length(CD) of CD);
	%label.urt(btex $4$ etex, point 0.8*length(BA) of BA);
	%
	%label.llft(btex $2$ etex, point 0.8*length(CD) of CD);
	%label.urt(btex $3$ etex, point 0.8*length(DC) of DC);
endfig;

beginfig(3)
	u := 1cm;
	pair A,B,C,D;
	pair E, F;

	A := (0, 0u);
	B := (0, -2u);
	C := (2u, -2u);
	D := (2u, 0);

	E := 1/2[A,C];
	F := E shifted (3u, 0);

	path AB, AD, CB, CD;

	AB := A .. B;
	AD := A .. D;
	CB := C .. B;
	CD := C .. D;
	
	% Put in shading, use the centre of gravity.

	%%% fill (A -- D -- F .. (D shifted (0,u)) .. cycle) scaled 0.9;

	fill A -- E -- B -- cycle withcolor 0.9white;
	fill C -- E -- D -- cycle withcolor 0.9white;
	fill A .. (D shifted (0,u)) .. F -- D -- cycle withcolor 0.9white;
	fill B .. (C shifted (0,-u)) .. F -- C -- cycle withcolor 0.9white;

	draw AB; draw AD; draw CB; draw CD;

	path BE, DE;
	BE := B .. E; DE := D .. E;

	draw B .. (point 0.9 of BE) dashed evenly scaled 1.3;
	draw D .. (point 0.9 of DE) dashed evenly scaled 1.3;
	draw A .. E dashed withdots;
	draw C .. E dashed withdots;
	
	draw D .. F dashed evenly scaled 1.3;
	draw B .. (C shifted (0,-u)) .. F dashed evenly scaled 1.3;
	
	draw A .. (D shifted (0,u)) .. F dashed withdots;
	draw C .. F dashed withdots;

	% Edge labels:
	%label.rt(btex $1$ etex, point 0.5*length(AB) of AB);
	%label.top(btex $2$ etex, point 0.5*length(AD) of AD);
	%label.bot(btex $3$ etex, point 0.5*length(CB) of CB);
	%label.lft(btex $4$ etex, point 0.5*length(CD) of CD);

	label(btex $\star$ etex, E);
	label(btex $\star$ etex, F);

	whiteCircle(B, 4bp);
	whiteCircle(D, 4bp);

	blackCircle(A, 4bp);
	blackCircle(C, 4bp);

	label.top(btex $0$ etex, A);
	label.lft(btex $1$ etex, B);
	label.bot(btex $1$ etex, C);
	label.top(btex $0$ etex, D);
	label.top(btex $1$ etex, E);
	label.rt(btex $0$ etex, F);
endfig;

beginfig(4)
	u := 1cm;
	pair A,B,C,D;
	pair E, F;

	A := (0, 0u);
	B := (0, -2u);
	C := (2u, -2u);
	D := (2u, 0);

	E := 1/2[A,C];
	F := E shifted (3u, 0);

	path AB, AD, CB, CD;

	AB := A .. B;
	AD := A .. D;
	CB := C .. B;
	CD := C .. D;
	
	% Put in shading, use the centre of gravity.

	%%% fill (A -- D -- F .. (D shifted (0,u)) .. cycle) scaled 0.9;

	fill A -- E -- B -- cycle withcolor 0.9white;
	fill C -- E -- D -- cycle withcolor 0.9white;
	fill A .. (D shifted (0,u)) .. F -- D -- cycle withcolor 0.9white;
	fill B .. (C shifted (0,-u)) .. F -- C -- cycle withcolor 0.9white;

	draw AB; draw AD; draw CB; draw CD;

	path BE, DE;
	BE := B .. E; DE := D .. E;

	draw B .. (point 0.9 of BE) dashed evenly scaled 1.3;
	draw D .. (point 0.9 of DE) dashed evenly scaled 1.3;
	draw A .. E dashed withdots;
	draw C .. E dashed withdots;
	
	draw D .. F dashed evenly scaled 1.3;
	draw B .. (C shifted (0,-u)) .. F dashed evenly scaled 1.3;
	
	draw A .. (D shifted (0,u)) .. F dashed withdots;
	draw C .. F dashed withdots;

	% Edge labels:
	%label.rt(btex $1$ etex, point 0.5*length(AB) of AB);
	%label.top(btex $2$ etex, point 0.5*length(AD) of AD);
	%label.bot(btex $3$ etex, point 0.5*length(CB) of CB);
	%label.lft(btex $4$ etex, point 0.5*length(CD) of CD);

	label(btex $\star$ etex, E);
	label(btex $\star$ etex, F);

	whiteCircle(B, 4bp);
	whiteCircle(D, 4bp);

	blackCircle(A, 4bp);
	blackCircle(C, 4bp);

	label.top(btex $0$ etex, A);
	label.lft(btex $1$ etex, B);
	label.bot(btex $1$ etex, C);
	label.top(btex $0$ etex, D);
	label.top(btex $1$ etex, E);
	label.rt(btex $0$ etex, F);

	drawarrow (1/2[A,D] + (0.1u,0.4u)) .. (0.5*(-u,u)) .. (1/2[A,B] + (0.3u, 0));
	label.ulft(btex $\tau_1$ etex, (0.5*(-u,u)));

endfig;

end



