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
	u := 2cm;
	pair A,B,C,D;

	A := (0, 0);
	B := u*dir(-10);
	C := u*dir(-10-60);
	D := u*dir(-10) shifted C;

	fill B -- C -- D -- cycle withcolor 0.9white;

	draw A -- C;
	draw C -- D;
	draw C -- (point 0.9 of (C--B)) dashed withdots;
	draw A -- (point 0.9 of (A--B)) dashed evenly scaled 1.3;
	draw D -- (point 0.9 of (D--B)) dashed evenly scaled 1.3;

	whiteCircle(A, 4bp);
	blackCircle(C, 4bp);
	whiteCircle(D, 4bp);
	label(btex $\star$ etex, B);

	x := 0.4; drawdblarrow x[A,D] -- (1-x)[A,D];

	label.llft(btex $b$ etex, x[A,D]);
endfig;

beginfig(2)
	u := 2cm;
	pair A,B,C,D;

	A := (0, 0);
	B := u*dir(60);
	C := u*dir(0);
	D := u*dir(-60);
	
	fill A -- B -- C -- cycle withcolor 0.9white;

	draw A -- C;
	%draw C -- D;
	draw A -- (point 0.9 of (A--B)) dashed withdots;
	draw A -- (point 0.9 of (A--D)) dashed withdots;
	draw C -- (point 0.9 of (C--B)) dashed evenly scaled 1.3;
	draw C -- (point 0.9 of (C--D)) dashed evenly scaled 1.3;
	%draw D -- (point 0.9 of (D--B)) dashed evenly scaled 1.3;

	whiteCircle(C, 4bp);
	blackCircle(A, 4bp);
	label(btex $\star$ etex, B);
	label(btex $\star$ etex, D);
	
	x := 0.4; drawdblarrow x[B,D] -- (1-x)[B,D];
	label.lft(btex $c$ etex, (1-x)[B,D]);
endfig;

beginfig(3)
	u := 2cm;
	pair A,B,C,D;

	A := (0, 0);
	C := u*dir(60);
	B := u*dir(0);
	D := u*dir(0) shifted C;
	
	fill A -- B -- C -- cycle withcolor 0.9white;

	draw A -- B;
	draw B -- D;
	draw A -- (point 0.95 of (A--C)) dashed withdots;
	draw D -- (point 0.95 of (D--C)) dashed withdots;
	draw B -- (point 0.9  of (B--C)) dashed evenly scaled 1.3;
	%draw C -- (point 0.9 of (C--D)) dashed evenly scaled 1.3;
	%draw D -- (point 0.9 of (D--B)) dashed evenly scaled 1.3;

	whiteCircle(B, 4bp);
	blackCircle(A, 4bp);
	blackCircle(D, 4bp);
	label(btex $\star$ etex, C);

	x := 0.4; drawdblarrow x[A,D] -- (1-x)[A,D];
	label.lrt(btex $a$ etex, (1-x)[A,D]);
endfig;


end



