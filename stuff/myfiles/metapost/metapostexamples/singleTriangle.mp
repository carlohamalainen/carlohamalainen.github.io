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
	pair B,C,D;

	B := u*dir(-10);
	C := u*dir(-10-60);
	D := u*dir(-10) shifted C;

	fill B -- C -- D -- cycle withcolor 0.9white;

	draw C -- D;
	draw C -- B;
	draw D -- B;
	%draw C -- (point 0.9 of (C--B)) dashed withdots;
	%draw D -- (point 0.9 of (D--B)) dashed evenly scaled 1.3;

	blackCircle(C, 4bp);
	whiteCircle(D, 4bp);
	label(btex $\star$ etex, B);
endfig;

end



