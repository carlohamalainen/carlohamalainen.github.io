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


beginfig(1)
	u := 2.5cm;
	pair A,B,C,D,E;

	A := (0, 0);
	B := (u, 0);
	C := (0, u);
	D := (u, u);
	E := (2u, 0);

	draw A -- B; 
	draw A -- C; 
	draw D -- C; 
	draw B -- D;

	label.bot(btex $1$ etex, 1/2[A,B]);
	label.lft(btex $2$ etex, 1/2[A,C]);
	label.top(btex $3$ etex, 1/2[D,C]);
	label.rt(btex $4$ etex, 1/2[B,D]);
	label.top(btex $5$ etex, (1/2[B,E] shifted (0,+0.1u)));	
	label.bot(btex $6$ etex, (1/2[B,E] shifted (0,-0.1u)));

	draw B .. (1/2[B,E] shifted (0,+0.1u)) .. E; 
	draw B .. (1/2[B,E] shifted (0,-0.1u)) .. E; 

	blackCircle(A, 4bp);
	blackCircle(D, 4bp);
	blackCircle(E, 4bp);

	whiteCircle(C, 4bp);
	whiteCircle(B, 4bp);
endfig;



end



