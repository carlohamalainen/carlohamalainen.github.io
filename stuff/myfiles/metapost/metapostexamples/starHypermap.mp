%&latex 
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
	u := 2.0cm;

	draw (0,0) -- u*dir(0*360/6);
	draw (0,0) -- u*dir(1*360/6);
	draw (0,0) -- u*dir(2*360/6);
	draw (0,0) -- u*dir(3*360/6);
	draw (0,0) -- u*dir(4*360/6);
	draw (0,0) -- u*dir(5*360/6);

	blackCircle((0,0), 4bp);
	whiteCircle(u*dir(0*360/6), 4bp);
	whiteCircle(u*dir(1*360/6), 4bp);
	whiteCircle(u*dir(2*360/6), 4bp);
	whiteCircle(u*dir(3*360/6), 4bp);
	whiteCircle(u*dir(4*360/6), 4bp);
	whiteCircle(u*dir(5*360/6), 4bp);

	label.top(btex $1$ etex, 0.5*u*dir(0*360/6));
	label.ulft(btex $2$ etex, 0.5*u*dir(1*360/6));
	label.llft(btex $3$ etex, 0.5*u*dir(2*360/6));
	label.bot(btex $4$ etex, 0.5*u*dir(3*360/6));
	label.lrt(btex $5$ etex, 0.5*u*dir(4*360/6));
	label.urt(btex $6$ etex, 0.5*u*dir(5*360/6));

endfig;


end

