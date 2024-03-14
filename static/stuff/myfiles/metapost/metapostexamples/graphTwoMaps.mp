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
	u := 1cm;
	s := 2.5*u;
	pair X,A,B,C,D,E,F;
	
	%X := (0, 0);

	%A := (d/2, 0);
	%B := A + 0.75*d*dir 50;
	%C := A + 0.75*d*dir -50;
	%draw A withpen pencircle scaled 4bp;
	%draw B withpen pencircle scaled 4bp;
	%draw C withpen pencircle scaled 4bp;

	draw fullcircle scaled s;
	draw fullcircle scaled s shifted (2*s, 0);

	A := (s/2, 0);
	B := A + 0.5*s*dir 60;
	C := A + 0.5*s*dir -60;

	draw A -- B;
	draw A -- C;

	D := (2*s, 0);
	E := (2*s + s/2, 0);
	F := (2*s + s/2 + s/2, 0);

	draw D -- E;
	draw E -- F;
	
	draw A withpen pencircle scaled 4bp;
	draw B withpen pencircle scaled 4bp;
	draw C withpen pencircle scaled 4bp;
	draw D withpen pencircle scaled 4bp;
	draw E withpen pencircle scaled 4bp;
	draw F withpen pencircle scaled 4bp;
	

endfig;

beginfig(2)
	u := 1cm;
	s := 2.5*u;
	pair X,A,B,C,D,E,F;
	
	%X := (0, 0);

	%A := (d/2, 0);
	%B := A + 0.75*d*dir 50;
	%C := A + 0.75*d*dir -50;
	%draw A withpen pencircle scaled 4bp;
	%draw B withpen pencircle scaled 4bp;
	%draw C withpen pencircle scaled 4bp;

	draw fullcircle scaled s;

	A := (s/2, 0);
	B := A + 0.5*s*dir 60;
	C := A + 0.5*s*dir -60;

	draw A -- B;
	draw A -- C;

	draw A withpen pencircle scaled 4bp;
	draw B withpen pencircle scaled 4bp;
	draw C withpen pencircle scaled 4bp;
	
	label.ulft(btex $1$ etex, A) shifted (-0.05u, 0.1u);
	label.llft(btex $2$ etex, A) shifted (-0.05u, -0.1u);
	label.urt(btex $3$ etex, A) shifted (0, 0.35u);
	label.lrt(btex $4$ etex, A) shifted (0.1u, 0);

	label.bot(btex $5$ etex, B) shifted (0, -0.15u);
	label.top(btex $6$ etex, C) shifted (-0.2u, -0.15u);

	% stupid hack to make the figure wider so that the caption in
	% LaTeX looks nicer.
	label(btex \phantom{hack} etex, (4.5u,0));
endfig;


end







