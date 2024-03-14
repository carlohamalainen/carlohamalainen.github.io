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
	pair A,B,C,D;

	A := (0u, 0);
	B := (2u, 0);
	C := (4u, 0);
	D := (6u, 0);	

	path AB, BA;

	AB := A .. (1/2[A, B] shifted (0, 0.5u)) .. B;
	BA := reverse (A .. (1/2[A, B] shifted (0, -0.5u)) .. B);

	draw AB; draw BA;

	path CD, DC;	
	CD := C .. (1/2[C, D] shifted (0, 0.5u)) .. D;
	DC := reverse (C .. (1/2[C, D] shifted (0, -0.5u)) .. D);
	draw CD; draw DC;
	
	draw A withpen pencircle scaled 4bp;
	draw B withpen pencircle scaled 4bp;
	draw C withpen pencircle scaled 4bp;
	draw D withpen pencircle scaled 4bp;

	label.llft(btex $1$ etex, point 0.8*length(AB) of AB);
	label.urt(btex $4$ etex, point 0.8*length(BA) of BA);
	
	label.llft(btex $2$ etex, point 0.8*length(CD) of CD);
	label.urt(btex $3$ etex, point 0.8*length(DC) of DC);
endfig;

beginfig(2)
	u := 1cm;
	pair A,B,C,D;

	A := (0u, 0);
	B := (2u, 0);
	C := (4u, 0);
	D := (6u, 0);	

	path AB, BA;

	AB := A .. (1/2[A, B] shifted (0, 0.5u)) .. B;
	BA := reverse (A .. (1/2[A, B] shifted (0, -0.5u)) .. B);

	draw AB; draw BA;

	draw A withpen pencircle scaled 4bp;
	draw B withpen pencircle scaled 4bp;

	label.llft(btex $1$ etex, point 0.8*length(AB) of AB);
	label.urt(btex $4$ etex, point 0.8*length(BA) of BA);
	
	label.ulft(btex $3$ etex, point 0.2*length(AB) of AB);
	label.lrt(btex $2$ etex, point 0.2*length(BA) of BA);
endfig;


end



