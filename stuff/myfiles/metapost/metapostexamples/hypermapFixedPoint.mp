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
	pair A,B,C,D;

	A := (0, 0);
	B := (u, 0);
	D := (u/cosd(30)) * dir -30;

	path AB, AD;

	AB := A .. B;
	AD := A .. D;

	draw AB; draw AD; 

	blackCircle(A, 4bp);

	whiteCircle(B, 4bp);
	whiteCircle(D, 4bp);

	label.top(btex $b$ etex, A);
	label.top(btex $w'$ etex, B);
	label.top(btex $w$ etex, D);

	pair x;
	x := point 0.3*length(AB) of AB;

	path sigma;
	sigma := reverse (x .. (x rotated -15) .. (x rotated -30));
	drawarrow sigma;

	path alpha;
	alpha := fullcircle scaled 0.3u shifted D;
	alpha := subpath(0.25*length(alpha), length(alpha)) of alpha;
	alpha := alpha cutbefore AD;

	drawarrow alpha;

	label.rt(btex $\sigma$ etex, point 0.5*length(sigma) of sigma);
	label.lrt(btex $\alpha$ etex, point 0.8*length(alpha) of alpha);

	label.top(btex $x'$ etex, point 0.5*length(AB) of AB);
	label.urt(btex $x$ etex, point 0.5*length(AD) of AD);
endfig;

beginfig(2)
	u := 2.5cm;
	pair A,B,C,D,E;

	A := (0u, 0);
	B := (1u, 0);
	C := (2u, 0);
	D := (3u, 0);
	E := (u/cosd(30)) * dir +30;

	path AB, BC, CD, AE;

	AB := A .. B;
	BC := B .. C;
	CD := C .. D;
	AE := A .. E;

	draw AB; draw BC; draw CD; draw AE;

	blackCircle(A, 4bp);
	whiteCircle(B, 4bp);
	blackCircle(C, 4bp);
	whiteCircle(D, 4bp);
	whiteCircle(E, 4bp);

	label.top(btex $b$ etex, A);
	label.top(btex $w$ etex, B);
	label.top(btex $b'$ etex, C);
	label.top(btex $w'$ etex, D);
	label.top(btex $w''$ etex, E);

	label.top(btex $x$ etex, point 0.5*length(AB) of AB);
	label.ulft(btex $x \rho$ etex, point 0.5*length(AE) of AE);
	label.top(btex $x \delta$ etex, point 0.5*length(BC) of BC);
	label.top(btex $x \delta \delta'$ etex, point 0.5*length(CD) of CD);

endfig;

beginfig(3)
	u := 2.5cm;
	pair A,B,C,D,E;

	A := (0u, 0);
	B := (1u, 0);
	C := (2u, 0);
	D := (3u, 0);
	E := (u/cosd(30)) * dir +30 shifted (u,0);

	path AB, BC, CD, BE;

	AB := A .. B;
	BC := B .. C;
	CD := C .. D;
	BE := B .. E;

	draw AB; draw BC; draw CD; draw BE;

	blackCircle(A, 4bp);
	whiteCircle(B, 4bp);
	blackCircle(C, 4bp);
	whiteCircle(D, 4bp);
	blackCircle(E, 4bp);

	label.top(btex $b$ etex, A);
	label.top(btex $w$ etex, B);
	label.top(btex $b'$ etex, C);
	label.top(btex $w'$ etex, D);
	label.top(btex $b''$ etex, E);

	label.top(btex $x$ etex, point 0.5*length(AB) of AB);
	label.ulft(btex $x \rho$ etex, point 0.5*length(BE) of BE);
	label.top(btex $x \delta$ etex, point 0.5*length(BC) of BC);
	label.top(btex $x \delta \delta'$ etex, point 0.5*length(CD) of CD);

endfig;

beginfig(4)
	u := 2.5cm;
	pair A,B,C,D;

	A := (0u, 0);
	B := (1u, 0);
	C := (2u, 0);
	D := (3u, 0);

	path AB, BC, CD;

	AB := A .. B;
	BC := B .. C;
	CD := C .. D;

	draw AB; draw BC; draw CD;

	blackCircle(A, 4bp);
	whiteCircle(B, 4bp);
	blackCircle(C, 4bp);
	whiteCircle(D, 4bp);

	label.top(btex $b$ etex, A);
	label.top(btex $w$ etex, B);
	label.top(btex $b'$ etex, C);
	label.top(btex $w'$ etex, D);

	label.top(btex $x$ etex, point 0.5*length(AB) of AB);
	label.top(btex $x \rho = x \delta$ etex, point 0.5*length(BC) of BC);
	label.top(btex $x \delta \delta'$ etex, point 0.5*length(CD) of CD);
endfig;

beginfig(5)
	u := 2.5cm;
	pair A,B,C,D;

	A := (0, 0);
	B := (u, 0);
	D := (u/cosd(30)) * dir -30;

	path AB, AD;

	AB := A .. B;
	AD := A .. D;

	draw AB; draw AD; 

	blackCircle(A, 4bp);

	whiteCircle(B, 4bp);
	whiteCircle(D, 4bp);

	label.top(btex $b$ etex, A);
	label.top(btex $w'$ etex, B);
	label.top(btex $w$ etex, D);

	pair x;
	x := point 0.3*length(AB) of AB;

	path sigma;
	sigma := reverse (x .. (x rotated -(30/2)) .. (x rotated -30));
	drawarrow sigma;

	path alpha;
	alpha := fullcircle scaled 0.3u shifted D;
	alpha := subpath(0.25*length(alpha), length(alpha)) of alpha;
	alpha := alpha cutbefore AD;

	drawarrow alpha;

	label.rt(btex $\hat{\rho}$ etex, point 0.4*length(sigma) of sigma);
	label.lrt(btex $\hat{\mu}$ etex, point 0.8*length(alpha) of alpha);

	label.top(btex $x'$ etex, point 0.5*length(AB) of AB);
	label.urt(btex $x$ etex, point 0.5*length(AD) of AD);
endfig;



end



