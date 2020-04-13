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

vardef TEX primary s =
   write "verbatimtex"                    to "mptextmp.mp";
   write "\documentclass[12pt]{article}"  to "mptextmp.mp";
   %write "\usepackage[T1]{fontenc}"       to "mptextmp.mp";
   %write "\usepackage{amsmath,amssymb}"   to "mptextmp.mp";
   write "\begin{document}"               to "mptextmp.mp";
   write "etex"                           to "mptextmp.mp";
   write "btex "&s&" etex"                to "mptextmp.mp";
   write EOF                              to "mptextmp.mp";
   scantokens "input mptextmp"
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
	u := 1cm;

	pair X;

	X := (0,0);

	blackCircle(X, 4bp);
	blackCircle(X + (1u,0), 4bp);
	blackCircle(X + (2u,0), 4bp);
	blackCircle(X + (3u,0), 4bp);
	blackCircle(X + (4u,0), 4bp);
	%blackCircle(X + (5u,0), 4bp);
	%blackCircle(X + (6u,0), 4bp);
	%blackCircle(X + (7u,0), 4bp);
	%blackCircle(X + (8u,0), 4bp);

	label.rt(btex $5$ etex, (X + (4u,0)));

	label.top(btex $2$ etex, (X + (1u,0)));
	label.top(btex $3$ etex, (X + (2u,0)));
	label.top(btex $4$ etex, (X + (3u,0)));

	drawarrow X -- (X + (1u,0));
	drawarrow (X + (1u,0)) -- (X + (2u,0));
	drawarrow (X + (2u,0)) -- (X + (3u,0));
	drawarrow (X + (3u,0)) -- (X + (4u,0));
	%drawarrow (X + (4u,0)) -- (X + (5u,0));
	%drawarrow (X + (5u,0)) -- (X + (6u,0));
	%drawarrow (X + (6u,0)) -- (X + (7u,0));
	%drawarrow (X + (7u,0)) -- (X + (8u,0));

	blackCircle((4/2)*u*dir(1*180/5) shifted (2u, 0), 4bp) ;
	blackCircle((4/2)*u*dir(2*180/5) shifted (2u, 0), 4bp);
	blackCircle((4/2)*u*dir(3*180/5) shifted (2u, 0), 4bp);
	blackCircle((4/2)*u*dir(4*180/5) shifted (2u, 0), 4bp);

	label.urt(btex $6$ etex, (4/2)*u*dir(1*180/5) shifted (2u, 0));
	label.top(btex $7$ etex, (4/2)*u*dir(2*180/5) shifted (2u, 0));
	label.top(btex $8$ etex, (4/2)*u*dir(3*180/5) shifted (2u, 0));
	label.ulft(btex $9$ etex, (4/2)*u*dir(4*180/5) shifted (2u, 0));
	label.lft(btex $1$ etex, (4/2)*u*dir(5*180/5) shifted (2u, 0));

	label.lrt(btex $13$ etex, (4/2)*u*dir(-1*180/5) shifted (2u, 0));
	label.bot(btex $12$ etex, (4/2)*u*dir(-2*180/5) shifted (2u, 0));
	label.bot(btex $11$ etex, (4/2)*u*dir(-3*180/5) shifted (2u, 0));
	label.llft(btex $10$ etex, (4/2)*u*dir(-4*180/5) shifted (2u, 0));

	blackCircle((4/2)*u*dir(2*180/5) shifted (2u, 0), 4bp);
	blackCircle((4/2)*u*dir(3*180/5) shifted (2u, 0), 4bp);
	blackCircle((4/2)*u*dir(4*180/5) shifted (2u, 0), 4bp);

drawarrow (X + (4u,0)) -- ((4/2)*u*dir(1*180/5) shifted (2u, 0));
drawarrow ((4/2)*u*dir(1*180/5) shifted (2u, 0)) -- ((4/2)*u*dir(2*180/5) shifted (2u, 0));
drawarrow ((4/2)*u*dir(2*180/5) shifted (2u, 0)) -- ((4/2)*u*dir(3*180/5) shifted (2u, 0));
drawarrow ((4/2)*u*dir(3*180/5) shifted (2u, 0)) -- ((4/2)*u*dir(4*180/5) shifted (2u, 0));
drawarrow ((4/2)*u*dir(4*180/5) shifted (2u, 0)) -- ((4/2)*u*dir(5*180/5) shifted (2u, 0));

	blackCircle((4/2)*u*dir(-1*180/5) shifted (2u, 0), 4bp) ;
	blackCircle((4/2)*u*dir(-2*180/5) shifted (2u, 0), 4bp);
	blackCircle((4/2)*u*dir(-3*180/5) shifted (2u, 0), 4bp);
	blackCircle((4/2)*u*dir(-4*180/5) shifted (2u, 0), 4bp);

drawarrow reverse(((4/2)*u*dir(-0*180/5) shifted (2u, 0)) -- ((4/2)*u*dir(-1*180/5) shifted (2u, 0)));
drawarrow reverse(((4/2)*u*dir(-1*180/5) shifted (2u, 0)) -- ((4/2)*u*dir(-2*180/5) shifted (2u, 0)));
drawarrow reverse(((4/2)*u*dir(-2*180/5) shifted (2u, 0)) -- ((4/2)*u*dir(-3*180/5) shifted (2u, 0)));
drawarrow reverse(((4/2)*u*dir(-3*180/5) shifted (2u, 0)) -- ((4/2)*u*dir(-4*180/5) shifted (2u, 0)));
drawarrow reverse(((4/2)*u*dir(-4*180/5) shifted (2u, 0)) -- ((4/2)*u*dir(-5*180/5) shifted (2u, 0)));

	drawarrow reverse(X            .. (X + (0.5u,-0.2u)) .. (X + (1u,0)));
	drawarrow reverse((X + (1u,0)) .. (X + (1.5u,-0.2u)) .. (X + (2u,0)));
	drawarrow reverse((X + (2u,0)) .. (X + (2.5u,-0.2u)) .. (X + (3u,0)));
	drawarrow reverse((X + (3u,0)) .. (X + (3.5u,-0.2u)) .. (X + (4u,0)));

	label(btex $a$ etex, (2u, 1u));
	label(btex $b$ etex, (2u, -1u));
endfig;


end
