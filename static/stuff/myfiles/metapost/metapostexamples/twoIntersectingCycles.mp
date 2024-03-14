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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
u := 2cm;
d := 0.2;

beginfig(1)
	drawarrow (0 + d*u,0) -- (u - d*u,0);
	label(btex $x$ etex, (0,0));
	label(btex $y$ etex, (u,0));
	label(btex $z$ etex, (u,-u));

	% horizontal arrows and dots	
	drawarrow (u + d*u,0) -- (2u - d*u,0);
	drawarrow (3u + d*u,0) -- (4u - d*u,0);
	drawarrow (4u + d*u,0) -- (5u - d*u,0);
	draw (2u + 0*u,0) -- (3u - 0*u,0) dashed withdots;

	% vertical arrows	
	drawarrow (u,0 - d*u) -- (u, -u + d*u);
	drawarrow (5u,0 - d*u) -- (5u, -u + d*u);

	% diagonal arrows
	drawarrow (u - 0.5*d*u, -u + 0.5*d*u) -- (0 + 0.5*d*u, 0 - 0.5*d*u);
	drawarrow ((u - 0.5*d*u, -u + 0.5*d*u) -- (0 + 0.5*d*u, 0 - 0.5*d*u)) shifted (4u,0);

	label(btex $x'$ etex, (0,0) shifted (4u,0));
	label(btex $y'$ etex, (u,0) shifted (4u,0));
	label(btex $z'$ etex, (u,-u) shifted (4u,0));

	label.top(btex $\rho_1$ etex, (0.5u, 0));
	label.top(btex $\rho_1$ etex, (1.5u, 0));
	label.top(btex $\rho_1$ etex, (3.5u, 0));
	label.top(btex $\rho_1$ etex, (4.5u, 0));

	label.rt(btex $\rho_2$ etex, (u, -0.5u));
	label.rt(btex $\rho_2$ etex, (5u, -0.5u));

	label.llft(btex $\rho_3$ etex, 1/2[(u - 0.5*d*u, -u + 0.5*d*u), (0 + 0.5*d*u, 0 - 0.5*d*u)]);
	label.llft(btex $\rho_3$ etex, 1/2[(5u - 0.5*d*u, -u + 0.5*d*u), (4u + 0.5*d*u, 0 - 0.5*d*u)]);
endfig;

beginfig(2)
	label(btex $x$ etex, (0,0));
	label(btex $x'$ etex, (u,0));
	label(btex $x''$ etex, (u,-u));
	label(btex $y$ etex, (0,-u));

	% horizontal arrows and dots	
	drawarrow (0 + d*u,0) -- (u - d*u,0);
	%drawarrow (u + d*u,0) -- (2u - d*u,0);
	%drawarrow (3u + d*u,0) -- (4u - d*u,0);
	%drawarrow (4u + d*u,0) -- (5u - d*u,0);
	%draw (2u + 0*u,0) -- (3u - 0*u,0) dashed withdots;

	% vertical arrows	
	drawarrow (u,0 - d*u) -- (u, -u + d*u);
	drawarrow (0,0 - d*u) -- (0, -u + d*u);
	%drawarrow (5u,0 - d*u) -- (5u, -u + d*u);

	% diagonal arrows
	drawarrow (u - 0.5*d*u, -u + 0.5*d*u) -- (0 + 0.5*d*u, 0 - 0.5*d*u);
	%drawarrow ((u - 0.5*d*u, -u + 0.5*d*u) -- (0 + 0.5*d*u, 0 - 0.5*d*u)) shifted (4u,0);

	%label(btex $x'$ etex, (0,0) shifted (4u,0));
	%label(btex $y'$ etex, (u,0) shifted (4u,0));
	%label(btex $z'$ etex, (u,-u) shifted (4u,0));

	label.top(btex $\rho_1$ etex, (0.5u, 0));
	%label.top(btex $\rho_1$ etex, (1.5u, 0));
	%label.top(btex $\rho_1$ etex, (3.5u, 0));
	%label.top(btex $\rho_1$ etex, (4.5u, 0));

	label.rt(btex $\rho_2$ etex, (u, -0.5u));
	label.lft(btex $\rho_2$ etex, (0, -0.5u));
	%label.rt(btex $\rho_2$ etex, (5u, -0.5u));

	label.llft(btex $\rho_3$ etex, 1/2[(u - 0.5*d*u, -u + 0.5*d*u), (0 + 0.5*d*u, 0 - 0.5*d*u)]);
	%label.llft(btex $\rho_3$ etex, 1/2[(5u - 0.5*d*u, -u + 0.5*d*u), (4u + 0.5*d*u, 0 - 0.5*d*u)]);
endfig;

end

