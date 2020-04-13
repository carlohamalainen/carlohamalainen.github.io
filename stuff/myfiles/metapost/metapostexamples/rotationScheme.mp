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

beginfig(1)
	u := 1.0cm;

	blackCircle((0,0), 4bp);

        numeric n; n:=5;
	for i=0 upto n-1:
		draw (0, 0) -- u*dir (i*360/n);
        endfor;

	label.top(btex $1$ etex, 1/2[(0,0), u*dir (0*360/n)]);
	label.ulft(btex $2$ etex, 1/2[(0,0), u*dir (1*360/n)]);
	label.llft(btex $3$ etex, 1/2[(0,0), u*dir (2*360/n)]);
	label.lrt(btex $4$ etex, 1/2[(0,0), u*dir (3*360/n)]);
	label.rt(btex $5$ etex, 1/2[(0,0), u*dir (4*360/n)]);

	label(btex $(1,2,3,4,5) \in \tau_1 \quad \Leftrightarrow \quad$ etex, (-3u,0));
endfig;


end







