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
	u := 2cm;

	%draw fullcircle xscaled 2u yscaled u;

	%path largeArc; largeArc := fullcircle xscaled 2u yscaled u;
	%path smallArc; smallArc := fullcircle xscaled 3u yscaled u;

	%a := 0.3; b := 0.3;
	%draw subpath((0.75-a)*length(largeArc), (0.75+a)*length(largeArc)) of largeArc;
	%draw subpath((0.25-a)*length(largeArc), (0.25+a)*length(largeArc)) of smallArc;

	path bottomArc; bottomArc := (0,0) .. (u, -0.3u) .. (2u, 0);

	x := 0.1;
	path topArc; topArc := (point x*length(bottomArc) of bottomArc) 
			.. (point 0.5*length(bottomArc) of bottomArc shifted (0, 0.5u)) 
			.. (point (1-x)*length(bottomArc) of bottomArc);

	draw bottomArc; draw topArc;

	% a := 3; draw (0,0) .. ((a/2)*u, -0.5u) .. (a*u, 0) .. ((a/2)*u, +0.5u) .. cycle shifted (-(a/4)*u, 0);

	a := 3;	
	draw fullcircle xscaled (a*u) yscaled 1.5u shifted (u, 0);

endfig;

end;

	draw fullcircle xscaled 2u yscaled u rotated -45 shifted (8u,0);
	fill fullcircle scaled u shifted (0,-2u);
	fill halfcircle--cycle scaled u shifted (2u,-2u);
	path quarterdisk; quarterdisk := quartercircle--origin--cycle;
	fill quarterdisk scaled u shifted (4u,-2u);
	fill quarterdisk scaled u rotated -45 shifted (6u,-2u);
	fill quarterdisk scaled u shifted (6u,-2u) rotated 45;
	fill quarterdisk rotated -90 scaled 2u shifted (8u,3u);
	fill unitsquare scaled u shifted (0,2u);
	fill unitsquare xscaled u yscaled 3/2u shifted (2u,2u);
	

