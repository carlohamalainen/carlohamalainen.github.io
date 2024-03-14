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


% % Makes arrow heads go half way.
% save arrowhead;
% vardef arrowhead expr p =
	% save A,u; pair A,u;
	% A := point 1/2length(p) of p;
	% u := unitvector(direction 1/2length(p) of p);
	% A -- (A - ahlength*u rotated 25) --
	% (A - ahlength*u rotated -25) -- cycle
% enddef;

input boxes

scaleFactorX := 3cm;
scaleFactorY := 1.5cm;

% beginfig(1)
def begindiag =
  begingroup;
  save _diag_x, _diag_x_max, _diag_y, _diag_y_max, _diag;
  numeric _diag_x, _diag_x_max, _diag_y, _diag_y_max;
  string _diag[][];
  % Num^^e9ro de ligne et de colonne courrants
  _diag_x = -1; _diag_y = 0;
  % Num^^e9ro de ligne et de colonne maximaux
  _diag_x_max = _diag_y_max = 0;
  save _diag_ar_n, _diag_ar_source, _diag_ar_but, _diag_ar_up, _diag_ar_down, _diag_ar_downleft;
  % Nombre de fl^^e8ches
  numeric _diag_ar_n; _diag_ar_n=-1;
  % Source et but de la fl^^e8che
  pair _diag_ar_source[], _diag_ar_but[];
  % Ce qu'il faut ^^e9crire au dessus ou au dessous
  string _diag_ar_up[], _diag_ar_down[], _diag_ar_downleft[];
  save _diag_ar_curved, _diag_ar_shape, _diag_ar_color, _diag_ar_width;
  % ^^ab courbure ^^bb (c'est une distance)
  numeric _diag_ar_curved[];
  % Forme de la fl^^e8che
  string _diag_ar_shape[];
  % Couleur, ^^e9paisseur, pointill^^e9s
  color _diag_ar_color[];
  numeric _diag_ar_width[];
  picture _diag_ar_dashed[];
enddef;

def node expr A =
  _diag_x := _diag_x + 1;
  _diag_x_max := max(_diag_x,_diag_x_max);
  _diag[_diag_x][_diag_y] := A;
enddef;

def nextline =
  _diag_x := -1;
  _diag_y := _diag_y + 1;
  _diag_y_max := max(_diag_y, _diag_y_max);
enddef;

tertiarydef a => b = a, b enddef;
def even (expr a) = not odd(a) enddef;

vardef rarrowto(expr a,b)(text t) =
  save i,p;
  _diag_ar_n := _diag_ar_n + 1;
  _diag_ar_source[_diag_ar_n] = (_diag_x, _diag_y);
  _diag_ar_but[_diag_ar_n] = (_diag_x + a, _diag_y + b);

  numeric i; i:=0;
  string current;
  for p=t:
    if even(i):
      current := p;
    else:
      if current = "above":
        _diag_ar_up[_diag_ar_n] = p;
      elseif current = "below":
        _diag_ar_down[_diag_ar_n] = p;
      elseif current = "belowleft":
        _diag_ar_downleft[_diag_ar_n] = p;
      elseif current = "shape":
        _diag_ar_shape[_diag_ar_n] = p;
      elseif current = "curved":
        _diag_ar_curved[_diag_ar_n] = p;
      elseif current = "color":
        _diag_ar_color[_diag_ar_n] = p;
      elseif current = "width":
        _diag_ar_width[_diag_ar_n] = p;
      elseif current = "dashed":
        _diag_ar_dashed[_diag_ar_n] = p;
      else:
        errmessage("rarrowto: Wrong argument "&ditto&current&ditto);
      fi;
    fi;
    i := i + 1;
  endfor;
  if odd i:
    errmessage("rarrowto: Odd number of arguments "&decimal(i));
  fi;
enddef;

%% Les t^^eates de fl^^e8ches

picture withsmalldots, notdashed;
withsmalldots := withdots scaled .3;
notdashed := dashpattern(on 50cm);

vardef diag_arrow_head (expr p, t) =
  save A,B,C,u; pair A,B,C,u;
  B := point t of p;
  u := -unitvector(direction t of p);
  A := B + ahlength*u rotated(-ahangle);
  C := B + ahlength*u rotated(+ahangle);
  A .. {-u} B {u} .. C
enddef;

vardef diag_arrow_bar (expr p, t) =
  save A,B,C,u; pair A,B,C,u;
  B := point t of p;
  u := unitvector(direction t of p);
  A := B + ahlength*sind(ahangle)*u rotated(90);
  C := B + ahlength*sind(ahangle)*u rotated(-90);
  A .. B .. C
enddef;

%% Les fl^^e8ches

def diag_draw_arrow_default(suffix a,b)(expr curved, w, col, dash) =
  p = a.c ..
  (1/2 [a.c,b.c] + curved*unitvector(b.c-a.c) rotated 90)
  .. b.c;
  pp := p cutbefore bpath.a cutafter bpath.b;
  draw pp
    withcolor col withpen pencircle scaled w dashed dash;
  draw diag_arrow_head (pp, length(pp))
    withcolor col withpen pencircle scaled w;
enddef;

def diag_draw_arrow_middle(suffix a,b)(expr curved, w, col, dash) =
  p = a.c ..
  (1/2 [a.c,b.c] + curved*unitvector(b.c-a.c) rotated 90)
  .. b.c;
  pp := p cutbefore bpath.a cutafter bpath.b;
  draw pp
    withcolor col withpen pencircle scaled w dashed dash;
  draw diag_arrow_head(p,1)
    withcolor col withpen pencircle scaled w;
enddef;

def diag_draw_arrow_epi(suffix a,b)(expr curved, w, col, dash) =
  p = a.c ..
  (1/2 [a.c,b.c] + curved*unitvector(b.c-a.c) rotated 90)
  .. b.c;
  pp := p cutbefore bpath.a cutafter bpath.b;
  draw pp
    withcolor col withpen pencircle scaled w dashed dash;
  draw diag_arrow_head (pp, length(pp))
    withcolor col withpen pencircle scaled w;
  path ppp;
  ppp := pp cutafter (fullcircle scaled 1mm shifted point length(pp) of pp);
  draw diag_arrow_head(ppp, length(ppp))
    withcolor col withpen pencircle scaled w;
enddef;

def diag_draw_arrow_mono(suffix a,b)(expr curved, w, col, dash) =
  p = a.c ..
  (1/2 [a.c,b.c] + curved*unitvector(b.c-a.c) rotated 90)
  .. b.c;
  pp := p cutbefore bpath.a cutafter bpath.b;
  path ppp;
  ppp := pp cutbefore (fullcircle scaled 1mm shifted point 0 of pp);
  draw ppp
    withcolor col withpen pencircle scaled w dashed dash;
  draw diag_arrow_head (pp, length(pp))
    withcolor col withpen pencircle scaled w;
  draw diag_arrow_head(ppp, 0)
    withcolor col withpen pencircle scaled w;
enddef;

vardef diag_draw_arrow_inj(suffix a,b)(expr curved, w, col, dash) =
  p = a.c ..
  (1/2 [a.c,b.c] + curved*unitvector(b.c-a.c) rotated 90)
  .. b.c;
  pp := p cutbefore bpath.a cutafter bpath.b;
  path ppp;
  ppp := pp cutbefore (fullcircle scaled 1mm shifted point 0 of pp);
  draw ppp
    withcolor col withpen pencircle scaled w dashed dash;
  draw diag_arrow_head (pp, length(pp))
    withcolor col withpen pencircle scaled w;
  save u,A,B,C;
  pair u,A,B,C;
  A := point 0 of ppp;
  u := unitvector(direction 0 of ppp);
  B := A + ahlength*(-u) rotated (-ahangle);
  C := A + 2 ahlength*sind(ahangle)*u rotated 90;
  draw C {-u} .. B .. A {u}
  withcolor col withpen pencircle scaled w dashed dash;
enddef;

def diag_draw_arrow_mapsto(suffix a,b)(expr curved, w, col, dash) =
  p = a.c ..
  (1/2 [a.c,b.c] + curved*unitvector(b.c-a.c) rotated 90)
  .. b.c;
  pp := p cutbefore bpath.a cutafter bpath.b;
  draw pp
    withcolor col withpen pencircle scaled w dashed dash;
  draw diag_arrow_head (pp, length(pp))
    withcolor col withpen pencircle scaled w;
  draw diag_arrow_bar (pp, 0)
    withcolor col withpen pencircle scaled w;
enddef;

def diag_draw_arrow_half_dotted(suffix a,b)(expr curved, w, col, dash) =
  p = a.c ..
  (1/2 [a.c,b.c] + curved*unitvector(b.c-a.c) rotated 90)
  .. b.c;
  pp := p cutbefore bpath.a cutafter bpath.b;
  draw subpath(0,1) of pp
    withcolor col withpen pencircle scaled w dashed withsmalldots;
  draw subpath(1,2) of pp
    withcolor col withpen pencircle scaled w;
  draw diag_arrow_head (pp, length(pp))
    withcolor col withpen pencircle scaled w;
enddef;

%% Fin des fl^^e8ches

def color_to_string (expr a) =
  "("&
    decimal(redpart a)
    &","&
    decimal(greenpart a)
    &","&
    decimal(bluepart a)
    &")"
enddef;

def enddiag =
  save i,j,k,l,mm,a,A,p,b;
  for i=0 upto _diag_x_max:
    for j=0 upto _diag_y_max:
      if known _diag[i][j]:
        % circleit.a[i][j]( _diag[i][j] );
	% Use our TEX macro to render the argument as proper TeX/LaTeX
        circleit.a[i][j]( TEX(_diag[i][j]) );

        a[i][j].dx = a[i][j].dy;
        a[i][j].c = (scaleFactorX * i, -scaleFactorY * j);
        drawunboxed( a[i][j] );
      fi;
    endfor;
  endfor;
  for m=0 upto _diag_ar_n:
    % V^^e9rifier que le but existe
    i := xpart _diag_ar_source[m];
    j := ypart _diag_ar_source[m];
    k := xpart _diag_ar_but[m];
    l := ypart _diag_ar_but[m];

    % On trace la fl^^e8che. Le chemin est mis dans la variable p.
    path p,pp;
    if unknown _diag_ar_shape[m]:  _diag_ar_shape[m]  := "default" fi;
    if unknown _diag_ar_color[m]:  _diag_ar_color[m]  := black     fi;
    if unknown _diag_ar_width[m]:  _diag_ar_width[m]  := .5bp      fi;
    if unknown _diag_ar_curved[m]: _diag_ar_curved[m] := 0         fi;
    if unknown _diag_ar_dashed[m]: _diag_ar_dashed[m] := notdashed fi;
    % On ne peut PAS utiliser m dans une cha^^eene ce caract^^e8res que l'on donne
    % ^^e0 scantokens, car m est une variable de boucle. C'est vraiment sp^^e9cial,
    % une variable de boucle.
    mm := m;
    scantokens(
      "diag_draw_arrow_"& _diag_ar_shape[m]
      &"("
        &"a[i][j], a[k][l],_diag_ar_curved[mm],_diag_ar_width[mm],"
        &"_diag_ar_color[mm],_diag_ar_dashed[mm]"
        &");"
      );

    % On ^^e9crit des choses au dessus ou au dessous des fl^^e8ches
    pair A;
    A = point 1/2 length(p) of p;
    if known _diag_ar_up[m]:
      % boxit.b[m](_diag_ar_up[m]);
      boxit.b[m](TEX(_diag_ar_up[m]));
      b[m].c = A + 4bp*unitvector(direction 1/2 length(p) of p rotated 90);
      drawunboxed(b[m]);
    fi;
    if known _diag_ar_down[m]:
      %boxit.c[m](_diag_ar_down[m]);
      boxit.c[m](TEX(_diag_ar_down[m]));
      c[m].c = A + 4bp*unitvector(direction 1/2 length(p) of p rotated -90);
      drawunboxed(c[m]);
    fi;
    if known _diag_ar_downleft[m]:
      %boxit.d[m](_diag_ar_downleft[m]);
      boxit.d[m](TEX(_diag_ar_downleft[m]));
      d[m].c = A + 2*4bp*unitvector(direction 1/2 length(p) of p rotated (180+45));
      drawunboxed(d[m]);
    fi;
  endfor;
  endgroup;
enddef;

beginfig(1)
begindiag;
	node "{\bf TopMap}";
		rarrowto(2,0, "above" => "c");
		rarrowto(1,1, "belowleft" => "ffff");
	node "";
	node "{\bf CMap}";
		rarrowto(-1, 1, "below" => "ggg");
	nextline;

	node "";
	node "{\bf Graph}";
	node "";
	nextline;

	node "{\bf TopMap}$_2$";
		rarrowto(2,0, "above" => "c");
		rarrowto(0, -2, "above" => "c", "shape" => "inj");
		% rarrowto(1,-1, "belowleft" => "ffff");
		rarrowto(1,1, "belowleft" => "ffff");
	node "";
	node "{\bf CMap}$_2$";
		rarrowto(0, -2, "above" => "c", "shape" => "inj");
		rarrowto(-1, 1, "below" => "ggg");
	nextline;

	node "";
	node "{\bf Graph}$_2$";
	node "";
	nextline;

	node "{\bf TopHMap}";
		%rarrowto(1,0, "above" => "c", "width" => 1bp, "shape" => "inj");
		rarrowto(0, -2, "above" => "c");
	node "";
	node "{\bf CHMap}";
		rarrowto(-2, 0, "above" => "c");
		rarrowto(0, -2, "above" => "c");
enddiag;
endfig;

beginfig(2)
begindiag;
	node "$\mathcal{T} \times \Gamma$";
		rarrowto(1,0, "above" => "$\nu$");
		rarrowto(0,1, "below" => "$F \times \theta$");
	node "$\mathcal{T}$";
		rarrowto(0,1, "below" => "$F$");
	nextline;

	node "$\Omega \times G$";
		rarrowto(1,0, "above" => "$\eta$");
	node "$\Omega$";
enddiag;
endfig;

end

  begindiag;
    node "A";
      rarrowto(1,0, "above" => "a",
               "shape" => "middle",
               "curved" => 3mm,
               "dashed" => withsmalldots);
      rarrowto(0,1, "below" => "b",
               "color" => blue,
               "shape" => "mapsto",
               "dashed" => evenly);
    node "A";
      rarrowto(1,0, "above" => "c", "width" => 1bp, "shape" => "inj");
      rarrowto(0,1, "below" => "d", "shape" => "mono");
    node "A";
    nextline;
    node "A";
      rarrowto(1,0, "below" => "e", "shape" => "epi");
    node "A";
      rarrowto(1,-1, "below" => "$f$", "curved" => -3mm, "shape" => "half_dotted");
	node "B";
  enddiag;

