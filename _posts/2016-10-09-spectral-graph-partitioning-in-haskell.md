---
id: 827
title: Spectral graph partitioning in Haskell
date: 2016-10-09T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2016/10/09/spectral-graph-partitioning-in-haskell/
permalink: /2016/10/09/spectral-graph-partitioning-in-haskell/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
I was reading [some notes from QUT](https://external-apps.qut.edu.au/futurelearn/resources/mm3/graph/graph-simple.html.utf8) about using spectral graph theory to partition a graph using eigenvalues of unnormalised Laplacian. Their Matlab code looks like this: 

<pre>% Form W = Gaussian distribution based on distances
W = zeros(100, 100);
D = zeros(100, 100);

sigma = 2;
N = 100;
for i = 1:N
    for j = 1:N
        if (j ~= i)
            % Calculate the distance between two points
            dist = norm([A(i, 1) - A(j, 1); A(i, 2) - A(j, 2)]);
            expp = exp(-dist^2 / (2 * sigma^2));
            adjacency(i, j) = 1;
            % Add the weights to the matrix
            W(i, j) = expp;
            % Add up the row sum as we go
            D(i, i) = D(i, i) + expp;
        end
    end
end

L = D - W;

Find the eigenpairs of L

[vec, val] = eig(L, 'vector');
% Ensure the eigenvalues and eigenvectors are sorted in ascending order
[val,ind] = sort(val);
vec = vec(:, ind);

% Plot with clusters identified by marker
% v2
figure;
hold on;
for i = 1:N
    plot(i, vec(i, 2), ['k' plot_syms{i}]);
end
</pre>

Personally, this gives me nightmares from undergrad numerical methods classes. So here’s how to do it in Haskell. Full source (including cabal config) for this post is [here](https://github.com/carlohamalainen/playground/tree/master/haskell/bigdata).

<pre>&gt; module Notes where
</pre>

To create the W matrix we use buildMatrix: 

<pre>&gt; buildW :: Double -&gt; Matrix Double -&gt; Matrix Double
&gt; buildW sigma a = buildMatrix n n $ (i,j) -&gt; f sigma a i j
&gt;   where
&gt;     n = rows a
&gt;
&gt;     f sigma a i j = if j /= i
&gt;                         then expp sigma a i j
&gt;                         else 0.0
&gt;
&gt;     dist :: Matrix Double -&gt; Int -&gt; Int -&gt; Double
&gt;     dist m i j = norm2 $ fromList [ m!i!0 - m!j!0, m!i!1 - m!j!1 ]
&gt;
&gt;     expp :: Double -&gt; Matrix Double -&gt; Int -&gt; Int -&gt; Double
&gt;     expp sigma m i j = exp $ (-(dist m i j)**2)/(2*sigma**2)
</pre>

The D matrix is a diagonal matrix with each element being the sum of a row of W, which is nicely expressible by composing a few functions: 

<pre>&gt; buildD :: Matrix Double -&gt; Matrix Double
&gt; buildD w = diag
&gt;          . fromList
&gt;          . map (foldVector (+) 0)
&gt;          . toRows
&gt;          $ w
&gt;   where
&gt;     n = rows w
</pre>

The L matrix is real and symmetric so we use [eigSH](https://hackage.haskell.org/package/hmatrix-0.16.1.5/docs/Numeric-LinearAlgebra-HMatrix.html#v:eigSH) which provides the eigenvalues in descending order.

<pre>&gt; lapEigs :: Double -&gt; Matrix Double -&gt; (Vector Double, Matrix Double)
&gt; lapEigs sigma m = eigSH l
&gt;   where
&gt;     w = buildW sigma m
&gt;     d = buildD w
&gt;     l = d - w
</pre>

To finish up, the Fiedler eigenvector corresponds to the second smallest eigenvalue: 

<pre>&gt; fiedler :: Double -&gt; Matrix Double -&gt; (Double, Vector Double)
&gt; fiedler sigma m = (val ! (n-2), vector $ concat $ toLists $ vec ¿ [n-2])
&gt;   where
&gt;     (val, vec) = lapEigs sigma m
&gt;     n = rows m
</pre>

To plot the eigenvalues and eigenvector we use the [Chart](https://hackage.haskell.org/package/Chart-1.8/docs/Graphics-Rendering-Chart-Easy.html) library which uses the [cairo](https://en.wikipedia.org/wiki/Cairo_(graphics)) backend.

<pre>&gt; doPlot "eigenvalues.png" "Eigenvalues" "eigenvalue" $ zip [0..] (reverse $ toList val)
&gt;
&gt; doPlot "fiedler.png"
&gt;        "Second eigenvalue of unnormalised Laplacian"
&gt;        "fiedler eigenvector"
&gt;        (zip [0..] $ toList algConnecEigVec)
</pre>

<img src="https://i0.wp.com/raw.githubusercontent.com/carlohamalainen/playground/master/haskell/bigdata/eigenvalues.png?w=600&#038;ssl=1"  data-recalc-dims="1" /> 

<img src="https://i2.wp.com/raw.githubusercontent.com/carlohamalainen/playground/master/haskell/bigdata/fiedler.png?w=600&#038;ssl=1"  data-recalc-dims="1" />