---
ID: 1390
post_title: Squares and Graphs
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/05/03/squares-and-graphs/
published: true
post_date: 2018-05-03 17:06:22
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />Recently I came across this intriguing little puzzle...
<h3 style="text-align: left;"><strong><em>Take the integers 1..n and, if possible, arrange them in a list in such a way that consecutive numbers in the list add up to a square number. Use each number 1..n once only.</em></strong></h3>
In exploring this puzzle I started writing down the numbers and forming a graph where two numbers are connected if they add up to a square. Drawing such graphs is fun but slightly tedious for, for example, [1..300]...  Here's such a graph for numbers 1..15. Starting at 8 then 1 etc. and following the path gives the solution for 1..15.

<img class="wp-image-1393 aligncenter" src="http://gitcommit.co.uk/wp-content/uploads/2018/05/g15-300x186.png" alt="" width="1075" height="666" />

I became sidetracked into writing a bit of Haskell to generate such graphs and and then create an image from it  :)

In this post we'll
<ol>
 	<li>look at one way of capturing the essence of what a graph is - without any formal development or assertions.</li>
 	<li>Then add a few utility functions to create and manipulate such a graph and finally.</li>
 	<li>Use some Haskell bindings to graphviz to create images of the graphs.</li>
</ol>
<h3>Graph Types</h3>
A graph is just a collection of vertices of a given type and a set of edges representing connections between vertices. There are various ways to represent this idea, here is one simplistic option.
<pre class="lang:haskell decode:true ">--
newtype V a     = V [a]        deriving Show
newtype E a     = E [(a, a)]   deriving Show
newtype Graph a = G (V a ,E a) deriving Show
</pre>
For a <em>Graph</em> holding things of type <em>a</em> we have a list of <em>a</em> representing the vertices -<em> V [a].</em>
An edge between two <em>a</em> s  is shown as a tuple and all edges is a list of such tuples - <em>E [(a, a)].</em>
Finally a <em>Graph</em> of <em>a</em>  is <em>G (V a, E a)</em>.
<h3>Some Functions</h3>
If we start with a list of vertices - for this problem we have 1..n but we can keep it quite general - some vertices will be connected to other vertices, some vertices may have many connections and some may have none. In our puzzle the criteria is that the two vertices (integers) add up to a square. Two numbers adding to give a square is a very specific function on integers. We can generalise it to a function that takes two vertices to give a boolean if the vertices should be connected.
i.e.  <span class="lang:haskell decode:true crayon-inline">(a -&gt; a -&gt; Bool)</span>

So, for a given list of vertices we can generate a graph by applying the function to each possible pair like this:
<pre class="lang:haskell decode:true">--
buildGraph :: (Eq a) =&gt; [a] -&gt; (a -&gt; a -&gt; Bool) -&gt; Graph a
buildGraph vs ed = G (V vs, E es) where
    es = nubBy sameEdge . join . makeEdges $ vs
    makeEdges  = map f where
        f n = foldr (\v a -&gt; if ed n v then (n, v) : a else a) [] vs

sameEdge ::(Eq a) =&gt; (a, a) -&gt;  (a, a) -&gt; Bool
sameEdge (a, b) (c, d) = a == d &amp;&amp; b == c || a == c &amp;&amp; b == d
</pre>
Here we are mapping a function f over the vertices and f is a fold over the vertices of the '<em>am I an edge function</em>' - <span class="lang:haskell decode:true crayon-inline ">(a -&gt; a -&gt; Bool)</span>

The <em>join</em> function is needed as <em>makeEdges</em> creates a list of lists and <em>nubBy sameEdge</em>  is used to remove duplicate edges.

We can use this general function for our puzzle by creating a suitable '<em>am I an edge function</em>' i.e.
<pre class="lang:haskell decode:true">--
sqEdge :: (Integral a, Num a) =&gt; a -&gt; a -&gt; Bool
sqEdge n v =  n /= v &amp;&amp; isSquare (n + v)

isSquare :: Integral a =&gt; a -&gt; Bool
isSquare n = sq * sq == n where sq = floor $ sqrt (fromIntegral n :: Double)
</pre>
To generate a graph where connected vertices add up to a square we just call the <em>buildGraph </em>with a list of vertices and the <em>sqEdge</em> function.
<pre class="lang:haskell decode:true">--
λ-&gt; buildGraph [1..15] sqEdge
G (V [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15],
E [(1,3),(1,8),(1,15),(2,7),(2,14),(3,6),(3,13),
(4,5),(4,12),(5,11),(6,10),(7,9),(10,15),(11,14),(12,13)])</pre>
<pre class="lang:haskell decode:true ">--
λ-&gt; buildGraph [1..50] sqEdge
G (V [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50],
E [(1,3),(1,8),(1,15),(1,24),(1,35),(1,48),(2,7),(2,14),(2,23),(2,34),(2,47),(3,6),(3,13),
(3,22),(3,33),(3,46),(4,5),(4,12),(4,21),(4,32),(4,45),(5,11),(5,20),(5,31),(5,44),(6,10),
(6,19),(6,30),(6,43),(7,9),(7,18),(7,29),(7,42),(8,17),(8,28),(8,41),(9,16),(9,27),(9,40),
(10,15),(10,26),(10,39),(11,14),(11,25),(11,38),(12,13),(12,24),(12,37),(13,23),(13,36),
(14,22),(14,35),(14,50),(15,21),(15,34),(15,49),(16,20),(16,33),(16,48),(17,19),(17,32),
(17,47),(18,31),(18,46),(19,30),(19,45),(20,29),(20,44),(21,28),(21,43),(22,27),(22,42),
(23,26),(23,41),(24,25),(24,40),(25,39),(26,38),(27,37),(28,36),(29,35),(30,34),(31,33),
(31,50),(32,49),(33,48),(34,47),(35,46),(36,45),(37,44),(38,43),(39,42),(40,41)])</pre>
<h3></h3>
<h3>Drawing with Graphviz</h3>
Having some previous familiarity with <a href="http://www.graphviz.org/">Graphviz</a> I found a <a href="http://hackage.haskell.org/package/graphviz-2999.19.0.0/docs/Data-GraphViz.html">Haskell library</a> with bindings for Graphviz. Of course you will need to install Graphviz but the Haskell side is covered by the stack and cabal files in the Github repo for this post.  In outline, the pipeline to get an image (.png) of a graph is to
<ol>
 	<li>Create the graph... we've covered that.</li>
 	<li>Modify the structure of the graph so that Graphviz can process it. This involves adding 'labels' to the edges and vertices.</li>
 	<li>Use the Haskell Graphviz library to create an intermediate Graphviz dot file.</li>
 	<li>Use the Graphviz command line to create a png from the dot file.</li>
 	<li>Look at the pictures :)</li>
</ol>
<strong>Modify the structure. </strong> Perhaps in retrospect I could/should have defined<em> Vs</em> and <em>Es</em> as a pair and a triple respectively as this is what Grapviz needs. The extra item is a label for the vertex or edge - so we need to add an extra value to the vertices and edges. Here's a labelling function to do just that.
<pre class="lang:haskell decode:true ">--
graphLabeller :: (a -&gt; b) -&gt; (a -&gt; a -&gt; c) -&gt; Graph a -&gt; ([(a,b)], [(a,a,c)])
graphLabeller vf ef (G (V vs, E es)) = (vs', es') where
    vs' = map (\ v -&gt; (v, vf v)) vs
    es' = map (\ (x, y) -&gt; (x, y, ef x y)) es
</pre>
It takes a vertex function, <em>vf</em> which defines how to label a vertex. Similarly an edge function, <em>ef</em>,  that creates a label from the two vertices of this edge. The function works by mapping these two functions over the edges and vertices.

<strong>Graphviz dot file. </strong>Creation of a dot file and the invocation of the Graphviz  command line is done in <em>makeImage</em>
<pre class="lang:haskell decode:true">--
graphParams ::(Show a) =&gt; G.GraphvizParams a a a () a
graphParams = G.defaultParams {
  G.fmtNode = const [colorAttribute $ G.RGB 0 0 0],
  G.fmtEdge = \(t, f, l) -&gt; [G.textLabel (TL.pack $ show l), G.arrowTo G.noArrow, colorAttribute (G.RGB 200 0 0)]}
  where
    colorAttribute color = G.Color $ G.toColorList [ color ]

makeImage :: FilePath -&gt; ([(Int, Int)], [(Int, Int, Int)]) -&gt; IO ()
makeImage name (vs, es) = do
    let dotGraph = G.graphElemsToDot graphParams vs es :: G.DotGraph Int
    let dotText  = G.printDotGraph dotGraph :: TL.Text
    TL.writeFile (name &lt;&gt; ".dot") dotText
    callCommand  ("dot " &lt;&gt; name &lt;&gt; ".dot -Tpng &gt; " &lt;&gt; name &lt;&gt; ".png") &gt;&gt;= print
</pre>
and the <em>graphParams</em> function gives basic details on how to format the display of a vertex - <em>G.fmtNode</em> -  and an edge - <em>G.fmtEdge. </em>Pulling all this together into one utility function gives
<pre class="lang:haskell decode:true ">--
graphToImage :: Int -&gt; IO ()
graphToImage  n = do
    let g = buildGraph [1..n] sqEdge
    let (vs, es) = graphLabeller id (+) g
    makeImage ("../images/1_" &lt;&gt; show n) (vs, es)
</pre>
And some sample outputs:

<strong><em>graphToImage 15</em></strong>

<img class="alignnone wp-image-1399" src="http://gitcommit.co.uk/wp-content/uploads/2018/05/1_15-300x295.png" alt="" width="1111" height="1092" />

&nbsp;

&nbsp;

<strong><em>graphToImage 30</em></strong>

<img class="alignnone wp-image-1400" src="http://gitcommit.co.uk/wp-content/uploads/2018/05/1_30-300x267.png" alt="" width="1119" height="995" />

&nbsp;

<strong><em>graphToImage 50</em></strong>

<img class="alignnone wp-image-1404" src="http://gitcommit.co.uk/wp-content/uploads/2018/05/1_50-300x130.png" alt="" width="1230" height="533" />

&nbsp;

&nbsp;

and the above can be sequenced like this <span class="lang:haskell decode:true crayon-inline ">graphToImage 15 &gt;&gt; graphToImage 30 &gt;&gt; graphToImage 50</span>

Really, just an interesting diversion into playing with graphs and then rendering them!

Of course we can quite easily use a different '<em>am I an edge function</em>' to create other 'Integer graphs'. For example <span class="lang:haskell decode:true crayon-inline ">buildGraph [1..20 ] (\ x y -&gt; x `mod` y == 0)</span>

produces this graph

<img class="alignnone wp-image-1409" src="http://gitcommit.co.uk/wp-content/uploads/2018/05/mod_0-300x125.png" alt="" width="1823" height="760" />

&nbsp;

Having touched on graphs and rendering them the next post will either start looking at how to solve the original problem or will perhaps explore and refine the graph type and further investigate rendering.

Anyway, all the code is here in <a href="https://github.com/banditpig/SquareSum">Github</a>.

Thanks for reading!

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;