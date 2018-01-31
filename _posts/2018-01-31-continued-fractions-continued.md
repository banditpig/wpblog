---
ID: 1266
post_title: Continued Fractions Continued.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/01/31/continued-fractions-continued/
published: true
post_date: 2018-01-31 20:47:42
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />[latexpage]
It seems to me that continued fractions (CFs) are perhaps too advanced for 'A' levels and too elementary for a degree maths course and are perhaps undervalued or ignored in schools and universities?  Since my <a href="http://gitcommit.co.uk/2017/11/22/parsers-to-fractions-to-square-roots/">last post</a> about fractions I've looked a little more at CFs and  found they have applications ranging from factorising large numbers to gear ratio calculations. And they're really  interesting when their layers are peeled away with a bit of Haskell. So, a bit of playing with numbers and a bit of Haskelling- what's not to like?

Let's start with a fraction, any fraction - say 103/24 and in CF form this is

\begin{align*}
\frac{103}{24} = 4+\cfrac{1}{3+\cfrac{1}{2+\cfrac{1}{3}}}
\end{align*}

and using the  <a href="http://gitcommit.co.uk/2017/11/22/parsers-to-fractions-to-square-roots/">list notation</a> this is [4,3,2,3]
The Haskell code to derive a (finite) CFList is quite simple. (The type CFList is just a synonym for [Integer])
<pre class="lang:haskell decode:true">-- Takes n/d and returns the CF in list form. eg.
--    41/13 = [3, 6, 2]
--    124/37 = [3, 2, 1, 5, 2]
--    5/12 = [0, 2, 2, 2]
toCFList :: Fraction -&gt; CFList
toCFList (F n (Numbr d))
    | d' == 0 = [a]
    | otherwise =  a : toCFList (F d (Numbr d')) where
       (a, d') = divMod n d</pre>
it just builds up the list of terms by repeated application of '<em>divMod</em>' and terminates when the remainder is zero.
Note that the previous post used a list format for CFs but that was from the perspective of CFs that never terminated - e.g. CFs for square roots. The lists here are, at the moment, finite and represent rational numbers.
If we wanted the fractions 2/2, 2/3, 2/4, 2/5 ... 2/20 in list form. Then <span class="lang:haskell decode:true crayon-inline ">map toCFList . map (\n -&gt; F 2 (Numbr n)) $ [2..10]</span> gives
<pre class="lang:haskell decode:true ">--
[1]
[0,1,2]
[0,2]
[0,2,2]
[0,3]
[0,3,2]
[0,4]
[0,4,2]
[0,5]</pre>
which has a very pleasing pattern to it.

Another useful aspect of a CF is the idea of a convergent which we looked at in a previous post when approximating square roots. The convergents are esentailly the 'partial sums' of the CF. And for finite CFs the list of convergents is finite and the final convergent is, of course, the original fraction.
For example
<pre class="lang:haskell decode:true ">convergents (357/167)</pre>
gives
<pre class="lang:haskell decode:true ">[2/1,15/7,47/22,62/29,357/167]</pre>
and each convergent is a better approximation to the original fraction. Initially, to calculate convergents, I wrote this function
<pre class="lang:haskell decode:true ">--
--
convergents :: Fraction -&gt; Convergents
convergents (F n (Numbr d)) = [frac (toInteger d') fracStruc | d' &lt;- [1..(len fracStruc )]] where
    fracStruc = toFracStruc (toCFList (F n (Numbr d)))
    len (FracStruc _ r) = 1 + length r
</pre>
which involved a slightly awkward creation of a FracStruc after creating a CFList representation of the Fraction.
A much simpler and direct way is to calculate the convergents using a recurrence relationship described <a href="https://en.wikipedia.org/wiki/Generalized_continued_fraction#Fundamental_recurrence_formulas">here</a>.
<pre class="lang:haskell decode:true ">--
--
type CFList = [Integer]
type Convergents = [Fraction]

convergents :: CFList -&gt; Convergents
convergents (a0 : a1 : as) = map (\(n, d) -&gt; F n (Numbr d) ) terms
    where
      p0 = a0
      q0 = 1
      p1 = a1 * a0 + 1
      q1 = a1
      terms = (p0, q0) : (p1, q1) : zipWith3 nextConv terms (tail terms) as
      nextConv (pi, qi) (pj, qj) ak = (pk, qk)
          where
            pk = ak * pj + pi
            qk = ak * qj + qi</pre>
Where <em>CFList</em> is just a list of Integer and <em>Convergents</em> is a synonym for a list of <em>Fraction</em>. This shows the elegance of Haskell in the way the <em>terms</em> list is built up recursively with 'two views' of the same list one view '<em>terms</em>' 'out of step' with the second view - '<em>tail terms</em>'. And the <em>zipWith3</em> function pulls it all together using the <em>nextConv</em> function. Finally the list is mapped over to convert (p, q) into a <em>Fraction</em>! :)

We can examine how close each convergent is to the original Fraction with a function like this:
<pre class="lang:haskell decode:true ">--
convergentDeltas :: Convergents -&gt; [(Fraction, Float)]
convergentDeltas  cs = map (\ fr -&gt; (fr, abs f - evalFrac fr)) cs where
    f = evalFrac (last cs)
</pre>
Playing with these in GHCI:
<pre class="lang:haskell decode:true ">λ-&gt; cs = toCFList (34231/32190)
*Fractions
λ-&gt; cs
[1,15,1,3,2,1,1,1,2,1,1,1,1,3]
*Fractions
λ-&gt; convergents cs
[1/1,16/15,17/16,67/63,151/142,218/205,369/347,587/552,1543/1451,2130/2003,3673/3454,5803/5457,9476/8911,34231/32190]</pre>
Notice that the last convergent is the original fraction - 'all the partial sums have been added together'.
We can see the error for each convergent by applying
<pre class="lang:haskell decode:true ">--
λ-&gt; convergentDeltas . convergents $  cs
[(1/1,6.34048e-2),
(16/15,-3.2619238e-3),
(17/16,9.047985e-4),
(67/63,-8.72612e-5),
(151/142,2.4557114e-5),
(218/205,-9.894371e-6),
(369/347,4.172325e-6),
(587/552,-9.536743e-7),
(1543/1451,2.3841858e-7),
(2130/2003,-1.1920929e-7),
(3673/3454,0.0),
(5803/5457,0.0),
(9476/8911,0.0),
(34231/32190,0.0)]
*Fractions</pre>
and notice that the error, whilst decreasing, also alternates either side of the convergents. Also the convergents of a CF give the best rational approximation to that fraction and any better approximating fraction would need a larger denominator than the convergent.

&nbsp;
<h2 style="text-align: center;">A Simple Application</h2>
A practical application of CFs is in calculating gear ratios. Christian Huygens used CFs back in 1700 to calculate the gears needed to <a href="http://www.irem.univ-mrs.fr/IMG/pdf/huygens-delft.pdf">build a planetarium</a>.

Huygens, by observation and calculation, derived the ratio of the orbital periods of the planets  to Earth's  as
<ul>
 	<li>Mercury 25335 / 105190</li>
 	<li>Venus  64725 / 105190</li>
 	<li>Mars 197836 / 105190</li>
 	<li>Jupiter 1247057 / 105190</li>
 	<li>Saturn 3095277 / 105190</li>
</ul>
If we represent these ratios as Fraction in a list we can map over them to get the convergents:
<pre class="lang:haskell decode:true ">--
--
λ-&gt; allFs = [25335/105190,64725/105190, 197836/105190, 1247057/105190, 3095277/105190] :: [Fraction]
*Fractions  map (convergents . toCFList) allFs
[[0,1/4,6/25,7/29,13/54,33/137,46/191,79/328,125/519,204/847,1553/6448,1757/7295,5067/21038],                 -- Mercury
[0,1/1,1/2,2/3,3/5,8/13,675/1097,683/1110,2724/4427,3407/5537,12945/21038],                                   -- Venus
[1/1,2/1,15/8,32/17,47/25,79/42,205/109,1104/587,6829/3631,28420/15111,35249/18742,98918/52595],              -- Mars
[11/1,12/1,71/6,83/7,901/76,8192/691,9093/767,17285/1458,26378/2225,228309/19258,254687/21483,1247057/105190],-- Jupiter
[29/1,59/2,147/5,206/7,1383/47,11270/383,12653/430,74535/2533,310793/10562,696121/23657,3095277/105190]]      -- Saturn
--
</pre>
From these Huygens selected 33/137 for Mercury, 8/13 for Venus pushed up to 32/52. For Mars he used 79/42 doubled to 158/84. Jupiter had the value 83/7 and Saturn 59/2 made into 118/4.
Which all gave quite satisfactory results!
In the next part of this series we'll look at <a href="https://en.wikipedia.org/wiki/Pell%27s_equation">Pells equation</a> and its solutions and in the process obtain a simple way of expressing the square root of an integer as a CF in list form then find better and better approximations by computing the convergents directly from the list.

Thanks for reading!