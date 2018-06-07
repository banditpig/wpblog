---
ID: 1474
post_title: Ford and his Circles.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/06/05/ford-and-his-circles/
published: true
post_date: 2018-06-05 17:46:01
---
<img class="alignleft size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />[latexpage]

&nbsp;

&nbsp;
<p style="text-align: left;">A <a href="https://en.wikipedia.org/wiki/Ford_circle">Ford circle</a>  is a circle derived from a pair of numbers that are co-prime, i.e. they have no common factors. For  a pair of co-prime integers p and q the Ford circle has radius r and centre at a point</p>
<p style="text-align: left;">P(x, y) where
\begin{align*}
r=\frac{1}{2q^2}\\
P=(\frac{p}{q}\ , r)\\
\end{align*}</p>
No matter what co-prime numbers, p and q, are used to create Ford circles the circles never intersect and they are all tangential to the horizontal  axis.

Now, we could generate 'random' Ford circles by picking any old co-prime pair (p, q). However, the Farey series is a 'ready made' set of such integers - all the Farey fractions are in reduced form and so the numerator and denominator are co-prime. For example, using the code from the <a href="http://gitcommit.co.uk/2018/05/26/count-the-fractions/">previous post</a>,
<pre class="lang:haskell decode:true">--
λ-&gt; farey 10
[0/1,1/10,1/9,1/8,1/7,1/6,1/5,2/9,1/4,2/7,3/10,1/3,3/8,2/5,3/7,4/9,1/2,
5/9,4/7,3/5,5/8,2/3,7/10,5/7,3/4,7/9,4/5,5/6,6/7,7/8,8/9,9/10,1/1]</pre>
To create Ford circles from these fractions we first define a suitable type and then create a few helper functions i.e.
<pre class="lang:haskell decode:true ">--
module FordCircles where
import           Farey
import           Fractions

type FordCircle = (Float, Float, Float)

-- for Farey of order n
fordCircles :: Integer -&gt; [FordCircle]
fordCircles = fmap fordCircle . farey

fordCircle :: Fraction -&gt; FordCircle
fordCircle  (F p q) = (r, fromIntegral p / fromIntegral q, r ) where
    r = 1/fromIntegral (2*q*q)


scaleFordCircle :: Float -&gt;  FordCircle -&gt; FordCircle
scaleFordCircle s (x, y, z) = (s * x, s * y, s * z)
</pre>
Here we're using the Farey sequence code from module <em>Farey</em> and mapping <em>fordCircle</em> over a Farey sequence. The function <em>fordCircle</em> is just the Haskell form of the equations described above.  However, generating a list of <em>FordCircle</em> gives data that is not particularly informative. e.g.
<pre class="lang:haskell decode:true">--
λ-&gt; fordCircles 10
[(0.5,0.0,0.5),(5.0e-3,0.1,5.0e-3),(6.1728396e-3,0.11111111,6.1728396e-3),(7.8125e-3,0.125,7.8125e-3),(1.0204081e-2,0.14285715,1.0204081e-2),(1.3888889e-2,0.16666667,1.3888889e-2),(2.0e-2,0.2,2.0e-2),(6.1728396e-3,0.22222222,6.1728396e-3),(3.125e-2,0.25,3.125e-2),(1.0204081e-2,0.2857143,1.0204081e-2),(5.0e-3,0.3,5.0e-3),(5.5555556e-2,0.33333334,5.5555556e-2),(7.8125e-3,0.375,7.8125e-3),(2.0e-2,0.4,2.0e-2),(1.0204081e-2,0.42857143,1.0204081e-2),(6.1728396e-3,0.44444445,6.1728396e-3),(0.125,0.5,0.125),(6.1728396e-3,0.5555556,6.1728396e-3),(1.0204081e-2,0.5714286,1.0204081e-2),(2.0e-2,0.6,2.0e-2),(7.8125e-3,0.625,7.8125e-3),(5.5555556e-2,0.6666667,5.5555556e-2),(5.0e-3,0.7,5.0e-3),(1.0204081e-2,0.71428573,1.0204081e-2),(3.125e-2,0.75,3.125e-2),(6.1728396e-3,0.7777778,6.1728396e-3),(2.0e-2,0.8,2.0e-2),(1.3888889e-2,0.8333333,1.3888889e-2),(1.0204081e-2,0.85714287,1.0204081e-2),(7.8125e-3,0.875,7.8125e-3),(6.1728396e-3,0.8888889,6.1728396e-3),(5.0e-3,0.9,5.0e-3),(0.5,1.0,0.5)]</pre>
But, using a little bit of <a href="https://hackage.haskell.org/package/gloss">Gloss</a>...
<pre class="lang:haskell decode:true">--

import           Farey
import           FordCircles
import           Graphics.Gloss


circFull :: FordCircle -&gt; Picture
circFull (r, x, y) =  translate x y $ color white $ circleSolid r

circFrame :: FordCircle -&gt; Picture
circFrame (r, x, y) =  translate x y $ circle r


makeCircles :: Picture
makeCircles = Pictures $ fmap (circFull . scaleFordCircle 500) (fordCircles 50)

makeCircles' :: Picture
makeCircles' = Pictures $ fmap (circFrame . scaleFordCircle 500) (fordCircles 50)


main :: IO ()
main =   display
         FullScreen
         black $
         Pictures [makeCircles, translate 500 0 $ rotate 180 makeCircles]
</pre>
we can generate an attractive interpretation of the data...

Notice how the circles don't intersect, are tangential to immediate neighbours and to the x axis.

<img class="size-full wp-image-1511 alignleft" src="http://gitcommit.co.uk/wp-content/uploads/2018/06/Screen-Shot-2018-06-05-at-16.24.17.png" alt="" width="2880" height="1800" />

<hr />

and with a bit of manipulation...

<img class="size-full wp-image-1513 alignleft" src="http://gitcommit.co.uk/wp-content/uploads/2018/06/Screen-Shot-2018-06-05-at-16.27.02.png" alt="" width="2880" height="1800" />

<hr />

&nbsp;

<img class="size-full wp-image-1515 alignleft" src="http://gitcommit.co.uk/wp-content/uploads/2018/06/Screen-Shot-2018-06-05-at-16.29.00.png" alt="" width="2880" height="1800" />

<hr />

With high contrast...

<img class="size-full wp-image-1509 alignleft" src="http://gitcommit.co.uk/wp-content/uploads/2018/06/Screen-Shot-2018-06-05-at-16.14.04-1.png" alt="" width="2880" height="1800" />

<hr />

<img class="size-full wp-image-1507 alignleft" src="http://gitcommit.co.uk/wp-content/uploads/2018/06/Screen-Shot-2018-06-05-at-16.17.17.png" alt="" width="2880" height="1800" />

<hr />

<img class="size-full wp-image-1506 alignleft" src="http://gitcommit.co.uk/wp-content/uploads/2018/06/Screen-Shot-2018-06-05-at-16.17.23.png" alt="" width="2880" height="1800" />

<hr />

<img class="size-full wp-image-1521 alignleft" src="http://gitcommit.co.uk/wp-content/uploads/2018/06/Screen-Shot-2018-06-05-at-18.42.27.png" alt="" width="2880" height="1800" />

Quite pleasing images for little effort :)

All the code is on <a href="https://github.com/banditpig/Farey/tree/monoidFractions">Github</a> and thanks for reading!

&nbsp;