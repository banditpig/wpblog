---
ID: 907
post_title: >
  Dual Numbers and Differentiation. Part
  3.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/07/05/dual-numbers-and-differentiation-part-3/
published: true
post_date: 2017-07-05 17:31:16
---
[latexpage]
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />Previously we looked at using Dual numbers get the value of the first derivative of a function. As useful as this is there is more potential if we can also obtain the second derivative. My initial, naive, approach to this was to extend the idea of a Dual to that of a Triple like this. <span class="lang:haskell decode:true crayon-inline ">data Triple a = T a a a deriving (Show)</span>. Creating Triple somehow seemed 'wrong', or if not wrong then certainly clumsy as can be seen in some of the code below.
<pre class="lang:haskell decode:true ">data Triple a = T a a a deriving (Show)

instance Fractional a =&gt; Fractional (Triple a ) where
  fromRational n = T (fromRational n) 0 0
  (T g g' g'') / (T h h' h'') = T (g / h) ((g * h' - h * g')/ h * h) secDiff where
    secDiff = ( 2*h'*(g*h' - h*g') - h*(g*h'' - h*g'')) / (h * h * h)
</pre>
Note how messy the code is! It's the result of apply the quotient rule to the result of applying the quotient rule. At the very least it told me that I didn't want to have to take this approach and have to write
<span class="lang:haskell decode:true crayon-inline ">instance (Fractional a, Floating a) =&gt; Floating (Triple a a a)</span> !

The approach taken in '<a href="https://pdfs.semanticscholar.org/4edf/d071cf5012aaa69449c9fe76646955a8d185.pdf">Functional Differentiation of Computer Programs</a>' involves a recursive definition and what are eloquently termed 'infinite towers of derivatives' which exploits the lazy nature of Haskell to give not just first and second derivates but a potentially infinite list of increasingly higher order derivatives. Of course they computation is not 'free' but with lazy Haskell you only pay for what you take and you pay when you take it. Here's the rather innocuous looking start point.
<pre class="lang:haskell decode:true ">data  Diff a = Diff a (Diff a)</pre>
I must admit that I found parts of the paper quite heavy and it took some effort to grasp it but the key thing to remember is that we are now dealing with a recursive data type and so the functions to express the derivatives will need to reflect this recursive structure. So, in the same way that we created instances of <em>Num</em>, <em>Fractional</em> and <em>Floating</em> for <em>Dual</em> we will do the same for <em>Diff</em>. Here's one I made earlier!
<pre class="lang:haskell decode:true">instance Num n =&gt; Num (Diff n) where
    -- # MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #
    (Diff x dx) + (Diff y dy) = Diff (x + y) (dx + dy)
    (Diff x dx) - (Diff y dy) = Diff (x - y) (dx - dy)
    -- d (u*v) = u dv + v du
    x@(Diff x1 dx1) * y@(Diff y1 dy1) = Diff (x1 * y1) (x * dy1 + y * dx1)
    abs (Diff x dx) = Diff (abs x) (abs dx)
    signum (Diff x dx) = Diff (signum x) 0
    fromInteger x = Diff (fromInteger x) 0

instance (Fractional n) =&gt; Fractional (Diff n) where
  -- # MINIMAL fromRational, (recip | (/)) 
  fromRational n = Diff (fromRational n) 0
  x@(Diff x1 dx1) / y@(Diff y1 dy1) = Diff (x1 / y1) ((y * dx1  - x * dy1)/ y^2)



-- helper from the paper
dlift :: Num a =&gt; [a -&gt; a] -&gt; Diff a -&gt; Diff a
dlift (f : f') p@(Diff x x') = Diff (f x) (x' * dlift f' p)

instance (Fractional a, Floating a) =&gt; Floating (Diff a) where
  pi                  = Diff pi 0
  exp (Diff x dx)     = res where res = Diff (exp x) (dx * res)
  log d@(Diff x dx)   = Diff (log x) (dx / d)
  sin (Diff x dx)     = dlift (cycle [sin, cos, negate . sin, negate . cos]) (Diff x dx)
  cos (Diff x dx)     = dlift (cycle [cos, negate . sin, negate . cos, sin]) (Diff x dx)
  asin d@(Diff x dx)  = Diff (asin x) ( dx / sqrt(1 - d*d))
  acos d@(Diff x dx)  = Diff (acos x) (-dx / sqrt(1 - d*d))
  atan d@(Diff x dx)  = Diff (atan x) ( dx / (d*d  - 1))
  sinh d@(Diff x dx)  = (exp d - exp (-d)) / 2
  cosh d@(Diff x dx)  = (exp d + exp (-d)) / 2
  asinh d@(Diff x dx) = Diff (asinh x) (dx / (sqrt(1 + d*d )))
  acosh d@(Diff x dx) = Diff (acosh x) (dx / (sqrt(d*d  - 1)))
  atanh d@(Diff x dx) = Diff (atanh x) (dx / (1 - d*d))


</pre>
In each case we've just implemented the required functions along with a helper function, <em>dlift</em>.
Looking at the  <em>cycle</em> function first we see, by example.
<pre class="lang:haskell decode:true ">λ-&gt; :i cycle
cycle :: [a] -&gt; [a] 	-- Defined in ‘GHC.List’
*Main Data.List
λ-&gt; take 1 $ cycle [1,2,3]
[1]
*Main Data.List
λ-&gt; take 2 $ cycle [1,2,3]
[1,2]
*Main Data.List
λ-&gt; take 10 $ cycle [1,2,3]
[1,2,3,1,2,3,1,2,3,1]</pre>
So seeing how cycle works and knowing that the differential of <em>sin</em> is <em>cos</em> and of <em>cos</em> is <em>-sin</em> then we can see that, for example, <span class="lang:haskell decode:true crayon-inline ">cycle [sin, cos, negate . sin, negate . cos]</span> keeps providing the repeated differential of <em>sin</em>. With this intuition we can see that the <em>dlift</em> helper function takes each, in turn, from <em>cycle</em> and constructs a <em>Diff</em> with that and a recursive call to itself.

If, for the moment, we restrict ourselves to at most the second derivative then a simple implementation of <em>show</em> need only go two levels deep. Like this
<pre class="lang:haskell decode:true ">instance Show a =&gt; Show (Diff a) where
  show (Diff x (Diff x' (Diff x'' _))) = 
    show ("f=" ++ (show x) ++ ", f'=" ++ (show x') ++ ", f''=" ++ (show x''))
</pre>
And armed with this <em>show</em> function we can do some simple tests to check we get the expected values for f, f' and f''.
Let's try $1/x^2$ at x=2
<pre class="lang:haskell decode:true ">λ-&gt; g = \x -&gt; 1/x^2
*Main
λ-&gt; g (Diff 2 1)
"f=0.25, f'=-0.25, f''=0.375"</pre>
That looks good. How about $x^3$ at $x=3.5$?
<pre class="lang:haskell decode:true">λ-&gt; g = \x -&gt; x^3
*Main
λ-&gt; g (Diff 3.5 1)
"f=42.875, f'=36.75, f''=21.0"</pre>
The next batch of examples are all re-runs of the functions shown in the first post but with, of course, the second derivative being calculated.
<pre class="lang:haskell decode:true ">λ-&gt; f = \x -&gt; (x^2 - 3*x) /(x^3 + 4*x^2 - 10 * x + 1)
*Main
λ-&gt; f (Diff 2 1)
"f=-0.4, f'=1.64, f''=-9.808"
*Main
λ-&gt; f = \x -&gt; pi**x + sin(x) + x * exp(x^2)
*Main
λ-&gt; f (Diff 2.1 1)
"f=184.69569597379345, f'=820.0495684271507, f''=4097.82379889742"</pre>
Rather than rely on a show method that's limited in its depth we can write
<pre class="lang:haskell decode:true ">splitN :: Int -&gt; Diff a -&gt; [a]
splitN 0 (Diff x _) = [x] 
splitN n diffx = x : splitN (n - 1) diffx' where
    (x, diffx')  = split diffx
    split (Diff x diffs) = (x, diffs)</pre>
which can be used to get up to the <em>nth</em> derivative. As we are focussing on going to just the second derivative we can then first write
<pre class="lang:haskell decode:true ">diff :: Num a =&gt; Int -&gt; (Diff a -&gt; Diff a) -&gt; a -&gt; [a]
diff n f x = splitN n (f ( Diff x 1)) where
  splitN :: Int -&gt; Diff a -&gt; [a]
  splitN 0 (Diff x _) = [x] 
  splitN n diffx = x : splitN (n - 1) diffx' where
    (x, diffx')  = split diffx
    split (Diff x diffs) = (x, diffs)</pre>
and then create
<pre class="lang:haskell decode:true ">diff2 :: Num a =&gt; (Diff a -&gt; Diff a) -&gt; a -&gt; [a]
diff2 = diff 2</pre>
which will allow us to get the value along with the first and second derivatives using a clearly named function.
e.g.
<pre class="lang:haskell decode:true">λ-&gt;  f = \x -&gt; (exp 1)**(x/2) * sin(4*x)
*Main
λ-&gt; diff2 f 2
[2.689354543632441,-0.23736311995230297,-43.939374453979475]</pre>
I think that's enough for the moment. In the next post we'll look at calculating second order partial derivatives and maybe an application of them. As usual all the code is in <a href="https://github.com/banditpig/autodiff">Github</a>.

Thanks for reading!