---
ID: 1161
post_title: Fractions to Phi to Fibonacci!
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/11/16/fractions-to-phi-to-fibonacci/
published: true
post_date: 2017-11-16 19:55:53
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />Oh no, not another Haskell way of calculating Fibonacci numbers! Well, yes but done perhaps slightly differently.
This post brings together
<ul>
 	<li><a href="https://en.wikipedia.org/wiki/Golden_ratio">The Golden Ratio</a></li>
 	<li><a href="https://en.wikipedia.org/wiki/Fibonacci_number">Fibonacci Numbers</a></li>
 	<li><a href="https://en.wikipedia.org/wiki/Continued_fraction">Continued Fractions</a></li>
</ul>
<h2><strong>The Golden Ratio (Phi)</strong></h2>
<p style="text-align: center;"><em>     "...two quantities are in the <b>golden ratio</b> if their ratio is the same as the ratio of their sum  to the larger of the two quantities."</em></p>
This ratio appears often mathematics and in nature, perhaps almost as pervasive as pi. And there is the Golden Rectangle, a 2-D extension of the Golden Ratio, often used in art because of its intrinsically appealing properties.
<h2><strong>Fibonacci Numbers</strong></h2>
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144,... 'Nuff said.
<h2><strong>Continued Fractions</strong></h2>
Are like this, fractions where the denominator is also a fraction and so on...

<img class="alignnone size-full wp-image-1191" src="http://gitcommit.co.uk/wp-content/uploads/2017/11/cfraction-e1510851791880.png" alt="" width="211" height="127" />

The idea in this post is to create a Haskell representation of fractions but in particular have that representation handle continued fractions. Once we can do that then we can create a continued fraction for the Golden Ratio, which is this:

<img class="alignnone size-medium wp-image-1171" src="http://gitcommit.co.uk/wp-content/uploads/2017/11/phi-300x177.gif" alt="" width="300" height="177" />

and then, if the continued fraction above is 'flattened' into a list of intermediate fractions, the denominators of those intermediate fractions are the terms of the Fibonacci series! Which I think is quite stunning.

For details see section 11 in this link: <a href="http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/cfINTRO.html#section11">"<strong><em>The Convergents of Phi's CF are Ratios of Fibonacci Numbers!</em></strong>" </a>

&nbsp;
<h2><strong>A Little Bit of Haskell</strong></h2>
A fraction is a numerator 'over' a denominator and we'll also consider a whole number to be a fraction with an implicit denominator of 1.
Furthermore, if we are to have continued fractions then the denominator must, optionally, be a fraction too. Here's a couple of type synonyms and an algebraic data type to express these ideas.
<pre class="lang:haskell decode:true">--
type Numerator   = Integer
type Denominator = Integer

data Fraction  = Numbr Numerator | F Numerator Fraction</pre>
This will allow us to create a simple fraction like, for example, <em>Numbr 4</em> which is 4/1 or using the F type constructor <em>F 7 (Numbr 8).</em> A continued fraction could be something like <em>F 7 (F 6 (F 11 (Numbr 23)))</em> which could be have been continued indefinitely, which is quite cool!

Next we need a sensible way of showing fractions - we do this by making Fraction an instance of the Show type class like this
<pre class="lang:haskell decode:true ">--
instance Show Fraction where
    show (Numbr n) =  show n ++ "/1"
    show (F n  0) = show n ++ "/0"
    show (F n (Numbr d))
        | n == 0 = show 0
        | d == 1 = show n ++ "/1"
        | (n &gt; 0 &amp;&amp; d &lt; 0) || (n &lt; 0 &amp;&amp; d &lt; 0 ) = show (-n) ++ "/" ++ show (-d)
        | otherwise = show n ++ "/" ++ show d
    show (F n f@(F _ _)) = show n ++  "/" ++  show f
</pre>
This is fairly simple pattern matching on the <em>show</em> function. The final pattern is slightly more complex as it handles the condition where a fraction may have another fraction as its denominator and so it calls <em>show</em> again recursively.

If we make Fraction an instance of the <em>Num</em> typeclass then we can, with suitable definitions,  use  operators  +, -, *  on Fractions. And, if we make Fraction an instance of Haskell's <em>Fractional</em> typeclass we can use the division, /, operator.  I think the code is fairly self explanatory so I'll add it all here and then describe some of the more important parts.
<pre class="lang:haskell decode:true">--
--
module Fractions where
import           Data.Ratio

type Numerator = Integer
type Denominator = Integer
{-
So a fraction can be a simple
3 or 3/4 or 3 / (4 / (...)) a recursive, continued, fraction:
-}
data Fraction  = Numbr Numerator | F Numerator Fraction

instance Show Fraction where
    show (Numbr n) =  show n ++ "/1"
    show (F n  0) = show n ++ "/0"
    show (F n (Numbr d))
        | n == 0 = show 0
        | d == 1 = show n ++ "/1"
        | (n &gt; 0 &amp;&amp; d &lt; 0) || (n &lt; 0 &amp;&amp; d &lt; 0 ) = show (-n) ++ "/" ++ show (-d)
        | otherwise = show n ++ "/" ++ show d
    show (F n f@(F _ _)) = show n ++  "/" ++  show f

-- Make Fraction into a Number so that +, * etc can be used.
instance Num Fraction where
    (+) = add
    (*) = mul
    (-) = sub
    abs (Numbr n) = Numbr (abs n)
    abs f@(F n d)   = F  (abs n1) (abs d1) where F n1 d1 = simplify f
    signum (Numbr n) = Numbr (signum n)
    signum f@(F n d)
        | n1 == 0 =  Numbr 0
        | n1 &gt; 0 &amp;&amp; d1 &lt; 0 = Numbr (-1)
        | n1 &lt; 0 &amp;&amp; d1 &gt; 0 = Numbr (-1)
        | otherwise = Numbr 1 where
            F n1 d1 = simplify f
    fromInteger = Numbr

-- Allows the 1/1 operator to be used with Fraction
instance Fractional Fraction where
    fromRational x = F (numerator x) (Numbr (denominator x))
    (/) = divid

-- Notion of equality between Fractions
instance Eq Fraction where
    (==) f g = res where
            fr  = reduce f
            gr  = reduce g
            res = num fr == num gr &amp;&amp; denom fr == denom gr

-- And ordering
instance Ord Fraction where
    (&lt;=) f g = signum (f - g ) == -1

-- Fraction specific definitions of basic arithmetic ops. These are referenced
-- in the previous typeclass definitions.
mul :: Fraction -&gt; Fraction -&gt; Fraction
mul f f1 = mul1 (simplify f) (simplify f1) where
    mul1 (Numbr p) (Numbr q)               = Numbr (p * q)
    mul1 (Numbr p) (F p1 (Numbr q1))       = F (p * p1) (Numbr q1)
    mul1 (F p1 (Numbr q1)) (Numbr p)       = F (p * p1) (Numbr q1)
    mul1 (F p (Numbr q)) (F p1 (Numbr q1)) = F (p * p1) (Numbr (q * q1))

add :: Fraction -&gt; Fraction -&gt; Fraction
add f f1 = add1 (simplify f) (simplify f1) where
    add1 (Numbr p) (Numbr q)               = Numbr (p + q)
    add1 (Numbr p) (F p1 (Numbr q1))       = F (q1 * p + p1) (Numbr q1)
    add1 (F p1 (Numbr q1)) (Numbr p)       = F (q1 * p + p1) (Numbr q1)
    add1 (F p (Numbr q)) (F p1 (Numbr q1)) = F (p * q1 + q * p1) (Numbr (q * q1))

sub :: Fraction -&gt; Fraction -&gt; Fraction
sub f f1 = sub1 (simplify f) (simplify f1) where
    sub1 (Numbr p) (Numbr q)               = Numbr (p - q)
    sub1 (Numbr p) (F p1 (Numbr q1))       = F (q1 * p - p1) (Numbr q1)
    sub1 (F p1 (Numbr q1)) (Numbr p)       = F (q1 * p - p1) (Numbr q1)
    sub1 (F p (Numbr q)) (F p1 (Numbr q1)) = F (p * q1 - q * p1) (Numbr (q * q1))

divid :: Fraction -&gt; Fraction -&gt; Fraction
divid f f1 = divid1 (simplify f) (simplify f1) where
    divid1 (Numbr p) (Numbr q)               = F p (Numbr q)
    divid1 (Numbr p) (F p1 (Numbr q1))       = F (p * q1) (Numbr p1)
    divid1 (F p1 (Numbr q1)) (Numbr p)       = F p1 (Numbr (q1 * p) )
    divid1 (F p (Numbr q)) (F p1 (Numbr q1)) = F (p * q1) (Numbr (q * p1))

-- This works by taking a possibly nested fraction, flattening
-- it into a list and then folds (/) over the list to simplify the
-- fraction
simplify :: Fraction -&gt; Fraction
simplify f = foldr (/) lastFraction remainingFractions where
    flat = flatten f
    lastFraction = last flat
    remainingFractions = takeWhile (/= lastFraction) flat


-- Remove common divisors
-- eg reduce 6/10 -&gt; 3/5, reduce 3/5 -&gt; 3/5
reduce :: Fraction -&gt; Fraction
reduce (Numbr n) = Numbr n
reduce f@(F n (Numbr d))
    | gDiv == 1 = f
    | otherwise = F  (n `div` gDiv) (Numbr (d `div` gDiv)) where
        gDiv = gcd n d
reduce (F n f) = F n (reduce f)

-- Take a possibly recursive fraction and reduce it to a list of fractions
flatten :: Fraction -&gt; [Fraction]
flatten f@(F _ (Numbr _) ) = [f]
flatten (Numbr n)          = [F n (Numbr 1)]
flatten (F n f)            = Numbr n : flatten f


-- Simple pattern matching to get the numerator from a Fraction
num :: Fraction -&gt; Integer
num (Numbr n) = n
num (F n _)   = n

-- and the denominator
denom :: Fraction -&gt; Integer
denom (Numbr _)       = 1
denom (F _ (Numbr n)) = n
denom (F _ f@(F _ _)) = denom f

-- Takes a Fraction, simplifies it by a "flatten and foldr" technique,
-- removes common divisors and finally resolves the fraction as a Float.
evalFrac :: Fraction -&gt; Float
evalFrac f = fromIntegral n / fromIntegral d where
    F n (Numbr d) = reduce . simplify $ f

--           The 'a' coefficients     The 'b' coefficients     Depth
contFrac :: (Integer -&gt; Fraction) -&gt; (Integer -&gt; Numerator) -&gt; Integer -&gt; Fraction
contFrac fa fb  = rf 0  where
      rf n t
        | n &gt; t = 0
        | otherwise =  fa n + F (fb n) (rf (n + 1) t)

root2 :: Integer -&gt; Fraction
root2  = contFrac fa fb where
    fa 0 = 1
    fa _ = 2
    fb _ = 1

root5 :: Integer -&gt; Fraction
root5  = contFrac fa fb  where
                fa 0 = 2
                fa _ = 4
                fb _ = 1
phi :: Integer -&gt; Fraction
phi  = contFrac fa fb  where
                fa _ = 1
                fb _ = 1


</pre>
The functions <em>mul, add, sub </em>and <em>divid</em> are all fairly simple and are really how you would expect multiplication, addition, subtraction and division of fractions to be done and they work on continued fractions by calling <em>simplify</em> before applying the operation.
The function <em>contFrac</em> is the core of what we are doing and it's really quite a simple recursive function. It take two 'generator' functions that supply the <em>a</em> and <em>b</em> values in this image:
<img class="alignnone size-full wp-image-1191" src="http://gitcommit.co.uk/wp-content/uploads/2017/11/cfraction-e1510851791880.png" alt="" width="211" height="127" />

and the depth parameter determines how many times it recurses. So with a pair of suitable generator functions many (maybe all) continued fractions can be generated. For example the square root of 2 is
<img class="alignnone size-full wp-image-1195" src="http://gitcommit.co.uk/wp-content/uploads/2017/11/root2.png" alt="" width="278" height="162" />
and so we have
<pre class="lang:haskell decode:true ">--
--
root2 :: Integer -&gt; Fraction
root2  = contFrac fa fb where
    fa 0 = 1
    fa _ = 2
    fb _ = 1</pre>
Trying this out in GHCi to a depth of 5 we get
<pre class="lang:haskell decode:true">λ-&gt; evalFrac . root2 $ 5
1.4137931</pre>
To a depth of 10 we get
<pre class="lang:haskell decode:true">λ-&gt; evalFrac . root2 $ 10
1.4142137</pre>
We can also get the terms for each value of <em>depth</em> like this
<pre class="lang:haskell decode:true">λ-&gt; roots = [evalFrac . root2 $ n | n &lt;- [0..20]]
*Fractions
λ-&gt; roots
[Infinity,1.0,1.5,1.4,1.4166666,1.4137931,1.4142857,1.4142011,1.4142157,1.4142132,1.4142137,1.4142135,1.4142135,1.4142135,1.4142135,1.4142135,1.4142135,1.4142135,1.4142135,1.4142135,1.4142135]</pre>
Or, in fraction form:
<pre class="lang:haskell decode:true">λ-&gt; roots = [root2 $ n | n &lt;- [0..20]]
*Fractions
λ-&gt; roots
[1/0,1/1,3/2,7/5,17/12,41/29,99/70,239/169,577/408,1393/985,3363/2378,8119/5741,19601/13860,47321/33461,114243/80782,275807/195025,665857/470832,1607521/1136689,3880899/2744210,9369319/6625109,22619537/15994428]</pre>
The continued fraction for phi is really simple - the '<em>a</em>' s and '<em>b</em>' s are all 1.
<pre class="lang:haskell decode:true">phi :: Integer -&gt; Fraction
phi  = contFrac fa fb  where
                fa _ = 1
                fb _ = 1
</pre>
Calculating to a depth of 100 we have
<pre class="lang:haskell decode:true ">--
λ-&gt; evalFrac . phi $ 100
1.6180339</pre>
which is quite accurate!
Getting the terms out we have...
<pre class="lang:haskell decode:true">--
--
λ-&gt; phis = [phi n | n &lt;- [0..30]]
*Fractions
λ-&gt; phis
[1/0,1/1,2/1,3/2,5/3,8/5,13/8,21/13,34/21,55/34,89/55,144/89,233/144,377/233,610/377,987/610,1597/987,2584/1597,4181/2584,
6765/4181,10946/6765,17711/10946,28657/17711,46368/28657,75025/46368,121393/75025,196418/121393,317811/196418,514229/317811,
832040/514229,1346269/832040]</pre>
If we now map <em>denom</em> (or num) over <em>phis</em> we get the terms of the Fibonacci series! :)
<pre class="lang:haskell decode:true">λ-&gt; map denom phis
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040]</pre>
or slightly larger...
<h2>λ-&gt; map denom phis
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040]</h2>
which was the purpose of this post.

&nbsp;
<h2>Concluding Notes</h2>
What I've written here has only touched the surface of continued fractions. I hope that future posts will
<ul>
 	<li>explore an alternative to foldr when resolving a continued fraction.</li>
 	<li>create a parser that will generate continued fractions from standard notation for continued fractions</li>
 	<li>extend the type beyond fractions using integers to fractions with complex numbers</li>
</ul>
This <a href="http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/cfINTRO.html">website</a>   is a mine of wonderfully explained ideas about fractions and is well worth a look. It explains in a clear and detailed way many properties of continued fractions.

You can find this Haskell code in  <a href="https://github.com/banditpig/Fractions">GitHub</a>.

Thanks for reading...!

&nbsp;

&nbsp;