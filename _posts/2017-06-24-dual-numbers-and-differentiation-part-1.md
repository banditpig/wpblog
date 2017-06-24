---
ID: 661
post_title: >
  Dual Numbers and Differentiation. Part
  1.
author: BanditPig
post_date: 2017-06-24 19:51:58
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/06/24/dual-numbers-and-differentiation-part-1/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />
<h3>[latexpage]
Overview</h3>
Just recently I came across the interesting and, at first viewing, the rather abstract idea of dual numbers. I suppose they are no more or less abstract than other numbers... anyway the idea is similar to that of complex numbers where we have

\begin{align*}
z &amp;= x + iy \\
and\ i^2 &amp;= -1 \\
\end{align*}

Dual numbers are quite similar, we have the dual number d as
\begin{align*}
d &amp;= u + \epsilon u' \\
where\ \epsilon ^2 &amp;= 0 \ and\ \epsilon \neq 0 \\
\end{align*}
So now lets take this idea of a dual number and explore how adding, multiplying and dividing them might be defined.
Addition and subtraction are simple - we just add the corresponding components - in much the same way that complex numbers are added. i.e.

\begin{align*}
(u + \epsilon u') + (v + \epsilon v') = (u + v) + \epsilon (u' + v') \\
\end{align*}

Similarly for subtraction.
\begin{align*}
(u + \epsilon u') - (v + \epsilon v') = (u - v) + \epsilon (u' - v') \\
\end{align*}

Multiplication requires some simple algebraic manipulation.
\begin{align*}
(u + \epsilon u')(v + \epsilon v') = uv + \epsilon uv' + \epsilon u'v + \epsilon ^2 u'v' \\
\end{align*}
and, as $\epsilon ^2 &amp;= 0$ this becomes
\begin{align*}
(u + \epsilon u')(v + \epsilon v') &amp;= uv + \epsilon uv' + \epsilon u'v \\
&amp;= uv + \epsilon (uv' + u'v) \\
\end{align*}

Finally to determine
\begin{align*}
\frac{(u + \epsilon u')}{(v + \epsilon v')} \\
\end{align*}
we take the dual conjugate, $(v - \epsilon v')$, of the denominator and multiply like this.

\begin{align*}
\frac{(u + \epsilon u')}{(v + \epsilon v')} &amp;= \frac{(u + \epsilon u')}{(v + \epsilon v')}\frac{(v - \epsilon v')}{(v - \epsilon v')} \\
&amp;= \frac{uv + \epsilon vu' - \epsilon uv'}{v^2} \\
&amp;= \frac{u}{v} + \epsilon \frac{(vu' -uv')}{v^2} \\
\end{align*}

So, where is this going you ask? Well Dual numbers can be used to compute the exact value of the derivatives of a function at a given point. Not just approximately - but exactly, at least with regard to the accuracy of the machine. The so called<strong> 'Automatic Differentiation'</strong>.

When calculating, from first principles, the derivative of a function $f(x)$ it can be shown that

\begin{align*}
f(x + h) &amp;= f(x) + hf'(x) \\
\end{align*}

for suitably small value of h. However too big or too small a value of h gives wild inaccuracies. But if we use dual numbers then the values are calculated to machine accuracy! For example

\begin{align*}
let\ f(x) &amp;= x^2 + 1 \  and\ lets\ find\  f'(4).\ To\ do\ that\ we\ evaluate\ f(4 + \epsilon) \\
f(4 + \epsilon) &amp;= 17 + 8\epsilon + \epsilon ^2 \\
&amp;= 17 + 8\epsilon \\
\end{align*}
And, by direct calculation,
\begin{align*}
f(x) &amp;= x^2 + 1 \\
hence\ f'(x) &amp;= 2x \\
\\
f(4) &amp;= 17 \\
f'(4) &amp;= 8 \\
\end{align*}

or, in dual form we have

\begin{align*}
f(4 + \epsilon) = 17 + 8\epsilon = f(4) + \epsilon f'(4) \\
\end{align*}
so both the function and its first derivate are calculated in 'one go'!
<h3>Simple Implementation</h3>
Now we need to create a type for dual in such a way that wherever we can write

$f(Of\ some\ real\ number)$ we can also write $f(Of\ the\ corresponding\ dual\ number)$

OK! What's my favourite language? Which language could express these notions in a clear and compact way?
<pre class="lang:haskell decode:true ">data Dual a = Dual a a deriving (Show)</pre>
This is parameterised over a general type, a, which will allow more flexibility if needed later. (e.g. complex numbers rather than reals.)
The next step will be to make Dual into a Number, typing :i Num into ghci shows what's needed.
<pre class="lang:haskell decode:true ">λ-&gt; :i Num
class Num a where
  (+) :: a -&gt; a -&gt; a
  (-) :: a -&gt; a -&gt; a
  (*) :: a -&gt; a -&gt; a
  negate :: a -&gt; a
  abs :: a -&gt; a
  signum :: a -&gt; a
  fromInteger :: Integer -&gt; a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}</pre>
Here's one I made earlier... and using x, dx notation to perhaps reinforce the link to derivatives.
<pre class="lang:haskell decode:true ">instance Num a =&gt;  Num (Dual a) where
    -- # MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #
    (Dual x dx) + (Dual y dy) = Dual (x + y) (dx + dy)
    (Dual x dx) - (Dual y dy) = Dual (x - y) (dx - dy)
    (Dual x dx) * (Dual y dy) = Dual (x * y) (x * dy + y * dx)
    abs (Dual x dx)           = Dual (abs x) (dx * (signum x))
    signum (Dual x dx)        = Dual (signum x) 0
    fromInteger n             = Dual (fromInteger n) 0
</pre>
Most of the above we've covered already. For <span class="lang:haskell decode:true crayon-inline ">abs (Dual x dx) = Dual (abs x) (dx * (signum x))</span>, well the signnum function can be <a href="https://en.wikipedia.org/wiki/Sign_function">defined</a> as the derivative of the absolute value function.

To try this out we need a couple of helper functions:
<pre class="lang:haskell decode:true ">derivDual :: Dual a -&gt; (a, a)
derivDual (Dual x x') = (x, x')

derivfx :: Num a =&gt; (Dual a -&gt; Dual a) -&gt; a -&gt; (a, a)
derivfx f x = derivDual . f $ Dual x 1
</pre>
The first one, <em>derivDual</em>, simply pattern matches and returns a tuple of the value and the value of the derivative. The helper function <em>derivfx</em> takes an 'x', makes it into a dual, applies f and then extracts the result. Let's try it with $f(x) &amp;= 3x^2 + 4x + 3$ and 'manually' we have

\begin{align*}
f(x) &amp;= 3x^2 + 4x + 3 \\
f'(x) &amp;= 6x + 4 \\
f(2) &amp;= 23 \\
f'(2) &amp;= 16
\end{align*}

and in ghci:
<pre class="lang:haskell decode:true ">λ-&gt; f = \x -&gt; 3*x^2 + 4*x + 3
*Main
λ-&gt; derivfx f 2
(23,16)
λ-&gt; derivfx f 3
(42,22)
*Main
λ-&gt; derivfx f 42.45
(5578.807500000001,258.70000000000005)
-- and so on...</pre>
Isn't that satisfying? It's a kind of Voodoo magic - and expressing it in Haskell really complements the mathematics.

Now, what about a function that includes division? For example $f(x) = \frac{2}{3x^2 + 1}$, to handle division we need the <em>Fractional </em> typeclass.
<pre class="lang:haskell decode:true ">λ-&gt; :i Fractional
class Num a =&gt; Fractional a where
  (/) :: a -&gt; a -&gt; a
  recip :: a -&gt; a
  fromRational :: Rational -&gt; a
  {-# MINIMAL fromRational, (recip | (/)) #-}</pre>
From this we see that we need <em>fromRational</em>, that's easy enough and division - well, we've already done that in the first section about dual numbers.
<pre class="lang:haskell decode:true ">instance Fractional a =&gt; Fractional (Dual a) where
  Dual u u' / Dual v v' = Dual (u / v) ((u' * v - u * v')/(v * v))
  fromRational x = Dual (fromRational x) 0
</pre>
So with this now added we can try some fractional functions.
<pre class="lang:haskell decode:true ">λ-&gt; f = \x -&gt; (x^2 - 3*x) /(x^3 + 4*x^2 - 10 * x + 1)
*Main
λ-&gt; derivfx f 2
(-0.4,1.64)

λ-&gt; derivfx f 5.25
(5.8060056831272557e-2,4.134796318136811e-3)</pre>
To check these results you can of course work out the derivate of
\begin{align*}
f(x) &amp;= \frac{x^2 - 3x}{x^3 + 4x^2 - 10x + 1} \\
\end{align*}
to be
\begin{align*}
f'(x) &amp;= -\dfrac{x^4-6x^3-2x^2-2x+3}{\left(x^3+4x^2-10x+1\right)^2} \\
\end{align*}
and then plugin the values of x. However I found this excellent '<a href="http://www.derivative-calculator.net/">Online Derivative Calculator</a>' that will save you a lot of paper and ink!

OK. So far we can readily calculate the derivatives of polynomial functions. What about trigonometric and log functions etc. Well, Haskell has the typeclass Floating and a quick query in ghci shows
<pre class="lang:haskell decode:true ">λ-&gt; :i Floating
class Fractional a =&gt; Floating a where
  pi :: a
  exp :: a -&gt; a
  log :: a -&gt; a
  sqrt :: a -&gt; a
  (**) :: a -&gt; a -&gt; a
  logBase :: a -&gt; a -&gt; a
  sin :: a -&gt; a
  cos :: a -&gt; a
  tan :: a -&gt; a
  asin :: a -&gt; a
  acos :: a -&gt; a
  atan :: a -&gt; a
  sinh :: a -&gt; a
  cosh :: a -&gt; a
  tanh :: a -&gt; a
  asinh :: a -&gt; a
  acosh :: a -&gt; a
  atanh :: a -&gt; a
  GHC.Float.log1p :: a -&gt; a
  GHC.Float.expm1 :: a -&gt; a
  GHC.Float.log1pexp :: a -&gt; a
  GHC.Float.log1mexp :: a -&gt; a
  {-# MINIMAL pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh,
              asinh, acosh, atanh #-}</pre>
and again we can see the minimal requirements. The implementation is below and is really just a matter of working out or looking up the derivatives of the required functions.
<pre class="lang:haskell decode:true ">instance (Fractional a, Floating a) =&gt;Floating (Dual a) where
  pi                = Dual pi 0
  exp   (Dual x dx) = Dual (exp x) (dx * exp x)
  log   (Dual x dx) = Dual (log x) (dx / x)
  sin   (Dual x dx) = Dual (sin x) (dx * cos x)
  cos   (Dual x dx) = Dual (cos x) (- dx * sin x)
  asin  (Dual x dx) = Dual (asin x) (dx / (sqrt(1 - x ** 2)))
  acos  (Dual x dx) = Dual (acos x) (- dx / (sqrt(1 - x ** 2)))
  atan  (Dual x dx) = Dual (atan x) (dx / (1 + x ** 2))
  sinh  (Dual x dx) = Dual (sinh x) (dx * cosh x)
  cosh  (Dual x dx) = Dual (cosh x) (dx * sinh x)
  asinh (Dual x dx) = Dual (asinh x) (dx / (sqrt(1 + x ** 2)))
  acosh (Dual x dx) = Dual (acosh x) (dx / (sqrt(x ** 2 - 1)))
  atanh (Dual x dx) = Dual (atanh x) (dx / (1 - x ** 2))
</pre>
And trying out some of these functions:
<pre class="lang:haskell decode:true ">λ-&gt; f = \x -&gt; pi**x + sin(x) + x * exp(x^2)
*Main
λ-&gt; derivfx f 2.1
(184.69569597379345,820.0495684271507)
*Main
λ-&gt; f = \x -&gt; (exp 1)**(x/2) * sin(4*x)
*Main
λ-&gt; derivfx f 3.4
(4.703006575505323,13.555666227053901)
*Main</pre>
A slightly more convoluted one:
<pre class="lang:haskell decode:true ">*Main
λ-&gt; f = \x -&gt;  log(log(log x))
*Main
λ-&gt; derivfx f 3.5
(-1.4900939320151863,1.0120515180097984)
*Main</pre>
And, just for fun,
<pre class="lang:haskell decode:true ">λ-&gt; f = \x -&gt;  sin (x** (log(log(log x))))
*Main
λ-&gt; derivfx f 24.5
(0.9988011331347698,-3.2713665802309565e-3)</pre>
and the differential by 'hand' is
<img class="alignnone wp-image-764" src="http://gitcommit.co.uk/wp-content/uploads/2017/06/Screen-Shot-2017-06-24-at-19.25.45-300x193.png" alt="" width="904" height="581" />

'<a href="http://www.derivative-calculator.net/">Online Derivative Calculator</a>' ;)

This is really just an introduction to the topic but I hope it gives a flavour of the potential it has. And, of course, the elegance of the Haskell type system comes to the fore with things like this. The next post in this series will look at extending these ideas to calculate 2nd and higher derivatives. In fact an infinite stream of derivatives based on using <span class="lang:haskell decode:true crayon-inline ">data Dual a = Dual a (Dual a)</span> as a recursive definition of Dual. All the code is in <a href="https://github.com/banditpig/autodiff">github</a> and...

Thanks for reading!