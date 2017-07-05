---
ID: 779
post_title: >
  Dual Numbers and Differentiation. Part
  2.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/06/29/dual-numbers-and-differentiation-part-2/
published: true
post_date: 2017-06-29 18:45:39
---
[latexpage]

<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />At the end of the <a href="http://gitcommit.co.uk/2017/06/24/dual-numbers-and-differentiation-part-1/">previous</a> post I had intended this posting to be an exploration of a recursive definition of <em>Dual</em> that will give an infinite (lazy) list of derivatives. However, there's still a lot to play with using our simple
<span class="lang:haskell decode:true crayon-inline ">data Dual a = Dual a a</span> Let's try a simple function of two variables...

\begin{align*}
f(x,y) &amp;= x^2 + xy \\
f(1,2) &amp;= 3 \\
\frac{\partial f}{\partial x} &amp;= 2x + y \\
\frac{\partial f}{\partial y} &amp;= x \\
\end{align*}
and at $(1,2)$ we have
\begin{align*}

\frac{\partial f}{\partial x} &amp;= 2x + y = 2 \times 1 + 2 = 4 \\
\frac{\partial f}{\partial y} &amp;= 1 \\
\end{align*}

Now we can evaluate $f(x,y) &amp;= x^2 + xy$ at $(1,2)$ using dual numbers with a subscript of x or y to 'remember' where it came from...i.e we want $f(1 + \epsilon_x, 2 + \epsilon_y)$ but really $\epsilon_x, \epsilon_y$ are the 'same thing'.

\begin{align*}
f(1 + \epsilon_x, 2 + \epsilon_y) &amp;= (1 + \epsilon_x)^2 + (1 + \epsilon_x)(2 + \epsilon_y) \\
&amp;= 1 + \epsilon_x^2 + 2\epsilon_x + 2 + \epsilon_y + 2\epsilon_x + \epsilon_x \epsilon_y \\
&amp;= 3 + 4 \epsilon_x + \epsilon_y \\
\end{align*}
Notice that the coefficients of $\epsilon_x, \epsilon_y$ are the same as the partial derivatives! This sort of suggests that we can get $\frac{\partial f}{\partial x}$ in isolation by setting $\epsilon_y = 0$. In effect setting $\epsilon_y = 0$ is the equivalent of treating $y$ as constant and similarly for $\epsilon_x = 0$, which is how partial derivatives are calculated.

So let's try this in Haskell...
<pre class="lang:haskell decode:true ">-- The function
λ-&gt; f = \x y -&gt; x^2 + x*y

-- evaluate at (1,2)
λ-&gt; f 1 2
3
-- Try dual keeping y constant - to get partial dx
λ-&gt; f (Dual 1 1) (Dual 2 0)
Dual 3 4
-- now x constant to get partial dy
λ-&gt; f (Dual 1 0) (Dual 2 1)
Dual 3 1</pre>
as can be seen the partial derivatives simply drop out!

Extending to three variables and hiding the use of <em>Dual</em> we have
<pre class="lang:haskell decode:true ">pDx, pDy, pDz :: (Num a) =&gt; (Dual a -&gt; Dual a -&gt; Dual a -&gt; t) -&gt; a -&gt; a -&gt; a -&gt; t
pDx f x y z = f (Dual x 1) (Dual y 0) (Dual z 0) 
pDy f x y z = f (Dual x 0) (Dual y 1) (Dual z 0) 
pDz f x y z = f (Dual x 0) (Dual y 0) (Dual z 1) 
</pre>
Which leads to a simple function Haskell function for the gradient operator

\begin{flalign*}
\nabla f(x, y, z) = \frac{\partial f}{\partial x}\hat{\imath} + \frac{\partial f}{\partial y}\hat{\jmath} + \frac{\partial f}{\partial z}\hat{k} \ \ \\
\end{flalign*}
<pre class="lang:haskell decode:true ">grad :: (Num a, Num a, Num a) =&gt; (Dual a -&gt; Dual a-&gt; Dual a -&gt; Dual a) -&gt; a -&gt; a -&gt; a -&gt; (a, a, a)
grad f x y z =   (x', y', z' ) where Dual _ x' = pDx f x y z
                                     Dual _ y' = pDy f x y z
                                     Dual _ z' = pDz f x y z
</pre>
Actually if we import the <em>Vector</em> package, used in the series <em>'Haskell, Vectors and Simple Mechanics</em>' then we can use '<em>Vector</em>' and write a 'proper' <em>grad</em> function:
<pre class="lang:haskell decode:true">gradV :: (Dual Scalar -&gt; Dual Scalar -&gt; Dual Scalar -&gt; Dual Scalar)  -&gt; Scalar -&gt; Scalar -&gt; Scalar -&gt; Vector
gradV f x y z = V  (grad f x y z)
</pre>
where we simply create a <em>Vector</em> from the result of applying the <em>grad</em> function - <em>gradV</em>.  Using a Vector will then open it up to further vector-based manipulation...

For example, this is taken from <a href="https://math.oregonstate.edu/home/programs/undergrad/CalculusQuestStudyGuides/vcalc/grad/grad.html">'The Gradient and Directional Derivatives'</a>

<span style="color: #3366ff;"><b>Example</b></span>

<span style="color: #3366ff;">What is the directional derivative in the direction &lt;1,2&gt; of the function z=f(x,y)=4x^2+y^2 at the point x=1 and y=1. The gradient is &lt;8x,2y&gt;, which is &lt;8,2&gt; at the point x=1 and y=1. The direction u is &lt;2,1&gt;. Converting this to a unit vector, we have &lt;2,1&gt;/sqrt(5). Hence,</span>

<span style="color: #3366ff;"><img src="https://math.oregonstate.edu/home/programs/undergrad/CalculusQuestStudyGuides/vcalc/grad/img5.gif" alt="displaymath70" width="433" height="40" align="BOTTOM" /></span>

&nbsp;

And working the above out in <em>ghci</em> using our <em>gradV</em> and <em>Vector</em> package...
<pre class="lang:haskell decode:true">-- The initial function
λ-&gt; f = \x y _ -&gt; 4*x^2 + y^2

-- The direction u is &lt;2,1&gt;
λ-&gt; v = V (2, 1, 0)

--  Converting this to a unit vector
λ-&gt; vn = normalise v

-- dot product of gradV and vn
λ-&gt; (gradV f 1 1 0) &gt;.&lt; vn
8.049845

-- which is  18/(sqrt 5)    :)

</pre>
So really this post was a bit of an aside to show how easily the use of <em>Dual</em> extends to functions of more than one variable and how the gradient operator can be easily defined. We then tied this into a simple example using <em>Vectors</em>. Next time we will look at extending the <em>Dual</em> idea to produce second and higher order derivatives and see where that takes us! As always the code is on <a href="https://github.com/banditpig/autodiff">Github</a>.

Thanks for reading!