---
ID: 1708
post_title: A Glitch In The Matrix.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2019/03/17/a-glitch-in-the-matrix/
published: true
post_date: 2019-03-17 20:15:19
---
<p style="text-align: left;"><img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />This is the first in a series of posts that will eventually get round to implementing a Kalman Filter (or filters) in Haskell.  It's not my intention to explain Kalman filtering in any detail - just sufficiently to complelent the Haskell code. There's a plethora of tutorials and videos about Kalman Filtering and one of the better series is <a href="http://www.ilectureonline.com/lectures/subject/SPECIAL%20TOPICS/26">here</a>.</p>
<p style="text-align: left;">As will be seen, the computations involved in Kalman Filtering are effectively expressed using matrices. The Haskell ecosystem has a number of Matrix libraries but I'll learn more by writing my own library even if its scope is limited to the immediate requirement of Kalman Filter computations.</p>
When thinking about implementing basic functions for matrices I wondered how - or if - the number of rows and columns could be encoded in the type rather than as a parameter in the matrix itself.  So what follows is my limited exploration of type jiggery in Haskell.

We can define a Matrix type
<pre class="lang:haskell decode:true ">newtype Matrix (r :: Nat) (c :: Nat) a = Matrix [[a]] deriving Show
</pre>
Where <em>r</em> and <em>c</em> are so called Phantom Type. i.e. Types that appear in the type constructor but not in the data constructor. It's a form of 'tag' on the data type that exists at compile time but is lost at runtime. Having the tags a compile time prevents errors being introduced  in the first place rather than having to handle them at runtime.
<h3>Matrix Multiplication</h3>
So what sort of errors can this phantom type help catch at compile time? Matrix multiplication is an example and was the motivation behind these experiments.
Multiplication of two matrices is only defined if the number of columns in the first matrix is equal to the number of rows in the second.

<em><strong>M1 = [m, n]</strong></em>

<em><strong>M2 =[p, q]</strong></em>

then for multiplication to be valid  <strong> <em>n == p</em></strong> and the result is<em><strong> M3 = [m, q]</strong></em>

And the corresponding function signature is
<pre class="lang:haskell decode:true">mul :: Num a =&gt; Matrix n m a -&gt; Matrix m p a -&gt; Matrix n p a</pre>
and at the moment it doesn't matter what the implementation is - a compile time error will occur if the <em>mul</em> function is called without the correct types.

e.g. If we try to compile this snippet
<pre class="lang:haskell decode:true">mul :: Num a =&gt; Matrix n m a -&gt; Matrix m p a  -&gt; Matrix n p a
mul = undefined

mu :: Matrix 3 3 Int
mu = Matrix [[1,2,2],[1,1,2], [4,5,6]]

mv :: Matrix 4 2 Int
mv = Matrix   [[1,2],[3,4], [1,1], [4,4]]


</pre>
we get
<div>
<div>
<pre class="lang:haskell decode:true">Matrix.hs:89:15: error:
    • Couldn't match type ‘4’ with ‘3’
      Expected type: Matrix 3 2 Int
        Actual type: Matrix 4 2 Int
    • In the second argument of ‘mul’, namely ‘mv’
      In the expression: mul mu mv
      In an equation for ‘prod’: prod = mul mu mv
   |
89 | prod = mul mu mv
   |               ^^
Failed, no modules loaded.</pre>
&nbsp;

</div>
</div>
The implementation of matrix multiplication is quite a nice algorithm to implement in Haskell and so I'll provide two!
<pre class="lang:haskell decode:true">mul :: Num a =&gt; Matrix n m a -&gt; Matrix m p a  -&gt; Matrix n p a
mul (Matrix xs) (Matrix ys ) = Matrix $ with xs $ \rowi -&gt; 
                                         with ys' $ \coli -&gt;
                                          sum $ zipWith (*) rowi coli
                                        where ys'= L.transpose ys

with ::  [a] -&gt; (a -&gt; b) -&gt; [b] 
with = flip fmap</pre>
here <em>with</em> - which is just <em>fmap</em> with its arguments swapped round - is used to 'offer up' a row to each column in turn, multiply corresponding entries and then add them all up.

An alternative - that doesn't explicitly use map is below.
<pre class="lang:haskell decode:true">mul' :: Num a =&gt; Matrix n m a -&gt; Matrix m p a  -&gt; Matrix n p a
mul' (Matrix xs) (Matrix ys ) = Matrix $ L.transpose . map (applyRow xs) . L.transpose $ ys 
  
applyRow :: Num a =&gt; [[a]] -&gt; [a] -&gt; [a]
applyRow [ys]     xs = [sum $ zipWith (*) xs ys]
applyRow (ys:yss) xs = sum (zipWith (*) xs ys) : applyRow yss xs
</pre>
<h3></h3>
<h3>Matrix Addition and Subtraction</h3>
Adding two matrices requires that both matrices have the same number of rows and columns. So this can be constrained by the function signature:
<pre class="lang:haskell decode:true">add :: Num a =&gt; Matrix n m a -&gt; Matrix n m a  -&gt; Matrix n m a
</pre>
and an implementation of <em>add</em> just needs to add corresponding elements in a  '<em>zipWith zipWith...</em>' style like this.
<pre class="lang:haskell decode:true">add :: Num a =&gt; Matrix n m a -&gt; Matrix n m a  -&gt; Matrix n m a
add (Matrix xs) (Matrix ys ) = Matrix $ zipWith (zipWith (+)) xs  ys
</pre>
and of course subtraction is very similar.
<pre class="lang:haskell decode:true">sub :: Num a =&gt; Matrix n m a -&gt; Matrix n m a  -&gt; Matrix n m a
sub (Matrix xs) (Matrix ys ) = Matrix $ zipWith (zipWith (-)) xs ys
</pre>
Clearly <em>add</em> and <em>sub</em> are very similar, they both embed a binary operation  and this can be abstracted into a separate function like this
<pre class="lang:haskell decode:true">-- A binary operation on square matrices.
sqOp :: Num a =&gt; (a -&gt; a -&gt; a) -&gt; Matrix n m a -&gt; Matrix n m a -&gt; Matrix n m a   
sqOp f (Matrix xs) (Matrix ys ) = Matrix $ zipWith (zipWith f) xs ys                             
</pre>
and now <em>add</em> and <em>sub</em> can now be written in a  much simpler way:
<pre class="lang:haskell decode:true ">--
add' :: Num a =&gt; Matrix n m a -&gt; Matrix n m a  -&gt; Matrix n m a
add' = sqOp (+)

sub' :: Num a =&gt; Matrix n m a -&gt; Matrix n m a  -&gt; Matrix n m a
sub' = sqOp (-)</pre>
&nbsp;
<h3>Matrix Division</h3>
The division considered here is division of corresponding elements between two square matrices of the same size.

The functions for addition and subtraction have had a type constraint of <em>(Num a</em>) - however the type <em>Num</em> does not support a division operation as can be seen from GHCi:
<pre class="lang:haskell decode:true ">--
λ-&gt; :i Num
class Num a where
  (+) :: a -&gt; a -&gt; a
  (-) :: a -&gt; a -&gt; a
  (*) :: a -&gt; a -&gt; a
  negate :: a -&gt; a
  abs :: a -&gt; a
  signum :: a -&gt; a
  fromInteger :: Integer -&gt; a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}</pre>
So the problem here how  does the division operation get resolved from just a <em>Num</em> type? In other words which division operation is actually used? Is it integer type division or the division of double types? A neat solution to this is to create a custom type class that resolves the actual division operation and use this new type class as a constraint on the division operation. Like this.
<pre class="lang:haskell decode:true">--
class DivSupported a where
  divOp :: a -&gt; a -&gt; a
  
instance DivSupported Int     where divOp = div
instance DivSupported Integer where divOp = div
instance DivSupported Double  where divOp = (/)
instance DivSupported Float   where divOp = (/)   

divEls :: (DivSupported a, Num a) =&gt; Matrix n m a -&gt; Matrix n m a  -&gt; Matrix n m a
divEls = sqOp divOp</pre>
and division now just becomes another use of the <em>sqOp</em> function. :)

&nbsp;
<h3>A Glitch In the Matrix</h3>
As I was writing the Haskell code for matrices using types to encode constraints I kept feeling something was missing. And it was that defining a type signature such as <span class="lang:haskell decode:true crayon-inline ">ma :: Matrix 5 3 Double</span> is all well and good there's nothing to ensure that the matrix does actually have 5 rows and 3 columns. And the issue came again when  I looked at making a <em>Monoid</em> instance for the Matrix type. As a reminder, a <em>Monoid</em> is a <em>Semigroup</em> with an identity element and a <em>SemiGroup</em> is a binary operation defined over a set (i.e a type). I often find it instructive and enjoyable to define Monoids and Semigroups etc. on types and Matrix is no exception.

Using GHCi to get a reminder of a <em>SemiGroup</em>:
<pre class="lang:haskell decode:true">--
λ-&gt; :i Semigroup 
class Semigroup a where
  (&lt;&gt;) :: a -&gt; a -&gt; a
  GHC.Base.sconcat :: GHC.Base.NonEmpty a -&gt; a
  GHC.Base.stimes :: Integral b =&gt; b -&gt; a -&gt; a
  {-# MINIMAL (&lt;&gt;) #-}</pre>
we see that the operation &lt;&gt; needs defining. I decided to use matrix multiplication (addition would be just as 'good'). Clearly only multiplication on square matrices could form a <em>SemiGroup</em> so the <em>SemiGroup</em> instance for square matrices is
<pre class="lang:haskell decode:true">--
instance (Num a ) =&gt; Semigroup ( Matrix (n :: Nat) (n :: Nat) a) where 
    (&lt;&gt;) = mul</pre>
restricting it to <em>n x n</em> matrices. A <em>Monoid</em> is a <em>SemiGroup</em> with an identity element and for a <em>[n, n]</em> matrix the identity element is the matrix with 1s down the diagonal and 0s elsewhere. The problem with this is that for  square matrix <em>[n, n]</em> the <em>n</em> is encoded in the type and I don't see how to 'extract' it - or if indeed it is possible to do so. i.e. What I'd like to write for a matrix <em>Monoid</em>  would be...
<pre class="lang:haskell decode:true">--

instance (Num a ) =&gt; Monoid  ( Matrix (n :: Nat) (n :: Nat) a) where
    mempty  =  Matrix $  identity n 
 
-- Creates array of arrays with 1 in the main diagonal and 0 elsewhere.     
identity :: (Num a ) =&gt; Int -&gt; [[a]]
identity n | n &lt;= 0 = [[]]
           | otherwise = take n (fmap (take n) rc) where 
                           rc = (1 : repeat 0) : fmap (0 :) rc
</pre>
but, not unreasonably, the value for <em>n</em> in the <em>Monoid</em> is not available. At this point I reached the limit of my 'type voodo' skills in Haskell and will revisit the matrix and also have the row/column sizes as part of the data constructor as well as the type constructor. That way there's the type safety at compile time and the flexibility of having the row/column sizes available at runtime.

The code is <a href="https://github.com/banditpig/kalman">here on GitHub.</a>

And thanks for reading!

&nbsp;