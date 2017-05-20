---
ID: 469
post_title: 'Haskell, Vectors and Simple Mechanics &#8211; part 1.'
author: BanditPig
post_date: 2017-05-07 14:28:37
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/05/07/haskell-vectors-and-simple-mechanics-part-1/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />I like vectors! A long time ago my maths teacher introduced me to them and I just like the way three numbers (typically three) can express the notion of a position in space and combining these under different operations can produce other interesting properties.

Just recently   I found this paper <a href="https://arxiv.org/pdf/1412.4880.pdf">Learn Physics by Programming in Haskell</a> which gives a very interesting Haskell oriented discussion on vectors and their use in  mechanical problems. Reading it has inspired me to create my own implementation of vectors, bore you all senseless with it and see where it goes.  What I'd like to do is:
<ul>
 	<li>Create a module for Vectors. How far this will go I'm not sure yet but it will most likely be over several posts and might get as far as vector calculus.  I'll also try to look at Monoids,  Monads etc.  from a vector perspective and indeed see if  there are meaningful vector oriented implementations of  these classes. I don't know yet. Almost all my Haskell posts are learning experiences for me!</li>
 	<li>Apply the vector module to simple mechanical and geometric problems.</li>
 	<li>Create some sort of visualisation of the solutions to these problems. Here the Haskell <a href="http://gloss.ouroborus.net/">Gloss package</a> is a prime contender.</li>
 	<li>And it would be instructive to use <a href="https://hackage.haskell.org/package/QuickCheck">QuickCheck</a> too!</li>
</ul>
All nice and open ended and I'm writing the code more or less as I write the blog, so a lot may change! The code is in <a href="https://github.com/banditpig/vectors">Github</a>.

So here's my first pass at a vector module.
<pre class="lang:haskell decode:true ">{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}

module Vectors
  ( Scalar
  , Vector
  , (^+^)
  , (^-^)
  , (^*)
  , (*^)
  , (&gt;&lt;)
  , (&gt;.&lt;)
  , (^/)
  , i
  , j
  , k
  , origin
  , normalise
  , neg
  , mag
  ) where

type Scalar = Double

type XYZ = (Scalar, Scalar, Scalar)

newtype Vector = V { xyz :: XYZ} deriving (Show)

-- Define operators using  functions defines later
infixl 6 ^+^
(^+^) :: Vector -&gt; Vector -&gt; Vector
(^+^) = vAdd

infixl 6 ^-^
(^-^) :: Vector -&gt; Vector -&gt; Vector
(^-^) = vSub

infixl 7 *^
(*^) :: Scalar -&gt; Vector -&gt; Vector
(*^) = sMul

infixl 7 ^*
(^*) :: Vector -&gt; Scalar -&gt; Vector
(^*) = sMul'

infixl 7 ^/
(^/) :: Scalar -&gt; Vector -&gt; Vector
(^/) = sDiv

infixl 7 &gt;.&lt;
(&gt;.&lt;) :: Vector -&gt; Vector -&gt; Scalar
(&gt;.&lt;) = dot

infixl 7 &gt;&lt;
(&gt;&lt;) :: Vector -&gt; Vector -&gt; Vector
(&gt;&lt;) = cross

--  The 'traditional' i, j, k 
i, j, k, origin :: Vector
i = V (1, 0, 0)
j = V (0, 1, 0)
k = V (0, 0, 1)
origin = V (0, 0, 0)

-- Normalise 
normalise :: Vector -&gt; Vector
normalise (V (0, 0, 0)) = V (0, 0, 0) 
normalise v = sDiv (mag v) v

-- negate vector
neg :: Vector -&gt; Vector
neg = mapVec ((-1) *)

-- Magnitude of a vector
mag :: Vector -&gt; Scalar
mag = sqrt . sumVec . mapVec (^2)

-- ----------------------------------------------------------
-- Map a function over the internal tuple
mapVec :: (Scalar -&gt; Scalar) -&gt; Vector -&gt; Vector
mapVec f (V (x, y, z)) = V (f x, f y, f z)

-- Just sum the tuple entries
sumVec :: Vector -&gt; Scalar
sumVec (V (x, y, z)) = x + y + z

-- Apply a scalar function to x,y,z components of each vector
zipWithVec :: (Scalar -&gt; Scalar -&gt; Scalar) -&gt; Vector -&gt; Vector -&gt; Vector
zipWithVec f (V (x, y, z)) (V (x', y', z')) = V (f x x', f y y', f z z')

-- These are fairly obvious.  The functions defined above
-- make these functions simpler and cleaner.

-- Just adding/subtracting  the corresponding components, 
-- so zipWithVec using (+) or (-)
vAdd :: Vector -&gt; Vector -&gt; Vector
vAdd = zipWithVec (+)

vSub :: Vector -&gt; Vector -&gt; Vector
vSub = zipWithVec (-)

-- Scalar then Vector - just map (*) over tuple entries
sMul :: Scalar -&gt; Vector -&gt; Vector
sMul s = mapVec (* s)

-- or Vector then Scalar
sMul' :: Vector -&gt; Scalar -&gt; Vector
sMul' = flip sMul

sDiv :: Scalar -&gt; Vector -&gt; Vector
sDiv s = mapVec (/ s)

-- dot product
dot :: Vector -&gt; Vector -&gt; Scalar
dot x = sumVec . zipWithVec (*) x

-- Cross product - is there a neater way?
-- note also that b×a=−a×b  and a×a=0 - will use QuickCheck
cross :: Vector -&gt; Vector -&gt; Vector
cross (V (u1, u2, u3)) (V (v1, v2, v3)) =
  V (u2 * v3 - u3 * v2, u3 * v1 - u1 * v3, u1 * v2 - u2 * v1)


</pre>
The Vector itself is defined as a Haskell <em>newtype</em>. Internally the Vector has a three-tuple of Double (with type synonym Scalar). I went for a three-tuple rather than a list as all our vectors will be at most three dimensional and a tuple seemed a good fit.  Maybe a list would be 'better' as it might simplify slightly the high-level functions <em>mapVec, sumVec and zipWithVec</em> that I've defined. The purpose of <em>mapVec, sumVec and zipWithVec</em> is to allow quite simple derivations of vector operations for addition, multiplication etc.  along with the dot and cross product. (Having said all that I prefer the notation (x, y, z) to [x, y, z] as it looks more like how it would be written!)
Note also that there's a number of custom operators, e.g. '^+^', these allow us to write <span class="lang:haskell decode:true crayon-inline ">a ^+^ b </span> rather than <span class="lang:haskell decode:true crayon-inline ">vAdd a b</span> or <span class="lang:haskell decode:true crayon-inline ">a `vAdd` b</span>

We can load this into <em>ghci</em> and try a few examples...
Here we just declare a vector <em>a</em> with components <em>(1,2,3)</em>
<pre class="lang:haskell decode:true">ghci-&gt; a = V (1,2,3)
ghci-&gt; a
V {xyz = (1.0,2.0,3.0)}
ghci-&gt;</pre>
The magnitude and negation (reversal) of vector <em>a.</em>
<pre class="lang:haskell decode:true ">ghci-&gt; mag a
3.7416573867739413
ghci-&gt; neg a
V {xyz = (-1.0,-2.0,-3.0)}</pre>
This shows subtraction of vectors and the cross product.
<pre class="lang:haskell decode:true ">ghci-&gt; a = V (4,6,-1)
ghci-&gt; b = V (1,7,12)
ghci-&gt;
ghci-&gt; a ^-^ b
V {xyz = (3.0,-1.0,-13.0)}
ghci-&gt; b ^-^ a
V {xyz = (-3.0,1.0,13.0)}
ghci-&gt; a &gt;&lt; b
V {xyz = (79.0,-49.0,22.0)}</pre>
The dot product and its type.
<pre class="lang:haskell decode:true">ghci-&gt; a &gt;.&lt; b
34.0
ghci-&gt; :t a &gt;.&lt; b
a &gt;.&lt; b :: Scalar</pre>
So these results look right but it would be much better to have some thorough testing and that's where QuickCheck comes in and will be covered in part 2 along with the first steps in having a visual representation of a vector.

Thanks for reading!