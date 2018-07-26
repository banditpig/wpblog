---
ID: 1651
post_title: Walking the Tree.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/07/26/walking-the-tree/
published: true
post_date: 2018-07-26 18:30:21
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />It's just fun playing around with Stern-Brocot trees and a bit of Haskell...!

For any  fraction on the Stern-Brocot tree there is just one path to it from the root. That path can written as a sequence of left or right 'turns' at each node depending on whether the target node is smaller or larger than the current node. This leads to the
<h4>Stern Path</h4>
For example in this small tree

<img class="alignnone size-full wp-image-1652" src="http://gitcommit.co.uk/wp-content/uploads/2018/07/tree.png" alt="" width="347" height="291" />

the path to, say, 4/5 is, from 1/1, LRRR. The fraction 5/2 has path RRL. To get to 5/8 we go LRLR... and so on.  (If you want to use the <a href="http://gitcommit.co.uk/wp-content/uploads/2018/07/test.html">larger tree</a> then you'll need to mentally rotate the tree - or modify the code :) )

Using code already written and adding a bit more we can write a bit of Haskell to create a path generator.

First some types
<pre class="lang:haskell decode:true ">--
data SternTerm = L | R deriving (Eq, Show)
type SternPath = [SternTerm]
</pre>
In other words a <em>SternPath</em> is just a list of<em> L</em> and <em>R</em>.

To generate the path for a given fraction we first generate a Stern-Brocot tree (lazily as we don't know how deep to go, see <a href="http://gitcommit.co.uk/2018/07/16/a-stern-tree/">earlier post</a> for <em>buildBrocTreeLazy</em>) and then build up the path by doing what is really a search on a binary tree. Like this:
<pre class="lang:haskell decode:true">--
sternPath :: Fraction -&gt; SternPath
sternPath frac = go (fraction &lt;$&gt; buildBrocTreeLazy) fr [] where
        fr = reduce frac
        go (BNode (F p q) l r) (F n d) path
            | p == n &amp;&amp; q == d   = path
            | F p q &lt; F n d      = go r fr (R : path)
            | otherwise          = go l fr (L : path)

</pre>
We create a tree and <em>fmap</em> <em>fraction</em> over it - this gives us a <em>Tree Fraction</em>. (see <a href="http://gitcommit.co.uk/2018/07/16/a-stern-tree/">previous posts</a> for details of <em>Tree</em>). Next, we make sure the supplied Fraction is in its simplest form and then we recursively append <em>R</em> or<em> L</em> to the result and quit when we find the Fraction. We know that the function will terminate as the Stern-Brocot contains all positive rational numbers... Very neat and pleasing.

Reducing a path is also quite simple - here's a version that returns a list of fractions - each corresponding to the term in the path.
<pre class="lang:haskell decode:true ">--
fractionPath :: SternPath -&gt; [Fraction]
fractionPath  = go ( fraction &lt;$&gt; buildBrocTreeLazy)  where
    go (BNode frac _ _) []     = [frac]
    go (BNode frac l r) (p:ps) = frac : go (pick p l r) ps where
        pick p l r = if p == L then l else r
</pre>
Some examples in GHCI:
<pre class="lang:haskell decode:true">--
λ-&gt; fractionPath [L,L,L,L,L,L,L,L]
[1/1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9]
*Matrix
λ-&gt; fractionPath [R,R,R,R,R,R,R,R]
[1/1,2/1,3/1,4/1,5/1,6/1,7/1,8/1,9/1]</pre>
which is what you'd expect.

A really interesting path to evaluate is alternating <em>L</em> then <em>R</em> i.e.
<pre class="lang:haskell decode:true ">λ-&gt; p = [L,R,L,R,L,R,L,R,L,R,L,R,L,R,L,R,L,R,L,R,L,R,L,R,L,R,L,R]
*Matrix
λ-&gt; fractionPath p
[1/1,1/2,2/3,3/5,5/8,8/13,13/21,21/34,34/55,55/89,89/144,144/233,233/377,
377/610,610/987,987/1597,1597/2584,2584/4181,4181/6765,6765/10946,
10946/17711,17711/28657,28657/46368,46368/75025,75025/121393,121393/196418,
196418/317811,317811/514229,514229/832040]</pre>
and  we see that the numerators (or denominators) are the Fibonacci numbers! (Which ties in quite nicely with the earlier posts about <a href="http://gitcommit.co.uk/2017/11/16/fractions-to-phi-to-fibonacci/">continued fractions</a> ).

It is also fascinating that we can express the <em>SternPath</em> as the multiplication of 2x2 matrices! This is discussed in detail in <em>Concrete Mathematics by Graham et al</em>

which is a gem of a book and gives a more detailed treatment of the stern path expressed as matrix manipulation. What follows next is just an overview of what's discussed in <em>Concrete Mathematics.</em>
<h4>Stern Matrices</h4>
In Haskell we can defines a data type <em>Matrix</em> and define a '<em>left</em>', '<em>right</em>' and identity matrix and make a <em>Monoid</em> instance for it like this
<pre class="lang:haskell decode:true">--
data Matrix = M Integer Integer Integer Integer deriving Show

instance Monoid Matrix where
    mempty = ident -- Identity 2x2 matrix
    -- matrix multiplication
    mappend (M a b c d) (M w x y z) =
        M (a*w + b*y) (a*x + b*z) (c*w + d*y) (c*x + d*z)

left :: Matrix
left = M 1 1 0 1

right :: Matrix
right = M 1 0 1 1

ident :: Matrix
ident = M 1 0 0 1
</pre>
And any Matrix - <em>M a b c d</em> - represents the fraction (c + d)/(a + b) and, even more 'amazingly', the immediate ancestor fractions are given by c/a and d/b.

We can reduce a <em>Matrix</em>  to a  <em>Fraction</em> like this:
<pre class="lang:haskell decode:true">--
reduceMatrix :: Matrix -&gt; Fraction
reduceMatrix (M a b c d) = F (c + d) (a + b)</pre>
and map a <em>SternTerm</em> to a <em>Matrix</em> with this simple function
<pre class="lang:haskell decode:true">--
sternTermMatrix :: SternTerm -&gt; Matrix
sternTermMatrix t = if t == L then left else right</pre>
With these these functions we can reduce a path to a fraction like this:
<pre class="lang:haskell decode:true">--
reduceSternPath :: SternPath -&gt; Fraction
reduceSternPath   = reduceMatrix . foldl (\ acc t -&gt; sternTermMatrix t &lt;&gt; acc ) ident
</pre>
where we fold over the list making use of the Monoidal nature of <em>Matrix</em>.  So, for example,
<pre class="lang:haskell decode:true ">--
λ-&gt; reduceSternPath [L,L,L,L,L,L,L,L]
1/9
λ-&gt; reduceSternPath [R,R,R,R,R,R,R,R]
9/1
</pre>
Whilst <em>reduceSternPath </em>is OK, a neater implementation is
<pre class="lang:haskell decode:true">--
reduceSternPath' :: SternPath -&gt; Fraction
reduceSternPath' = reduceMatrix . mconcat . fmap sternTermMatrix
</pre>
where we use the 'free' (Monoid) implementation of <em>mconcat</em> to reduce a list of <em>Matrix</em> that's created by <em>fmapping</em> over the <em>SternPath</em>.  It's little things like this that make Haskell such a joy to use and shows that you don't always have to swim in the deep end to have fun with the language!

All the code is in Github and thanks for reading!

&nbsp;

&nbsp;