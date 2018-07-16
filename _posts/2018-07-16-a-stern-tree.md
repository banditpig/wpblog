---
ID: 1629
post_title: A Stern Tree
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/07/16/a-stern-tree/
published: true
post_date: 2018-07-16 11:11:34
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />The purpose of this post is to show how to generate a visual representation of the Stern-Brocot tree, like this: <a href="http://gitcommit.co.uk/wp-content/uploads/2018/07/test.html">SBTree</a> the rendering of which uses Northwood's <a href="https://gojs.net/latest/index.html">GoJS</a> Javascript library.

To create to a rendered Stern-Brocot tree we need to:
<ul>
 	<li>Create a binary tree datatype with supporting functions.</li>
 	<li>Generate a specific type of binary tree - i.e. a Stern-Brocot tree.</li>
 	<li>Examine the GoJS model details and write some Haskell to  map the binary tree to the Javascript.</li>
</ul>
<h4>Binary Trees</h4>
Yes, I could have found an existing Haskell library for binary trees - but really, where's the fun in that?

A binary tree is a fairly simple recursive data structure that can be expressed as a sum data type:

<span class="lang:haskell decode:true crayon-inline ">data BTree a = Empty | BNode a (BTree a) (BTree a)</span>

this indicates that a <em>BTree</em> of a is either <em>Empty</em> or is a <em>BNode</em>  containing an <em>a</em> along with a left and right subtree.  There are numerous useful operations that can be defined on a <em>BTree</em> - fmapping over it, traversing it in various orders - many of which are shown here.
<pre class="lang:haskell decode:true ">--
--
module BinaryTrees where
import           Data.List      (sort)
import           Data.Tree
import           Data.Tree.View

data BTree a = Empty | BNode a (BTree a) (BTree a)

instance Functor BTree where
    fmap _ Empty                  = Empty
    fmap f (BNode v Empty Empty ) = BNode (f v) Empty Empty
    fmap f (BNode v l r )         = BNode (f v) (fmap f l) (fmap f r)

foldBTree :: (a -&gt; b) -&gt;  BTree a -&gt; [b]
foldBTree _ Empty         = []
foldBTree f (BNode v l r) = f v : foldBTree f l ++ foldBTree f r

foldBTreeNodes :: (a -&gt; BTree a -&gt; BTree a -&gt; b) -&gt;  BTree a -&gt; [b]
foldBTreeNodes _ Empty         = []
foldBTreeNodes f (BNode v l r) = f v l r : foldBTreeNodes f l ++ foldBTreeNodes f r

traverseDepthFirst :: BTree a -&gt; [a]
traverseDepthFirst Empty        = []
traverseDepthFirst (BNode a l r) = a : traverseDepthFirst l ++ traverseDepthFirst r

traverseBreadthFirst :: BTree a -&gt; [a]
traverseBreadthFirst tree = go [tree]
    where
        go [] = []
        go xs = fmap bNodeVal xs ++ go (concatMap lrSubTrees xs)
        bNodeVal   (BNode v _ _)         = v
        lrSubTrees (BNode _ Empty Empty) = []
        lrSubTrees (BNode _ Empty b)     = [b]
        lrSubTrees (BNode _ a Empty)     = [a]
        lrSubTrees (BNode _ a b)         = [a,b]



traverseInOrder :: BTree a -&gt; [a]
traverseInOrder Empty         = []
traverseInOrder (BNode v l r) = traverseInOrder l ++ [v] ++ traverseInOrder r

traversePreOrder :: BTree a -&gt; [a]
traversePreOrder Empty         = []
traversePreOrder (BNode v l r) = v : traversePreOrder l ++ traversePreOrder r

traversePostOrder :: BTree a -&gt; [a]
traversePostOrder Empty         = []
traversePostOrder (BNode v l r) = traversePostOrder l ++ traversePostOrder r ++ [v]

depth :: BTree a -&gt; Int
depth Empty         = 0
depth (BNode _ l r) = 1 +  max (depth l) (depth r)

width :: BTree a -&gt; Int
width t = leftCount t + rightCount t where
    leftCount t = goLeft t 1 where
        goLeft (BNode _ Empty _) n = n
        goLeft (BNode _ l     _) n = goLeft l (n + 1)
    rightCount t = goRight t 1 where
        goRight (BNode _ _ Empty) n = n
        goRight (BNode _ _ r    ) n = goRight r (n + 1)

treeFromList :: (Ord a) =&gt; [a] -&gt; BTree a
treeFromList lst = go (sort lst) where
        go [] = Empty
        go xs = BNode (xs !! half)
                  (go $ take half xs)
                  (go $ drop (half + 1) xs)
                where
                  len = length xs
                  half = len `div` 2

insert :: (Eq a, Ord a) =&gt; BTree a -&gt; a -&gt; BTree a
insert Empty x = BNode x Empty Empty
insert (BNode a left right) x
    | x == a = BNode a left right
    | x &lt; a  = BNode a (insert left x) right
    | x &gt; a  = BNode a left (insert right x)
</pre>
And I think they're fairly simple and self-explanatory.

&nbsp;
<h4>Stern-Brocot Tree</h4>
Generation of the Stern-Brocot tree makes extensive use of the mediant of two fractions. The mediant is the 'wrong' way of adding two fractions. The numerators are added to become the new numerator and similarly for the denominator. i.e.

[latexpage]

\begin{align*}
\frac{a}{b} + \frac{c}{d} = \frac{a + c}{b + d}\\ \\
\end{align*}

Remembering our <em>Fraction</em> data type as <span class="lang:haskell decode:true crayon-inline ">data Fraction = F Integer Integer</span>
we can define a Monoid on <em>Fraction</em> simply as
<pre class="lang:haskell decode:true ">--
instance Monoid Fraction where
    mempty = F 0 0
    mappend (F a b) (F c d) = F (a + c) (b + d)</pre>
Trying this out in GHCI we have
<pre class="lang:haskell decode:true ">--
λ-&gt; (F 1 2) &lt;&gt; (F 3 4)
4/6
*Fractions
λ-&gt; (F 3 6) &lt;&gt; (F 1  2)
4/8</pre>
The details of how the Stern-Brocot tree numbers are generated is discussed in some detail <a href="https://www.cut-the-knot.org/blue/Stern.shtml">here</a>, <a href="https://en.wikipedia.org/wiki/Stern%E2%80%93Brocot_tree">here</a> and in Concrete Mathematics by Graham et al. In essence it is a matter of inserting mediants between successive pairs of fractions. To do that we recursively generate a binary tree using the fact that <em>Fraction</em> is a Monoid and that we combine Fractions (in this context) using the mediant function. To keep a reference to the 'left' and 'right' values the tree will have a triple of <em>Fraction</em> as the type. i.e.
<pre class="lang:haskell decode:true ">--
-- nearest left, value, nearest right
type Data = (Fraction, Fraction, Fraction)

buildBrocTree :: Int -&gt; BTree Data
buildBrocTree = build (BNode (F 0 1, F 1 1, F 1 0) Empty Empty) where
            build t 0  = t
            build (BNode nd@(fvw, fab, fxy) Empty Empty) n = build (BNode nd newLeft newRight) (n - 1) where
                         newLeft   = BNode (fvw, fvw &lt;&gt; fab, fab) Empty Empty
                         newRight  = BNode (fab, fab &lt;&gt; fxy, fxy) Empty Empty
            build (BNode nd l r) n = BNode nd (build l n) (build r n)
</pre>
The above builds the tree to a given depth. We could also have an infinite tree and take what we need from it. i.e.
<pre class="lang:haskell decode:true ">--
buildBrocTreeLazy :: BTree Data
buildBrocTreeLazy = build (BNode (F 0 1, F 1 1, F 1 0) Empty Empty) where
            build (BNode nd@(fvw, fab, fxy) Empty Empty) = build (BNode nd newLeft newRight) where
                        newLeft   = BNode (fvw, fvw &lt;&gt; fab, fab) Empty Empty
                        newRight  = BNode (fab, fab &lt;&gt; fxy, fxy) Empty Empty
            build (BNode nd l r)  = BNode nd (build l) (build r)
</pre>
Of course, in terms of types, what we really want is <span class="lang:haskell decode:true crayon-inline ">BTree Fraction</span> rather than <span class="lang:haskell decode:true crayon-inline ">BTree Data</span> and this is quite easy to do by fmapping over the tree and keeping only the middle <em>Fraction</em> from each triple. i.e.
<pre class="lang:haskell decode:true">--
-- match out the 'important' fraction.
fraction :: Data -&gt; Fraction
fraction (_, fr, _) = fr</pre>
and in GHCI
<pre class="lang:haskell decode:true ">--
λ-&gt; t = buildBrocTree 10
*RationalTrees
λ-&gt; :t t
t :: BTree Data
*RationalTrees
λ-&gt; tf = fmap fraction t
*RationalTrees
λ-&gt; :t tf
tf :: BTree Fraction</pre>
and we can also try one of the <em>traverseXXX</em> functions on binary trees - for example <span class="lang:haskell decode:true crayon-inline ">traversePreOrder tf</span> gives:

[1/1, 1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10, 1/11, 2/19, 2/17, 3/26, 3/25, 2/15, 3/23, 4/31, 5/38, 3/22, 5/37, 4/29, 2/13, 3/20, 4/27, 5/34, 7/47, 5/33, 8/53, 7/46, 3/19, 5/32, 7/45, 8/51, 4/25, 7/44, 5/31, 2/11, 3/17, 4/23, 5/29, 6/35, 9/52, 7/40, 11/63, 10/57, 5/28, 8/45, 11/62, 13/73, 7/39, 12/67, 9/50, 3/16, 5/27, 7/38, 9/49, 12/65, 8/43, 13/70, 11/59, 4/21, 7/37, 10/53, 11/58, 5/26, 9/47, 6/31, 2/9, 3/14, 4/19, 5/24, 6/29, 7/34, 11/53, 9/43, 14/67, 13/62, 7/33, 11/52, 15/71, 18/85, 10/47, 17/80, 13/61, 5/23, 8/37, 11/51, 14/65, 19/88, 13/60, 21/97, 18/83, 7/32, 12/55, 17/78, 19/87, 9/41, 16/73, 11/50, 3/13, 5/22, 7/31, 9/40, 11/49, 16/71, 12/53, 19/84, 17/75, 8/35, 13/57, 18/79, 21/92, 11/48, 19/83, 14/61, 4/17, 7/30, 10/43, 13/56, 17/73, 11/47, 18/77, 15/64, 5/21, 9/38, 13/55, 14/59, 6/25, 11/46, 7/29, 2/7, 3/11, 4/15, 5/19, 6/23, 7/27, 8/31, 13/50, 11/42, 17/65, 16/61, 9/34, 14/53, 19/72, 23/87, 13/49, 22/83, 17/64, 7/26, 11/41, 15/56, 19/71, 26/97, 18/67, 29/108, 25/93, 10/37, 17/63, 24/89, 27/100, 13/48, 23/85, 16/59, 5/18, 8/29, 11/40, 14/51, 17/62, 25/91, 19/69, 30/109, 27/98, 13/47, 21/76, 29/105, 34/123, 18/65, 31/112, 23/83, 7/25, 12/43, 17/61, 22/79, 29/104, 19/68, 31/111, 26/93, 9/32, 16/57, 23/82, 25/89, 11/39, 20/71, 13/46, 3/10, 5/17, 7/24, 9/31, 11/38, 13/45, 20/69, 16/55, 25/86, 23/79, 12/41, 19/65, 26/89, 31/106, 17/58, 29/99, 22/75, 8/27, 13/44, 18/61, 23/78, 31/105, 21/71, 34/115, 29/98, 11/37, 19/64, 27/91, 30/101, 14/47, 25/84, 17/57, 4/13, 7/23, 10/33, 13/43, 16/53, 23/76, 17/56, 27/89, 24/79, 11/36, 18/59, 25/82, 29/95, 15/49, 26/85, 19/62, 5/16, 9/29, 13/42, 17/55, 22/71, 14/45, 23/74, 19/61, 6/19, 11/35, 16/51, 17/54, 7/22, 13/41, 8/25, 2/5, 3/8, 4/11, 5/14, 6/17, 7/20, 8/23, 9/26, 15/43, 13/37, 20/57, 19/54, 11/31, 17/48, 23/65, 28/79, 16/45, 27/76, 21/59, 9/25, 14/39, 19/53, 24/67, 33/92, 23/64, 37/103, 32/89, 13/36, 22/61, 31/86, 35/97, 17/47, 30/83, 21/58, 7/19, 11/30, 15/41, 19/52, 23/63, 34/93, 26/71, 41/112, 37/101, 18/49, 29/79, 40/109, 47/128, 25/68, 43/117, 32/87, 10/27, 17/46, 24/65, 31/84, 41/111, 27/73, 44/119, 37/100, 13/35, 23/62, 33/89, 36/97, 16/43, 29/78, 19/51, 5/13, 8/21, 11/29, 14/37, 17/45, 20/53, 31/82, 25/66, 39/103, 36/95, 19/50, 30/79, 41/108, 49/129, 27/71, 46/121, 35/92, 13/34, 21/55, 29/76, 37/97, 50/131, 34/89, 55/144, 47/123, 18/47, 31/81, 44/115, 49/128, 23/60, 41/107, 28/73, 7/18, 12/31, 17/44, 22/57, 27/70, 39/101, 29/75, 46/119, 41/106, 19/49, 31/80, 43/111, 50/129, 26/67, 45/116, 33/85, 9/23, 16/41, 23/59, 30/77, 39/100, 25/64, 41/105, 34/87, 11/28, 20/51, 29/74, 31/79, 13/33, 24/61, 15/38, 3/7, 5/12, 7/17, 9/22, 11/27, 13/32, 15/37, 24/59, 20/49, 31/76, 29/71, 16/39, 25/61, 34/83, 41/100, 23/56, 39/95, 30/73, 12/29, 19/46, 26/63, 33/80, 45/109, 31/75, 50/121, 43/104, 17/41, 29/70, 41/99, 46/111, 22/53, 39/94, 27/65, 8/19, 13/31, 18/43, 23/55, 28/67, 41/98, 31/74, 49/117, 44/105, 21/50, 34/81, 47/112, 55/131, 29/69, 50/119, 37/88, 11/26, 19/45, 27/64, 35/83, 46/109, 30/71, 49/116, 41/97, 14/33, 25/59, 36/85, 39/92, 17/40, 31/73, 20/47, 4/9, 7/16, 10/23, 13/30, 16/37, 19/44, 29/67, 23/53, 36/83, 33/76, 17/39, 27/62, 37/85, 44/101, 24/55, 41/94, 31/71, 11/25, 18/41, 25/57, 32/73, 43/98, 29/66, 47/107, 40/91, 15/34, 26/59, 37/84, 41/93, 19/43, 34/77, 23/52, 5/11, 9/20, 13/29, 17/38, 21/47, 30/67, 22/49, 35/78, 31/69, 14/31, 23/51, 32/71, 37/82, 19/42, 33/73, 24/53, 6/13, 11/24, 16/35, 21/46, 27/59, 17/37, 28/61, 23/50, 7/15, 13/28, 19/41, 20/43, 8/17, 15/32, 9/19, 2/3, 3/5, 4/7, 5/9, 6/11, 7/13, 8/15, 9/17, 10/19, 17/32, 15/28, 23/43, 22/41, 13/24, 20/37, 27/50, 33/61, 19/35, 32/59, 25/46, 11/20, 17/31, 23/42, 29/53, 40/73, 28/51, 45/82, 39/71, 16/29, 27/49, 38/69, 43/78, 21/38, 37/67, 26/47, 9/16, 14/25, 19/34, 24/43, 29/52, 43/77, 33/59, 52/93, 47/84, 23/41, 37/66, 51/91, 60/107, 32/57, 55/98, 41/73, 13/23, 22/39, 31/55, 40/71, 53/94, 35/62, 57/101, 48/85, 17/30, 30/53, 43/76, 47/83, 21/37, 38/67, 25/44, 7/12, 11/19, 15/26, 19/33, 23/40, 27/47, 42/73, 34/59, 53/92, 49/85, 26/45, 41/71, 56/97, 67/116, 37/64, 63/109, 48/83, 18/31, 29/50, 40/69, 51/88, 69/119, 47/81, 76/131, 65/112, 25/43, 43/74, 61/105, 68/117, 32/55, 57/98, 39/67, 10/17, 17/29, 24/41, 31/53, 38/65, 55/94, 41/70, 65/111, 58/99, 27/46, 44/75, 61/104, 71/121, 37/63, 64/109, 47/80, 13/22, 23/39, 33/56, 43/73, 56/95, 36/61, 59/100, 49/83, 16/27, 29/49, 42/71, 45/76, 19/32, 35/59, 22/37, 5/8, 8/13, 11/18, 14/23, 17/28, 20/33, 23/38, 37/61, 31/51, 48/79, 45/74, 25/41, 39/64, 53/87, 64/105, 36/59, 61/100, 47/77, 19/31, 30/49, 41/67, 52/85, 71/116, 49/80, 79/129, 68/111, 27/44, 46/75, 65/106, 73/119, 35/57, 62/101, 43/70, 13/21, 21/34, 29/47, 37/60, 45/73, 66/107, 50/81, 79/128, 71/115, 34/55, 55/89, 76/123, 89/144, 47/76, 81/131, 60/97, 18/29, 31/50, 44/71, 57/92, 75/121, 49/79, 80/129, 67/108, 23/37, 41/66, 59/95, 64/103, 28/45, 51/82, 33/53, 7/11, 12/19, 17/27, 22/35, 27/43, 32/51, 49/78, 39/62, 61/97, 56/89, 29/46, 46/73, 63/100, 75/119, 41/65, 70/111, 53/84, 19/30, 31/49, 43/68, 55/87, 74/117, 50/79, 81/128, 69/109, 26/41, 45/71, 64/101, 71/112, 33/52, 59/93, 40/63, 9/14, 16/25, 23/36, 30/47, 37/58, 53/83, 39/61, 62/97, 55/86, 25/39, 41/64, 57/89, 66/103, 34/53, 59/92, 43/67, 11/17, 20/31, 29/45, 38/59, 49/76, 31/48, 51/79, 42/65, 13/20, 24/37, 35/54, 37/57, 15/23, 28/43, 17/26, 3/4, 5/7, 7/10, 9/13, 11/16, 13/19, 15/22, 17/25, 28/41, 24/35, 37/54, 35/51, 20/29, 31/45, 42/61, 51/74, 29/42, 49/71, 38/55, 16/23, 25/36, 34/49, 43/62, 59/85, 41/59, 66/95, 57/82, 23/33, 39/56, 55/79, 62/89, 30/43, 53/76, 37/53, 12/17, 19/27, 26/37, 33/47, 40/57, 59/84, 45/64, 71/101, 64/91, 31/44, 50/71, 69/98, 81/115, 43/61, 74/105, 55/78, 17/24, 29/41, 41/58, 53/75, 70/99, 46/65, 75/106, 63/89, 22/31, 39/55, 56/79, 61/86, 27/38, 49/69, 32/45, 8/11, 13/18, 18/25, 23/32, 28/39, 33/46, 51/71, 41/57, 64/89, 59/82, 31/43, 49/68, 67/93, 80/111, 44/61, 75/104, 57/79, 21/29, 34/47, 47/65, 60/83, 81/112, 55/76, 89/123, 76/105, 29/40, 50/69, 71/98, 79/109, 37/51, 66/91, 45/62, 11/15, 19/26, 27/37, 35/48, 43/59, 62/85, 46/63, 73/100, 65/89, 30/41, 49/67, 68/93, 79/108, 41/56, 71/97, 52/71, 14/19, 25/34, 36/49, 47/64, 61/83, 39/53, 64/87, 53/72, 17/23, 31/42, 45/61, 48/65, 20/27, 37/50, 23/31, 4/5, 7/9, 10/13, 13/17, 16/21, 19/25, 22/29, 35/46, 29/38, 45/59, 42/55, 23/30, 36/47, 49/64, 59/77, 33/43, 56/73, 43/56, 17/22, 27/35, 37/48, 47/61, 64/83, 44/57, 71/92, 61/79, 24/31, 41/53, 58/75, 65/84, 31/40, 55/71, 38/49, 11/14, 18/23, 25/32, 32/41, 39/50, 57/73, 43/55, 68/87, 61/78, 29/37, 47/60, 65/83, 76/97, 40/51, 69/88, 51/65, 15/19, 26/33, 37/47, 48/61, 63/80, 41/52, 67/85, 56/71, 19/24, 34/43, 49/62, 53/67, 23/29, 42/53, 27/34, 5/6, 9/11, 13/16, 17/21, 21/26, 25/31, 38/47, 30/37, 47/58, 43/53, 22/27, 35/43, 48/59, 57/70, 31/38, 53/65, 40/49, 14/17, 23/28, 32/39, 41/50, 55/67, 37/45, 60/73, 51/62, 19/23, 33/40, 47/57, 52/63, 24/29, 43/52, 29/35, 6/7, 11/13, 16/19, 21/25, 26/31, 37/44, 27/32, 43/51, 38/45, 17/20, 28/33, 39/46, 45/53, 23/27, 40/47, 29/34, 7/8, 13/15, 19/22, 25/29, 32/37, 20/23, 33/38, 27/31, 8/9, 15/17, 22/25, 23/26, 9/10, 17/19, 10/11, 2/1, 3/2, 4/3, 5/4, 6/5, 7/6, 8/7, 9/8, 10/9, 11/10, 19/17, 17/15, 26/23, 25/22, 15/13, 23/20, 31/27, 38/33, 22/19, 37/32, 29/25, 13/11, 20/17, 27/23, 34/29, 47/40, 33/28, 53/45, 46/39, 19/16, 32/27, 45/38, 51/43, 25/21, 44/37, 31/26, 11/9, 17/14, 23/19, 29/24, 35/29, 52/43, 40/33, 63/52, 57/47, 28/23, 45/37, 62/51, 73/60, 39/32, 67/55, 50/41, 16/13, 27/22, 38/31, 49/40, 65/53, 43/35, 70/57, 59/48, 21/17, 37/30, 53/43, 58/47, 26/21, 47/38, 31/25, 9/7, 14/11, 19/15, 24/19, 29/23, 34/27, 53/42, 43/34, 67/53, 62/49, 33/26, 52/41, 71/56, 85/67, 47/37, 80/63, 61/48, 23/18, 37/29, 51/40, 65/51, 88/69, 60/47, 97/76, 83/65, 32/25, 55/43, 78/61, 87/68, 41/32, 73/57, 50/39, 13/10, 22/17, 31/24, 40/31, 49/38, 71/55, 53/41, 84/65, 75/58, 35/27, 57/44, 79/61, 92/71, 48/37, 83/64, 61/47, 17/13, 30/23, 43/33, 56/43, 73/56, 47/36, 77/59, 64/49, 21/16, 38/29, 55/42, 59/45, 25/19, 46/35, 29/22, 7/5, 11/8, 15/11, 19/14, 23/17, 27/20, 31/23, 50/37, 42/31, 65/48, 61/45, 34/25, 53/39, 72/53, 87/64, 49/36, 83/61, 64/47, 26/19, 41/30, 56/41, 71/52, 97/71, 67/49, 108/79, 93/68, 37/27, 63/46, 89/65, 100/73, 48/35, 85/62, 59/43, 18/13, 29/21, 40/29, 51/37, 62/45, 91/66, 69/50, 109/79, 98/71, 47/34, 76/55, 105/76, 123/89, 65/47, 112/81, 83/60, 25/18, 43/31, 61/44, 79/57, 104/75, 68/49, 111/80, 93/67, 32/23, 57/41, 82/59, 89/64, 39/28, 71/51, 46/33, 10/7, 17/12, 24/17, 31/22, 38/27, 45/32, 69/49, 55/39, 86/61, 79/56, 41/29, 65/46, 89/63, 106/75, 58/41, 99/70, 75/53, 27/19, 44/31, 61/43, 78/55, 105/74, 71/50, 115/81, 98/69, 37/26, 64/45, 91/64, 101/71, 47/33, 84/59, 57/40, 13/9, 23/16, 33/23, 43/30, 53/37, 76/53, 56/39, 89/62, 79/55, 36/25, 59/41, 82/57, 95/66, 49/34, 85/59, 62/43, 16/11, 29/20, 42/29, 55/38, 71/49, 45/31, 74/51, 61/42, 19/13, 35/24, 51/35, 54/37, 22/15, 41/28, 25/17, 5/3, 8/5, 11/7, 14/9, 17/11, 20/13, 23/15, 26/17, 43/28, 37/24, 57/37, 54/35, 31/20, 48/31, 65/42, 79/51, 45/29, 76/49, 59/38, 25/16, 39/25, 53/34, 67/43, 92/59, 64/41, 103/66, 89/57, 36/23, 61/39, 86/55, 97/62, 47/30, 83/53, 58/37, 19/12, 30/19, 41/26, 52/33, 63/40, 93/59, 71/45, 112/71, 101/64, 49/31, 79/50, 109/69, 128/81, 68/43, 117/74, 87/55, 27/17, 46/29, 65/41, 84/53, 111/70, 73/46, 119/75, 100/63, 35/22, 62/39, 89/56, 97/61, 43/27, 78/49, 51/32, 13/8, 21/13, 29/18, 37/23, 45/28, 53/33, 82/51, 66/41, 103/64, 95/59, 50/31, 79/49, 108/67, 129/80, 71/44, 121/75, 92/57, 34/21, 55/34, 76/47, 97/60, 131/81, 89/55, 144/89, 123/76, 47/29, 81/50, 115/71, 128/79, 60/37, 107/66, 73/45, 18/11, 31/19, 44/27, 57/35, 70/43, 101/62, 75/46, 119/73, 106/65, 49/30, 80/49, 111/68, 129/79, 67/41, 116/71, 85/52, 23/14, 41/25, 59/36, 77/47, 100/61, 64/39, 105/64, 87/53, 28/17, 51/31, 74/45, 79/48, 33/20, 61/37, 38/23, 7/4, 12/7, 17/10, 22/13, 27/16, 32/19, 37/22, 59/35, 49/29, 76/45, 71/42, 39/23, 61/36, 83/49, 100/59, 56/33, 95/56, 73/43, 29/17, 46/27, 63/37, 80/47, 109/64, 75/44, 121/71, 104/61, 41/24, 70/41, 99/58, 111/65, 53/31, 94/55, 65/38, 19/11, 31/18, 43/25, 55/32, 67/39, 98/57, 74/43, 117/68, 105/61, 50/29, 81/47, 112/65, 131/76, 69/40, 119/69, 88/51, 26/15, 45/26, 64/37, 83/48, 109/63, 71/41, 116/67, 97/56, 33/19, 59/34, 85/49, 92/53, 40/23, 73/42, 47/27, 9/5, 16/9, 23/13, 30/17, 37/21, 44/25, 67/38, 53/30, 83/47, 76/43, 39/22, 62/35, 85/48, 101/57, 55/31, 94/53, 71/40, 25/14, 41/23, 57/32, 73/41, 98/55, 66/37, 107/60, 91/51, 34/19, 59/33, 84/47, 93/52, 43/24, 77/43, 52/29, 11/6, 20/11, 29/16, 38/21, 47/26, 67/37, 49/27, 78/43, 69/38, 31/17, 51/28, 71/39, 82/45, 42/23, 73/40, 53/29, 13/7, 24/13, 35/19, 46/25, 59/32, 37/20, 61/33, 50/27, 15/8, 28/15, 41/22, 43/23, 17/9, 32/17, 19/10, 3/1, 5/2, 7/3, 9/4, 11/5, 13/6, 15/7, 17/8, 19/9, 32/15, 28/13, 43/20, 41/19, 24/11, 37/17, 50/23, 61/28, 35/16, 59/27, 46/21, 20/9, 31/14, 42/19, 53/24, 73/33, 51/23, 82/37, 71/32, 29/13, 49/22, 69/31, 78/35, 38/17, 67/30, 47/21, 16/7, 25/11, 34/15, 43/19, 52/23, 77/34, 59/26, 93/41, 84/37, 41/18, 66/29, 91/40, 107/47, 57/25, 98/43, 73/32, 23/10, 39/17, 55/24, 71/31, 94/41, 62/27, 101/44, 85/37, 30/13, 53/23, 76/33, 83/36, 37/16, 67/29, 44/19, 12/5, 19/8, 26/11, 33/14, 40/17, 47/20, 73/31, 59/25, 92/39, 85/36, 45/19, 71/30, 97/41, 116/49, 64/27, 109/46, 83/35, 31/13, 50/21, 69/29, 88/37, 119/50, 81/34, 131/55, 112/47, 43/18, 74/31, 105/44, 117/49, 55/23, 98/41, 67/28, 17/7, 29/12, 41/17, 53/22, 65/27, 94/39, 70/29, 111/46, 99/41, 46/19, 75/31, 104/43, 121/50, 63/26, 109/45, 80/33, 22/9, 39/16, 56/23, 73/30, 95/39, 61/25, 100/41, 83/34, 27/11, 49/20, 71/29, 76/31, 32/13, 59/24, 37/15, 8/3, 13/5, 18/7, 23/9, 28/11, 33/13, 38/15, 61/24, 51/20, 79/31, 74/29, 41/16, 64/25, 87/34, 105/41, 59/23, 100/39, 77/30, 31/12, 49/19, 67/26, 85/33, 116/45, 80/31, 129/50, 111/43, 44/17, 75/29, 106/41, 119/46, 57/22, 101/39, 70/27, 21/8, 34/13, 47/18, 60/23, 73/28, 107/41, 81/31, 128/49, 115/44, 55/21, 89/34, 123/47, 144/55, 76/29, 131/50, 97/37, 29/11, 50/19, 71/27, 92/35, 121/46, 79/30, 129/49, 108/41, 37/14, 66/25, 95/36, 103/39, 45/17, 82/31, 53/20, 11/4, 19/7, 27/10, 35/13, 43/16, 51/19, 78/29, 62/23, 97/36, 89/33, 46/17, 73/27, 100/37, 119/44, 65/24, 111/41, 84/31, 30/11, 49/18, 68/25, 87/32, 117/43, 79/29, 128/47, 109/40, 41/15, 71/26, 101/37, 112/41, 52/19, 93/34, 63/23, 14/5, 25/9, 36/13, 47/17, 58/21, 83/30, 61/22, 97/35, 86/31, 39/14, 64/23, 89/32, 103/37, 53/19, 92/33, 67/24, 17/6, 31/11, 45/16, 59/21, 76/27, 48/17, 79/28, 65/23, 20/7, 37/13, 54/19, 57/20, 23/8, 43/15, 26/9, 4/1, 7/2, 10/3, 13/4, 16/5, 19/6, 22/7, 25/8, 41/13, 35/11, 54/17, 51/16, 29/9, 45/14, 61/19, 74/23, 42/13, 71/22, 55/17, 23/7, 36/11, 49/15, 62/19, 85/26, 59/18, 95/29, 82/25, 33/10, 56/17, 79/24, 89/27, 43/13, 76/23, 53/16, 17/5, 27/8, 37/11, 47/14, 57/17, 84/25, 64/19, 101/30, 91/27, 44/13, 71/21, 98/29, 115/34, 61/18, 105/31, 78/23, 24/7, 41/12, 58/17, 75/22, 99/29, 65/19, 106/31, 89/26, 31/9, 55/16, 79/23, 86/25, 38/11, 69/20, 45/13, 11/3, 18/5, 25/7, 32/9, 39/11, 46/13, 71/20, 57/16, 89/25, 82/23, 43/12, 68/19, 93/26, 111/31, 61/17, 104/29, 79/22, 29/8, 47/13, 65/18, 83/23, 112/31, 76/21, 123/34, 105/29, 40/11, 69/19, 98/27, 109/30, 51/14, 91/25, 62/17, 15/4, 26/7, 37/10, 48/13, 59/16, 85/23, 63/17, 100/27, 89/24, 41/11, 67/18, 93/25, 108/29, 56/15, 97/26, 71/19, 19/5, 34/9, 49/13, 64/17, 83/22, 53/14, 87/23, 72/19, 23/6, 42/11, 61/16, 65/17, 27/7, 50/13, 31/8, 5/1, 9/2, 13/3, 17/4, 21/5, 25/6, 29/7, 46/11, 38/9, 59/14, 55/13, 30/7, 47/11, 64/15, 77/18, 43/10, 73/17, 56/13, 22/5, 35/8, 48/11, 61/14, 83/19, 57/13, 92/21, 79/18, 31/7, 53/12, 75/17, 84/19, 40/9, 71/16, 49/11, 14/3, 23/5, 32/7, 41/9, 50/11, 73/16, 55/12, 87/19, 78/17, 37/8, 60/13, 83/18, 97/21, 51/11, 88/19, 65/14, 19/4, 33/7, 47/10, 61/13, 80/17, 52/11, 85/18, 71/15, 24/5, 43/9, 62/13, 67/14, 29/6, 53/11, 34/7, 6/1, 11/2, 16/3, 21/4, 26/5, 31/6, 47/9, 37/7, 58/11, 53/10, 27/5, 43/8, 59/11, 70/13, 38/7, 65/12, 49/9, 17/3, 28/5, 39/7, 50/9, 67/12, 45/8, 73/13, 62/11, 23/4, 40/7, 57/10, 63/11, 29/5, 52/9, 35/6, 7/1, 13/2, 19/3, 25/4, 31/5, 44/7, 32/5, 51/8, 45/7, 20/3, 33/5, 46/7, 53/8, 27/4, 47/7, 34/5, 8/1, 15/2, 22/3, 29/4, 37/5, 23/3, 38/5, 31/4, 9/1, 17/2, 25/3, 26/3, 10/1, 19/2, 11/1]

Of course, a list of fractions like this is OK but with a bit of work the <em>BTree Fraction</em> can be rendered graphically.
<h4>GoJS</h4>
At the heart of rendering using Javascript/HTML is the notion of a <em>TreeModel</em> and <em>TreeLayout</em> as described in the GoJS <a href="https://gojs.net/latest/learn/index.html">documentation</a>.

The Haskell code to handle the use of GoJS is here:
<pre class="lang:haskell decode:true">--
{-# LANGUAGE QuasiQuotes #-}
module JScript where
import           BinaryTrees
import           Fractions
import           RationalTrees
import           Text.RawString.QQ

topPage :: String
topPage = [r|
    &lt;html&gt;
    &lt;head&gt;
      &lt;script src="https://cdnjs.cloudflare.com/ajax/libs/gojs/1.8.21/go-debug.js"&gt;&lt;/script&gt;
      &lt;script id="code"&gt;
      function init() {
        var $ = go.GraphObject.make;
        myDiagram = $(go.Diagram, "treeDiv",
        {
          initialAutoScale: go.Diagram.UniformToFill,
          layout: $(go.TreeLayout, { nodeSpacing: 5, layerSpacing: 30 })
        });
        var model = $(go.TreeModel);
        model.nodeDataArray =
        [
 |]

bottomPage :: String
bottomPage = [r|
        ];
        myDiagram.model = model;
      }
    &lt;/script&gt;
   &lt;/head&gt;
   &lt;body onload="init()"&gt;
   &lt;div id="tree"&gt;
   &lt;!-- The DIV for the Diagram needs an explicit size or else we won't see anything.
        This also adds a border to help see the edges of the viewport. --&gt;
   &lt;div id="treeDiv" style="border: solid 1px black; width:100%; height:100%"&gt;&lt;/div&gt;

 &lt;/div&gt;
&lt;/body&gt;
&lt;/html&gt;
|]

fullPage :: (Show a) =&gt; BTree a -&gt;  String
fullPage tr = concat [topPage, goJSModel . toNodeParentList $ tr, bottomPage]

-- a given node might/might not have children.
toNodeParentList :: BTree a -&gt; [(a, Maybe a, Maybe a)]
toNodeParentList = foldBTreeNodes f where
        f v Empty          Empty          = (v, Nothing, Nothing)
        f v Empty          (BNode vr _ _) = (v, Nothing, Just vr)
        f v (BNode vl _ _) Empty          = (v, Just vl, Nothing)
        f v (BNode vl _ _) (BNode vr _ _) = (v, Just vl, Just vr)

shw :: (Show a) =&gt; a -&gt; String
shw x = "'" ++ show x ++ "'"

goJSModel :: (Show a) =&gt; [(a, Maybe a, Maybe a)] -&gt; String
goJSModel xs@((r, _, _):_) = foldr f (concat["{key:", shw r, "}"]) xs  where
    f  (w, Just x,  Just y)  ac =
        concat [ "{key:", shw x, ",parent:", shw w,"},",
        "{key:", shw y, ",parent:", shw w,"},", ac ]
    f  (w, Nothing, Just y)  ac =
        concat ["{key:", shw y, ",parent:", shw w,"},", ac ]
    f  (w, Just x,  Nothing) ac =
        concat [ "{key:", shw x, ",parent:", shw w,"},", "{key:", shw x,"}", ac ]
    f  (w, Nothing, Nothing) ac = ac

makeFractionTreeHTML :: FilePath -&gt; BTree Fraction -&gt; IO ()
makeFractionTreeHTML fname tr = writeFile fname (fullPage tr)

</pre>
It uses QuasiQuotes to make the handling of  text/html etc. simpler. The function <em>fullPage</em> combines the html head preamble with a  GoJs compliant data model, expressed as a string, and then adds the body of the html along with the needed closing tags. The end result is a string of HTML and Javascript that will render the tree. The function <span class="lang:haskell decode:true crayon-inline ">toNodeParentList :: BTree a -&gt; [(a, Maybe a, Maybe a)]</span>  morphs a <em>BTree</em> into a list of tuples that can be further processed to create data that is specific to GoJS. i.e. it is Javascript that contains parent-child relationships like this...
<div>
<div><strong>{key:'1/2',parent:'1/1'},{key:'2/1',parent:'1/1'},{key:'1/3',parent:'1/2'}</strong>... etc.</div>
<div>and finally, to generate the page we have the function <span class="lang:haskell decode:true crayon-inline ">makeFractionTreeHTML :: FilePath -&gt; BTree Fraction -&gt; IO ()</span></div>
<div></div>
</div>
We can now compose a few functions to generate the HTML/JS for a small Stern- Brocot tree.

<span class="lang:haskell decode:true crayon-inline">makeFractionTreeHTML "tf3.html" . fmap fraction . buildBrocTree $ 3</span>

which generates
<pre class="lang:xhtml decode:true ">    &lt;html&gt;
    &lt;head&gt;
      &lt;script src="https://cdnjs.cloudflare.com/ajax/libs/gojs/1.8.21/go-debug.js"&gt;&lt;/script&gt;
      &lt;script id="code"&gt;
      function init() {
        var $ = go.GraphObject.make;
        myDiagram = $(go.Diagram, "treeDiv",
        {
          initialAutoScale: go.Diagram.UniformToFill,
          layout: $(go.TreeLayout, { nodeSpacing: 5, layerSpacing: 30 })
        });
        var model = $(go.TreeModel);
        model.nodeDataArray =
        [
 {key:'1/2',parent:'1/1'},{key:'2/1',parent:'1/1'},{key:'1/3',parent:'1/2'},
 {key:'2/3',parent:'1/2'},{key:'1/4',parent:'1/3'},{key:'2/5',parent:'1/3'},
 {key:'3/5',parent:'2/3'},{key:'3/4',parent:'2/3'},{key:'3/2',parent:'2/1'},
 {key:'3/1',parent:'2/1'},{key:'4/3',parent:'3/2'},{key:'5/3',parent:'3/2'},
 {key:'5/2',parent:'3/1'},{key:'4/1',parent:'3/1'},{key:'1/1'}
        ];
        myDiagram.model = model;
      }
    &lt;/script&gt;
   &lt;/head&gt;
   &lt;body onload="init()"&gt;
   &lt;div id="tree"&gt;
   &lt;!-- The DIV for the Diagram needs an explicit size or else we won't see anything.
        This also adds a border to help see the edges of the viewport. --&gt;
   &lt;div id="treeDiv" style="border: solid 1px black; width:100%; height:100%"&gt;&lt;/div&gt;

 &lt;/div&gt;
&lt;/body&gt;
&lt;/html&gt;
</pre>
If we now open the above HTML we see a small Stern-Brocot tree.
<img class="aligncenter size-full wp-image-1645" src="http://gitcommit.co.uk/wp-content/uploads/2018/07/Screen-Shot-2018-07-16-at-10.48.53.png" alt="" width="536" height="518" />
Which is just a smaller version of the tree shown at the start of this post. <a href="http://gitcommit.co.uk/wp-content/uploads/2018/07/test.html">SBTree</a>

All the code is in <a href="https://github.com/banditpig/Farey/tree/monoidFractions">Github</a> and thanks for reading!