---
ID: 1589
post_title: A Stern View.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/06/16/a-stern-view/
published: true
post_date: 2018-06-16 09:36:07
---
[latexpage]

<img class="size-full wp-image-317 alignleft" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />

&nbsp;

&nbsp;

We all know the Fibonacci sequence and how it is generated...

1, 1, 2, 3, 5, 8,...

We just add the 'previous' pair to get the next number and then move along to the next pair.
An interesting variation on this is to add adjacent pairs and write down their sum as before but, after appending the sum, also append the second digit of the pair just added and this gives...

1, 1, 2, 1, 3, 2, 3, 1 ...

Starting with 1, 1

1 + 1 -&gt;  2 - append 2 and then copy forward 1
1 + 2 -&gt; 3 - append 3 and then copy forward 2
2 + 1 -&gt; 3 - append 3 and then copy forward 1
etc.
This sequence gets longer and longer whilst the 'processing' lags behind! Each step of the production creates two terms and moves along by just one term.

The sequence is the so called Stern-Brocot sequence (<a href="https://oeis.org/A002487">A002487</a>) and the sequences can be used, quite easily, to generate all rational numbers in their simplest form (i.e. the numerator and denominator are co-prime).
So, first some Haskell showing three alternative ways of generating the sequence. The first way will be  a brutal list manipulation with a final reverse at the end. The second function will use a recurrence relation to calculate the terms. The final one (and my favourite) will recursively stitch together two lists - the second being the tail of the first one.
<h4>Stern One</h4>
This one is the least elegant and requires an integer indicating how many terms to generate.
<pre class="lang:haskell decode:true ">--
stern1 :: Integer -&gt; [Integer]
stern1 n = reverse ns where
    (_, _, ns)   = head . dropWhile f . iterate update $ (0, 1, [1,1])
    f (_, _, xs) = (toInteger . length $ xs) &lt;= n
    update (ia, ib, xs) = (ia + 1, ib + 1, b:a + b:xs) where
        len = length xs - 1
        a = xs !! (len - ia)
        b = xs !! (len - ib)</pre>
Here the key function is <em>update</em> which uses two indices to determine which are the list elements to add together and which one to copy forward. This is the snippet <span class="lang:haskell decode:true crayon-inline "> b:a + b:xs</span> that creates each new term.

Sample output:
<pre class="lang:haskell decode:true ">--
λ-&gt; stern1 25
[1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8,5,7,2,7,5]</pre>
The next implementation is somewhat neater and uses a recurrence relation defined in  (<a href="https://oeis.org/A002487">A002487</a>)
<h4>Stern Two</h4>
The recurrence equations are:
<div>$a_0 = 0$</div>
<div>$a_1  = 1$</div>
<div>$When\  n &gt; 0\  then\  a(2n) = a(n), \   a(2n+1) = a(n) + a(n+1)$</div>
<div>which converts quite easily to</div>
<pre class="lang:haskell decode:true">--
stern2 :: [Integer]
stern2 = [next n | n &lt;- [1..]] where
    next :: Integer -&gt; Integer
    next n
        | n &lt; 2 = n
        | even n = next (n `div` 2)
        | otherwise = next n' + next (n' + 1) where
            n' = (n - 1) `div` 2</pre>
the final implementation is, I think, quite elegant and encapsulates what I truly enjoy about Haskell.
<h4>Stern Three</h4>
<pre class="lang:haskell decode:true ">--
stern3 :: [Integer]
stern3 = 1 : 1 : knit (tail stern3) stern3
  where
    knit (a:as) (b:bs) = a + b : a : knit as bs
</pre>
The similarity between <em>stern3</em> and the canonical Haskell implementation of the Fibonacci sequence,
<span class="lang:haskell decode:true crayon-inline ">fibs = 0 : 1 : zipWith (+) fibs (tail fibs)</span> is not entirely coincidental :)

We now have several ways of generating 1, 1, 2, 1, 3, 2, 3, 1, 4, 3, 5, 2, 5, 3, 4, 1, 5, 4, 7, 3, 8, 5, 7... Now,  using the sequence,  we can generate fractions by taking elements by pairs <em>(a, b)</em> setting the numerator to <em>a</em>, the denominator to <em>b</em> and then moving along by one. i.e
1/1, 1/2, 2/1, 1/3, 3/2... and the interesting thing is that each fraction appears in its lowest form and appears once only and that all (positive) fractions will be in the list. This leads to the next section entitled:
<h4>Stern Fractions</h4>
Here's a first pass at generating fractions. (Note that the Fraction type was introduced in the post <a href="http://gitcommit.co.uk/2018/05/26/count-the-fractions/">Count the Fractions</a> )
<pre class="lang:haskell decode:true ">--
fracs1 :: [Integer] -&gt; [Fraction]
fracs1 []       = []
fracs1 [p,q]    = [F p q]
fracs1 (p:q:ns) = F p q : fracs1 (q:ns)</pre>
and we can combine it with one of the <em>stern</em> functions to get
<pre class="lang:haskell decode:true ">--
λ-&gt; take 30 . fracs1 $  stern3
[1/1,1/2,2/1,1/3,3/2,2/3,3/1,1/4,4/3,3/5,5/2,2/5,5/3,3/4,4/1,1/5,5/4,4/7,7/3,3/8,8/5,5/7,7/2,2/7,7/5,5/8,8/3,3/7,7/4,4/5]</pre>
and an alternative <em>fracs</em> function is
<pre class="lang:haskell decode:true">--
fracs2 :: [Integer] -&gt; [Fraction]
fracs2 xs = fromTuple &lt;$&gt; (zip &lt;*&gt; tail $ xs) where fromTuple (p, q) = F p q
</pre>
and <span class="lang:haskell decode:true crayon-inline ">zip &lt;*&gt; tail</span>  is quite a useful idiom that sort of enables fmap-ing using pairs of items from a list. For example

<span class="lang:haskell decode:true crayon-inline">λ-&gt; zip &lt;*&gt; tail $ ([1,2,3,4,5,6]::[Int]) gives  [(1,2),(2,3),(3,4),(4,5),(5,6)]</span>

Typically the  fractions just generated are viewed in a tree  know as the '<a href="https://en.wikipedia.org/wiki/Stern%E2%80%93Brocot_tree">Stern-Brocot Tree</a>'. Creating such a tree and displaying it will be done in detail in the next post.  In the meantime here is an example of <a href="http://gitcommit.co.uk/wp-content/uploads/2018/06/tree.html">such a tree,</a>  rendered using  <a href="http://hackage.haskell.org/package/tree-view-0.5/docs/Data-Tree-View.html#t:Behavior">tree-view</a>,  note that it is shown collapsed initially, just click on the fractions to expand/collapse them.

All the code is in <a href="https://github.com/banditpig/Farey/tree/monoidFractions">Github</a> and thanks for reading!

&nbsp;