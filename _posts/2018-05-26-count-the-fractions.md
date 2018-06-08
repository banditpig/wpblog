---
ID: 1421
post_title: Count the Fractions.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/05/26/count-the-fractions/
published: true
post_date: 2018-05-26 07:55:57
---
[latexpage]
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />This is the first in a series of posts about sequences of fractions, circles, trees of fractions, binary search trees and ways of representing rational numbers as paths through these trees . The idea is to enjoy a bit of recreational mathematics and to use Haskell to express some of the notions in its own succinct  way.
<h3 style="text-align: center;">One Lot Of Fractions</h3>
First we'll look at all unique and simplified fractions between 0 and 1 with a denominator no larger than a given value. For example, with a maximum denominator of 3, 4 and then 5 we have

\begin{align*}
\(fracs(3) = \frac{1}{3}, \frac{1}{2}, \frac{2}{3} \\ \\
\(fracs(4) = \frac{1}{4}, \frac{1}{3}, \frac{1}{2}, \frac{2}{3}, \frac{3}{4}\\ \\
\(fracs(5) = \frac{1}{5}, \frac{1}{4}, \frac{1}{3}, \frac{2}{5}, \frac{1}{2}, \frac{3}{5}, \frac{2}{3}, \frac{3}{4}, \frac{4}{5}\\ \\
\end{align*}

For completeness and a pleasing symmetry we can regard each sequence as starting with $\frac{0}{1}$ and ending with $\frac{1}{1}$.  Such a  sequences are known as the Farey sequences of  given denominator or order. i.e. The Farey sequence of order N is the sequence with N as the maximum denominator. Let's look at alternative ways to compute these fractions.
<h3 style="text-align: center;">Four Lots of Haskell</h3>
To start writing Haskell functions we first create a suitable data type for fractions - very much like the type that allowed for <a href="http://gitcommit.co.uk/2017/11/16/fractions-to-phi-to-fibonacci/">continued fractions</a> but somewhat simpler. A fraction is just the ratio of two integers:
<pre class="lang:haskell decode:true">--
data Fraction  = F Integer Integer
instance Show Fraction where show (F n d) = show n ++ "/" ++ show d

instance Eq Fraction where
    (==) f1@(F m n) f2@(F p q) = m' == p' &amp;&amp; n' == q' where
        F m' n' = reduce f1
        F p' q' = reduce f2

instance Ord Fraction where
    (&lt;=) (F m n ) (F p q) =  m * q &lt;= n * p
    (&lt;)  (F m n ) (F p q) =  m * q &lt; n * p

reduce :: Fraction -&gt; Fraction
reduce (F p q) = F p' q'
    where p' = p `div` gDiv
          q' = q `div` gDiv
          gDiv = gcd p q</pre>
and I've added a few utility instances and a function to simplify a fraction. This allows us to create fractions and reduce them. e.g.
<pre class="lang:haskell decode:true">--
λ-&gt; f = F 12 5
*Farey
λ-&gt; reduce f
12/5
*Farey
λ-&gt; f = F 12 4
*Farey
λ-&gt; reduce f
3/1
*Farey
λ-&gt; f = F 4 12
*Farey
λ-&gt; reduce f
1/3</pre>
A brute force way of calculating a Farey sequence of order N is just to enumerate all the possible combinations, remove duplicates and sort them... like this.
<h4 style="text-align: center;">Farey One</h4>
<pre class="lang:haskell decode:true ">--
farey1 :: Integer -&gt; [Fraction]
farey1 n = F 0 1 :  mid ++  [F 1 1] where
    mid = nub . sort $ [ reduce ( F p q) | p &lt;- [1..n], q &lt;- [1..n], p &lt; q]
</pre>
running some examples in ghci we get
<pre class="lang:haskell decode:true">--
λ-&gt; farey1 3
[0/1,1/3,1/2,2/3,1/1]
*Farey
λ-&gt; farey1 4
[0/1,1/4,1/3,1/2,2/3,3/4,1/1]
*Farey
λ-&gt; farey1 5
[0/1,1/5,1/4,1/3,2/5,1/2,3/5,2/3,3/4,4/5,1/1]</pre>
As can be imagined this function is not particularly efficient. Enabling timing in ghci and we get these quite long calculation times.
<pre class="lang:haskell decode:true ">--
λ-&gt; :set +s
*Farey
λ-&gt; last . farey1 $ 100
1/1
(7.63 secs, 7,370,328,840 bytes)
*Farey
λ-&gt; last . farey1 $ 200
1/1
(134.53 secs, 123,142,301,896 bytes)</pre>
There are more efficient ways of calculating these sequences.
<h4 style="text-align: center;">Farey Two</h4>
One way is to take a sequence of order N (i.e. the largest denominator is N) and repeatedly compare adjacent fractions and, whenever the denominators added together give N + 1, insert between the adjacent fractions a new fraction with denominator (N + 1) and numerator equal to the sum of the adjacent fractions numerators.
<pre class="lang:haskell decode:true">--
maxDenom :: [Fraction] -&gt; Integer
maxDenom  = foldr f 1 where f (F _ q) = max q

-- produce next seq given the previous seq.
fareyFromPreviousSeq :: [Fraction] -&gt; [Fraction]
fareyFromPreviousSeq fs = proc fs (1 + maxDenom fs)  where
    proc [f] _ = [f]
    proc (F m n :F m' n':fs) l = if n + n' == l
                                    then
                                        F m n : F (m + m') l : proc (F m' n': fs) l
                                    else
                                        F m n : proc (F m' n':fs) l
</pre>
The <em>maxDenom</em> is just a right fold, initialised at 1, that takes the largest denominator on each step. The function <em>fareyFromPreviousSeq</em> takes an existing sequence and 'consumes' it from the left using fractions from the supplied sequence and inserting new fractions when the insertion condition is met.

This allows us to compose calls to <em>fareyFromPreviousSeq</em> like this.
<pre class="lang:haskell decode:true ">--
λ-&gt; fareyFromPreviousSeq [F 0 1, F 1 1]
[0/1,1/2,1/1]
*Farey
λ-&gt; fareyFromPreviousSeq . fareyFromPreviousSeq $ [F 0 1, F 1 1]
[0/1,1/3,1/2,2/3,1/1]
*Farey
λ-&gt; fareyFromPreviousSeq . fareyFromPreviousSeq . fareyFromPreviousSeq  $ [F 0 1, F 1 1]
[0/1,1/4,1/3,1/2,2/3,3/4,1/1]
*Farey
λ-&gt; fareyFromPreviousSeq . fareyFromPreviousSeq . fareyFromPreviousSeq . fareyFromPreviousSeq $ [F 0 1, F 1 1]
[0/1,1/5,1/4,1/3,2/5,1/2,3/5,2/3,3/4,4/5,1/1]</pre>
Or, pleasingly, take what we want from an infinite list of sequences :)
<pre class="lang:haskell decode:true">--
λ-&gt; take 10 . iterate fareyFromPreviousSeq $ [F 0 1, F 1 1]
[[0/1,1/1],[0/1,1/2,1/1],[0/1,1/3,1/2,2/3,1/1],[0/1,1/4,1/3,1/2,2/3,3/4,
1/1],[0/1,1/5,1/4,1/3,2/5,1/2,3/5,2/3,3/4,4/5,1/1],[0/1,1/6,1/5,1/4,1/3,
2/5,1/2,3/5,2/3,3/4,4/5,5/6,1/1],[0/1,1/7,1/6,1/5,1/4,2/7,1/3,2/5,3/7,
1/2,4/7,3/5,2/3,5/7,3/4,4/5,5/6,6/7,1/1],[0/1,1/8,1/7,1/6,1/5,1/4,2/7,
1/3,3/8,2/5,3/7,1/2,4/7,3/5,5/8,2/3,5/7,3/4,4/5,5/6,6/7,7/8,1/1],
[0/1,1/9,1/8,1/7,1/6,1/5,2/9,1/4,2/7,1/3,3/8,2/5,3/7,4/9,1/2,5/9,4/7,
3/5,5/8,2/3,5/7,3/4,7/9,4/5,5/6,6/7,7/8,8/9,1/1],[0/1,1/10,1/9,1/8,1/7,
1/6,1/5,2/9,1/4,2/7,3/10,1/3,3/8,2/5,3/7,4/9,1/2,5/9,4/7,3/5,5/8,2/3,7/10,
5/7,3/4,7/9,4/5,5/6,6/7,7/8,8/9,9/10,1/1]]</pre>
which gives all sequences leading to the final one. To get just  the final sequence  we use <em>last</em>. i.e.
<pre class="lang:haskell decode:true ">--
λ-&gt; last . take 10 . iterate fareyFromPreviousSeq $ [F 0 1, F 1 1]
[0/1,1/10,1/9,1/8,1/7,1/6,1/5,2/9,1/4,2/7,3/10,1/3,3/8,2/5,3/7,4/9,1/2,
5/9,4/7,3/5,5/8,2/3,7/10,5/7,3/4,7/9,4/5,5/6,6/7,7/8,8/9,9/10,1/1]</pre>
Even generating all prior sequences this method is much quicker than <strong>Farey One</strong>. e.g.
<pre class="lang:haskell decode:true">--
λ-&gt; last . last . take 100 . iterate fareyFromPreviousSeq $ [F 0 1, F 1 1]
1/1
(0.13 secs, 82,779,136 bytes)

λ-&gt; last . last . take 1000 . iterate fareyFromPreviousSeq $ [F 0 1, F 1 1]
1/1
(113.17 secs, 83,112,154,032 bytes)
</pre>
Yet another way is to start with the initial sequence and repeatedly insert new fractions into it. Here is...
<h4 style="text-align: center;">Farey Three</h4>
<pre class="lang:haskell decode:true">--
-- Recursive insertion of terms up to given order
fareyForOrder :: Integer -&gt; [Fraction]
fareyForOrder 0 = [F 0 1, F 1 1]
fareyForOrder level = process (fareyForOrder (level - 1)) where
    process [f] = [f]
    process (F m n :F m' n':fs) = if n + n' == level
                                                then
                                                    F m n : F (m + m') level : process (F m' n' : fs)
                                                else
                                                    F m n : process (F m' n':fs)</pre>
<em>fareyForOrder</em> starts with the initial sequence [F 0 1, F 1 1] and recursively builds up the required sequence from the initial one. The execution times are, as would be expected, an improvement on<strong> Farey Two</strong>.
<pre class="lang:haskell decode:true">--
λ-&gt; last $ fareyForOrder 100
1/1
(0.07 secs, 64,166,928 bytes)
*Farey
λ-&gt; last $ fareyForOrder 1000
1/1
(65.28 secs, 69,769,323,616 bytes)</pre>
The final Farey function is one based on a recurrence relation involving three consecutive Farey fractions.
<h4 style="text-align: center;">Farey Four</h4>
The proof of the recurrence relation is an exercise in the rather excellent book "Concrete Mathematics"  by Ronald L. Graham, Donald E. Knuth and Oren Patashnik or look <a href="https://en.wikipedia.org/wiki/Farey_sequence">here</a>  on  Wikipedia.
<pre class="lang:haskell decode:true ">--
-- Using recurrence relation
farey :: Integer -&gt; [Fraction]
farey 1 = [F 0 1, F 1 1]
farey n = farey' (F 0 1) (F 1 n) where
    farey' (F a b) (F c d)
        | p == 1 &amp;&amp; q == 1 = [F a b, F c d, F 1 1]
        | otherwise = F a b : farey' (F c d) (F p q) where
            n' = floor $ fromIntegral (n + b) / fromIntegral d
            p = n' * c - a
            q = n' * d - b</pre>
As can be seen, it 'pacmans' its way along creating the next fraction (F p q) from the previous two, F a b  and F c d.  Of the four Farey functions this one, as might be expected is faster than the others, calculating Farey 10000 in less than a minute.
<pre class="lang:haskell decode:true">--
λ-&gt; last . farey $ 100
1/1
(0.01 secs, 4,900,808 bytes)
*Farey
λ-&gt; last . farey $ 1000
1/1
(0.57 secs, 464,068,544 bytes)
*Farey
λ-&gt; last . farey $ 10000
1/1
(58.21 secs, 46,348,229,112 bytes)</pre>
No doubt there are other ways of implementing these functions and I'd be please to hear from anyone who wants to add theirs!

Finally here's a neatly formatted pyramid of Farey fractions up to order 9.
<p style="text-align: center;"><span style="color: #0000ff;">0/1 1/1</span>
<span style="color: #0000ff;">0/1 1/2 1/1</span>
<span style="color: #0000ff;">0/1 1/3 1/2 2/3 1/1</span>
<span style="color: #0000ff;">0/1 1/4 1/3 1/2 2/3 3/4 1/1</span>
<span style="color: #0000ff;">0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1</span>
<span style="color: #0000ff;">0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1</span>
<span style="color: #0000ff;">0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1</span>
<span style="color: #0000ff;">0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1</span>
<span style="color: #0000ff;">0/1 1/9 1/8 1/7 1/6 1/5 2/9 1/4 2/7 1/3 3/8 2/5 3/7 4/9 1/2 5/9 4/7 3/5 5/8 2/3 5/7 3/4 7/9 4/5 5/6 6/7 7/8 8/9 1/1</span></p>
The <a href="http://gitcommit.co.uk/2018/06/05/ford-and-his-circles/">next post</a> will be showing a geometrical interpretation of these fractions.

All the code is in <a href="https://github.com/banditpig/Farey/tree/monoidFractions">Github</a> and thanks for reading!

&nbsp;