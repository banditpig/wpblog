---
ID: 1340
post_title: >
  The expressiveness of Haskell – Split
  a List.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/03/10/the-expressiveness-of-haskell-split-a-list/
published: true
post_date: 2018-03-10 10:01:18
---
<img class="alignnone size-medium wp-image-349" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/cropped-refresh-1-300x300.png" alt="" width="300" height="300" />Just recently playing around with one of the excellent <a href="https://adventofcode.com/">Advent of Code</a> problems (specifically <a href="https://adventofcode.com/2015/day/6">2015 question 6</a> ) a possible solution involved splitting a list into two parts where the first element goes into the first part, the second element into the second part and so on. For example
<ul>
 	<li>[1, 2, 3, 4, 5, 6] -&gt; [1, 3, 5], [2, 4, 6]</li>
</ul>
So, in terms of function signatures we can write <span class="lang:haskell decode:true crayon-inline ">splitList :: [a] -&gt; ([a], [a])</span>  Notice there's no constraint on the type in the list, it's a  very general function.

One way of doing this is is to make use of pattern matching and build up a solution recursively. i.e.
<pre class="lang:haskell decode:true">--
--
splitList :: [a] -&gt; ([a], [a])
splitList xys = go [] [] xys where
    go xs ys []         = (xs, ys)
    go xs ys [x]        = (xs ++ [x], ys)
    go xs ys [x, y]     = (xs ++ [x], ys ++ [y])
    go xs ys (x:y:rest) = go (xs ++ [x]) (ys ++ [y]) rest</pre>
Here the <em>splitList</em> function calls another function that returns the solution in the 'correct shape'. This <em>go</em> function uses pattern matching for the possible cases of the input list.
<ul>
 	<li>An empty input list get split into two empty lists.</li>
 	<li>Splitting a list of one item gives a list with that item and an empty list.</li>
 	<li>Similarly for a list of two items  the result is two lists with one item in each.</li>
 	<li>The more general case pattern matches the input to two items and 'the rest', it then  builds up the result lists and calls itself.</li>
</ul>
This certainly works fine - here are some sample runs.
<pre class="lang:haskell decode:true">--
--
λ-&gt; splitList []
([],[])
*Main
λ-&gt; splitList [1]
([1],[])
*Main
λ-&gt; splitList [1,2]
([1],[2])
*Main
λ-&gt; splitList [1,2,3,4]
([1,3],[2,4])
*Main
λ-&gt; splitList [1,2,3,4,5]
([1,3,5],[2,4])
*Main
λ-&gt; splitList [1,2,3,4,5,6]
([1,3,5],[2,4,6])</pre>
And looking at the code it is fairly clear as to what's happening. However it is a bit verbose and I think Haskell can provide a 'better' solution!

We have a list containing elements of type <em>a</em>. We know nothing about <em>a</em> so trying to apply a filter to the list won't really work. However the list could be morphed into another list such that the original data is still there but there's extra information that can be used as leverage. The <em>zip</em> function takes two lists and returns a list of pairs. i.e. <span class="lang:haskell decode:true crayon-inline ">zip :: [a] -&gt; [b] -&gt; [(a, b)]</span>  and if one list is longer than the other then zip terminates when the shortest list has ben processed. This allows us to use an infinite list when zipping. So we can <em>zip</em> up the original list and in effect tag the data in such a way that it will allow us to extract alternate elements.

For example
<pre class="lang:haskell decode:true">--
λ-&gt; zip ['a', 'b', 'c', 'd'] [0..]
[('a',0),('b',1),('c',2),('d',3)]</pre>
We can now filter <span class="lang:haskell decode:true crayon-inline ">[('a',0),('b',1),('c',2),('d',3)]</span>  by taking only those elements whose second value is odd (or even) i.e.  filtering by odd:
<pre class="lang:haskell decode:true ">--
λ-&gt; filter (odd . snd) [('a',0),('b',1),('c',2),('d',3)]
[('b',1),('d',3)]</pre>
We now have the alternate elements but they are tagged - so we need to remove the tags by taking just the first element of each entry in the list of pairs.
<pre class="lang:haskell decode:true">--
λ-&gt; map fst [('b',1),('d',3)]
"bd"</pre>
Using the above ideas in  an alternate implementation of <em>splitList</em> we have
<pre class="lang:haskell decode:true ">--
--
splitList :: [a] -&gt; ([a], [a])
splitList xs = (f 1 xs, f 0 xs) where
  f n a = map fst . filter (odd . snd) . zip a $ [n..]</pre>
here the pair of lists are created from the alternate elements simply by zipping up with tags starting at different values.

A very succinct one-liner using function composition. I find little things like this in Haskell  very, very pleasing and immensely satisfying! :)

Here are a few sample runs.
<pre class="lang:haskell decode:true ">--
λ-&gt; splitList ['a'..'z']
("acegikmoqsuwy","bdfhjlnprtvxz")
*Main
λ-&gt; splitList ['a'..'z']
("acegikmoqsuwy","bdfhjlnprtvxz")
*Main
λ-&gt; splitList ['A'..'Z']
("ACEGIKMOQSUWY","BDFHJLNPRTVXZ")
*Main
λ-&gt; splitList [-9..9]
([-9,-7,-5,-3,-1,1,3,5,7,9],[-8,-6,-4,-2,0,2,4,6,8])</pre>
&nbsp;

Thanks for reading...!

&nbsp;