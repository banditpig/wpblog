---
ID: 1144
post_title: >
  The expressiveness of Haskell – Random
  Numbers.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/09/24/the-expressiveness-of-haskell-random-numbers/
published: true
post_date: 2017-09-24 11:40:19
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />Back in the 1940s the mathematician John von Neumann invented the <a href="https://en.wikipedia.org/wiki/Middle-square_method">'middle square'</a> method of generating pseudo random numbers. The algorithm starts with a 4 digit number which is then squared. If the resulting number has fewer than 8 digits then it is padded with leading zeros. From this 8 digit number the middle 4 digits are extracted and returned as the result and also as input to repeating the algorithm.

This implementation, in Haskell, will manipulate the digits of the number as a list and so in outline we need to take the seed number and

'<strong><em>square it</em></strong>' then '<strong><em>convert number to  digits</em></strong>' then '<strong><em>pad to 8</em></strong>' then '<strong><em>get</em></strong> <strong><em>middle 4</em></strong>' then '<strong><em>convert digits to number</em></strong>'

i.e. we can write a von Neumann Random number generator <em>vnRan</em> as
<pre class="lang:haskell decode:true">--
vnRan :: Int -&gt; Int
vnRan = digsToNum . mid4 . pad . numTodigs . sq</pre>
Starting from the right... <em>sq</em> can be defined as (^2)
<pre class="lang:haskell decode:true ">--
sq :: Int -&gt; Int
sq = (^ 2)</pre>
<em>numToDigs</em> combines <em>div</em> and <em>mod</em> to extract all the digits of the number.
<pre class="lang:haskell decode:true ">--
numTodigs :: Integral a =&gt; a -&gt; [a]
numTodigs 0 = []
numTodigs x = numTodigs (x `div` 10) ++ [x `mod` 10]
</pre>
the <em>pad</em> function is a simple recursive function that prepends '0' until the length is as needed.
<pre class="lang:haskell decode:true">--
pad :: Integral a =&gt; [a] -&gt; [a]
pad xs 
  | length xs &gt;= 8 = xs
  | otherwise = pad (0 : xs)
</pre>
The  <em>mid4</em> function uses the <em>substr</em> function written in a <a href="http://gitcommit.co.uk/2017/09/08/the-expressiveness-of-haskell-substrings/">previous post.</a> i.e.
<pre class="lang:haskell decode:true">--
mid4 :: [a] -&gt; [a]
mid4 = substr 2 4 
substr :: Int -&gt; Int -&gt; [a] -&gt; [a]
substr s e xs = take e (drop s xs)
</pre>
and finally <em>digsToNum</em> builds up the number, from a list, using powers of 10.
<pre class="lang:haskell decode:true ">--
digsToNum :: [Int] -&gt; Int
digsToNum ds = eval (10 ^ l) ds
  where
    l = length ds - 1
    eval _ []     = 0
    eval n (x:xs) = (n * x) + eval (n `div` 10) xs
</pre>
So there it is, fairly trivial, but does highlight the notion of producing more complex functions  by the composition of smaller, simpler functions.
To see it in action we need to use the <em>iterate</em> function to repeatedly apply <em>vnRan</em> like this:
<pre class="lang:haskell decode:true ">--
λ-&gt; take 100 $ iterate vnRan 1213
[1213,1369,4161,3921,4241,6081,8561,721,9841,5281,8961,9521,9441,2481,5361,321,3041,7681,7761,3121,641,881,6161,7921,2241,2081,561,4721,7841,1281,961,3521,7441,8481,7361,4321,1041,3681,9761,7121,8641,6881,8161,1921,241,8081,2561,8721,5841,7281,2961,7521,5441,4481,9361,8321,9041,9681,1761,1121,6641,2881,161,5921,8241,4081,4561,2721,3841,3281,4961,1521,3441,481,1361,2321,7041,5681,3761,5121,4641,8881,2161,9921,6241,81,6561,6721,1841,9281,6961,5521,1441,6481,3361,6321,5041,1681,5761,9121]
*Main
λ-&gt;</pre>
Here is the entire code in one block and I've added a main function that uses the system time to seed the <em>vnRan</em> with a 'random' 4 digit number.
<pre class="lang:haskell decode:true ">--
import Data.Time.Clock.POSIX

numTodigs :: Integral a =&gt; a -&gt; [a]
numTodigs 0 = []
numTodigs x = numTodigs (x `div` 10) ++ [x `mod` 10]

pad :: Integral a =&gt; [a] -&gt; [a]
pad xs 
  | length xs &gt;= 8 = xs
  | otherwise = pad (0 : xs)

mid4 :: [a] -&gt; [a]
mid4 = substr 2 4 
substr :: Int -&gt; Int -&gt; [a] -&gt; [a]
substr s e xs = take e (drop s xs)

digsToNum :: [Int] -&gt; Int
digsToNum ds = eval (10 ^ l) ds
  where
    l = length ds - 1
    eval _ []     = 0
    eval n (x:xs) = (n * x) + eval (n `div` 10) xs

sq :: Int -&gt; Int
sq = (^ 2)

vnRan :: Int -&gt; Int
vnRan = digsToNum . mid4 . pad . numTodigs . sq

sysInt :: IO Int
sysInt = round . (* 1000) &lt;$&gt; getPOSIXTime

fourDigits :: Int -&gt; Int
fourDigits n = n `mod` 10000

main = do
  n &lt;- sysInt
  print $ take 150 $ iterate vnRan (fourDigits n)
</pre>
Anyone playing with this will soon see the sequence quite quickly either repeats or converges to zero and then repeats. This <a href="https://arxiv.org/abs/1704.00358">paper</a> modifies the algorithm slightly and overcomes the quick convergence of the sequence and I'll hopefully post about it soon!

Thanks for reading!