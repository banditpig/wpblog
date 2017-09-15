---
ID: 1077
post_title: The Root of the Problem. Part 2.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/09/15/the-root-of-the-problem-part-2/
published: true
post_date: 2017-09-15 15:34:18
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />In the <a href="http://gitcommit.co.uk/2017/08/25/the-root-of-the-problem-part-1/">previous post</a> I discussed an algorithm for calculating the square root of a number. This short post will show an implementation in Haskell.

I did one implementation using the State Monad but to be quite honest it was an impenetrable mess! I'm sure part of that was down to my embryonic Haskell skills but perhaps the State Monad was a bad fit for this problem? The implementation below uses  a fold over a list of digit pairs and the start state for the fold is based on 'the largest integer squared' with subsequent calculations using 'doubling' - as per the algorithm. Most of the other functions are really helpers for formatting, digit pair extraction, decimal point location etc. I think the code is fairly readable yet I still feel it is a little too complex and would welcome any comments from more experienced Haskellers.
<pre class="lang:haskell decode:true ">--

import Data.List.Split
import Data.Char 
import Numeric

-- The entry point.
root :: Float -&gt; Float
root n = f   where
  ((f, _):_) = readFloat (lhs ++ "." ++ rhs)
  (_, rootDigits) = rootFold n
  (lhs, rhs) = splitAt (dpLocation n) rootDigits
--    
-- fold with the initial value based on intSquareRoot function
-- and subsequent calculations based on doubling and the biggestN function.
rootFold :: Float -&gt; (Integer, String)
rootFold n = foldr  calculate (makeStartVal p1 p2) pairs where
  (p1:p2:pairs) = digitList  n 

  makeStartVal :: Integer -&gt; Integer -&gt; (Integer, String)
  makeStartVal p1 p2 = res where
    rt =  intSquareRoot  p1
    res = (p2 + (p1 - rt * rt) * 100 , show rt)

  calculate :: Integer -&gt; (Integer, String) -&gt; (Integer, String)
  calculate  p (n, res) = next where
    (toAppend, remain) = biggestN (2 * read res) n
    -- bring down the next pair and accumulate the result
    next = (remain * 100 + p, res ++ show toAppend)

-- Where should decimal point be?
dpLocation :: Float -&gt; Int
dpLocation n =  if (even len) then len `div` 2 else (len + 1) `div` 2  where
  [left, _] = splitOn "." . show $ n
  len = length left
 
-- helper for formatting
formatFloatN numOfDecimals floatNum  = showFFloat (Just numOfDecimals) floatNum ""
showFlt = formatFloatN 16

-- Takes float and makes list of 'paired' integers
digitList :: Float -&gt; [Integer]
digitList n = res where
    [l, r] = splitOn "." . showFlt $ n
    res = map read $ (pairs . pad $ l) ++  (pairs . pad $ r) where
      pairs [] = []
      pairs xs =
        let (ys, zs) = splitAt 2 xs
        in  ys : pairs  zs
      pad xs 
        | odd . length $ xs = "0" ++ xs
        | otherwise = xs

     
-- eg  largest number N such that 4N x N &lt;= 161 
-- and biggestN 4 161 = (3, 32)
--
biggestN :: Integer -&gt; Integer -&gt; (Integer, Integer)
biggestN  = get 0  where 
    get n x y 
        | (x*10 + n) * n &gt;  y = (n-1, y - (x*10 + n - 1)*(n - 1))
        | (x*10 + n) * n == y = (n  , y - (x*10 + n) * n )
        | otherwise           = get (n + 1) x y

-- gives the largest int whose square is &lt;= n
intSquareRoot :: Integer -&gt; Integer
intSquareRoot n = root 0 where
  root i   
   | i*i &lt;= n  = root (i + 1) 
   | otherwise = i - 1
</pre>
And some examples of using it...
<pre class="lang:haskell decode:true ">λ-&gt; map root [1, 2, 4, 33, 100, 101, 123, 144, 625, 123456.234]
[1.0,1.4142135,2.0,5.7445626,10.0,10.049875,11.090536,12.0,25.0,351.28336]</pre>
and if we compose map <em>(^2)</em> with <em>map root</em> we should get back exactly what we started with in the case of perfect squares and something more or less the same for others.
<pre class="lang:haskell decode:true ">λ-&gt; map (^2) . map root $ [1, 2, 4, 33, 100, 101, 123, 144, 625, 123456.234]
[1.0,1.9999999,4.0,33.0,100.0,100.99999,122.99999,144.0,625.0,123399.99]
*Main</pre>
Thanks for reading...!