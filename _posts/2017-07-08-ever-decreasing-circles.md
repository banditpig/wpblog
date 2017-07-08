---
ID: 948
post_title: Ever Decreasing Circles.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/07/08/ever-decreasing-circles/
published: true
post_date: 2017-07-08 09:59:32
---
[latexpage]
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />A few days ago I came across this really interesting problem on the 'dailyprogrammer'.  Here's the question.

<em>Imagine you've got a square in 2D space, with axis values between 0 and 1, like <a href="http://i.imgur.com/5K0HZEk.png">this diagram</a>. The challenge today is conceptually simple: can you place a circle within the square such that exactly half of the points in the square lie within the circle and half lie outside the circle, like <a href="http://i.imgur.com/n7BDeyg.png">here</a>? You're going to write a program which does this - but you also need to find the smallest circle which solves the challenge, ie. has the minimum area of any circle containing exactly half the points in the square.</em>

<em>This is a hard challenge so we have a few constraints:</em>
<ul>
 	<li><em>Your circle must lie entirely within the square (the circle may touch the edge of the square, but no point within the circle may lie outside of the square).</em></li>
 	<li><em>Points on the edge of the circle count as being inside it.</em></li>
 	<li><em>There will always be an even number of points.</em></li>
</ul>
<em>There are some inputs which cannot be solved. If there is no solution to this challenge then your solver must indicate this - for example, in <a href="http://i.imgur.com/fDGPvX3.png">this scenaro</a>, there's no "dividing sphere" which lies entirely within the square.</em>

I mulled this over for quite a while and the first '<em>light going on</em>' was that the minimum circle would have to have some of the points on its circumference - because if it didn't then it wouldn't be the minimum circle and so could be made smaller. Combining this idea with the fact that a circle can be drawn through any three points (or the degenerate case of two points being coincident and diametrically opposed to the other point) gave me the idea of creating all possible circles from three and then from two points and counting how many points are are in these circles.

So let's start with some types...
<pre class="lang:haskell decode:true">-- x,y
type Point  = (Float, Float)
-- center (x,y) and radius.
type Circle = (Point, Float)  
</pre>
and now a function to create a circle from three points. Notice that we haven't implemented it. We're just exploring a solution based on types and function signatures. Where an implementation is simple or obvious we'll do it, otherwise it will be done later on a second iteration.
<pre class="lang:haskell decode:true ">makeCircle :: Point -&gt; Point -&gt; Point -&gt; Circle
makeCircle = undefined</pre>
As we can make one circle let's use it to create all possible circles from a list of points using the <em>makeCircle</em> function.
<pre class="lang:haskell decode:true ">allCircles :: [Point] -&gt; [Circle]
allCircles ps = [makeCircle p1 p2 p3 | p1 &lt;- ps, p2 &lt;- ps, p3 &lt;- ps, p1 /= p2 &amp;&amp; p1 /= p3 &amp;&amp; p2 /= p3]</pre>
All we're doing is taking the points three at a time, making sure that they're all different, then calling <em>makeCircle</em> with them. Even though <em>makeCircle</em> isn't implemented this all compiles and type checks.
We also need the 'two point circles' - these can be obtained by calling <em>makeCircle</em> with the the second and third point being the same. So <em>allCircles</em> now looks like...
<pre class="lang:haskell decode:true ">allCircles :: [Point] -&gt; [Circle]
allCircles ps = [makeCircle p1 p2 p3 | p1 &lt;- ps, p2 &lt;- ps, p3 &lt;- ps, p1 /= p2 &amp;&amp; p1 /= p3 &amp;&amp; p2 /= p3]  
             ++ [makeCircle p1 p2 p2 | p1 &lt;- ps, p2 &lt;- ps, p1 /= p2 &amp;&amp; p2 /= p1]
</pre>
At some point we will need to test if a given point is in a given circle. This is simple enough and is based on the equation of a circle $x^2 + y^2 = r^2$ and the implementation is a couple of lines.
<pre class="lang:haskell decode:true ">isInCircle :: Point -&gt; Circle -&gt; Bool
isInCircle (x, y) ((cx, cy), r) = (x - cx)^2 + (y - cy)^2 - r^2 &lt; 0.0001</pre>
where 0.0001 is fairly arbitrary constant that we could perhaps 'tweak'.

We can now write a function that takes each point in turn and checks to see if it is in a given circle.
<pre class="lang:haskell decode:true ">countPointsIn :: [Point] -&gt; Circle -&gt; Int
countPointsIn [] _ = 0
countPointsIn (p:ps) c 
    | isInCircle p c = 1 + countPointsIn ps c
    | otherwise      =     countPointsIn ps c</pre>
Line 2 is the base case, if we have no points then a total of zero points are in any circle.
The general case checks for the point at the head of the list being in the circle and acts accordingly.

The next function is
<pre class="lang:haskell decode:true ">isMinCircle :: [Point] -&gt; Circle -&gt; Bool
isMinCircle ps c = countPointsIn ps c == min where min = div (length ps) 2
</pre>
Part of condition for the circle to be the minimum circle is that it has exactly half of the points inside of it. Which is what <em>isMinCircle</em> succinctly expresses.
The next function is really the key to it all. The <em>getMinCirlce</em> first gets all the circles and, crucially, <strong>sorts them on the ascending radius of the circle</strong>. Then it takes each circle in turn until it finds one that has the desired number of points inside of it then stops. This will be the smallest circle as they are sorted by smallest radius first.
<pre class="lang:haskell decode:true ">getMinCircle :: [Point] -&gt; Maybe Circle
getMinCircle ps = result where
    -- smallest radius first
    circs = sortBy (comparing snd) . allCircles $ ps
    -- keep dropping circle while they don't have correct number of points inside
    maybeList =  dropWhile (\x -&gt; not $ isMinCircle  ps x ) circs
    -- pull result out
    result = f maybeList where
        f [] = Nothing
        f ls = Just (head ls)
</pre>
The <em>main</em> function is below along with a couple of helpers, one to show the result and the other to create the points from an input file.
<pre class="lang:haskell decode:true ">toPoint :: [String] -&gt; Point
toPoint (x:y:[]) = (read x :: Float, read y :: Float)

showResult :: Maybe Circle -&gt; String
showResult c = 
    case c of
        Nothing           -&gt; "Nothing"
        Just ((x, y), d ) -&gt; show x ++ " " ++ show y ++ " " ++ show d
   
main :: IO ()
main = do 
    input &lt;- readFile "points.txt"
    let points = map toPoint .  map words . lines $ input
    print $  showResult . getMinCircle $ points


</pre>
and the input text file is like this...
<pre class="lang:haskell decode:true">0.17741 0.3643
0.70953 0.64191
0.8507 0.40874
0.56568 0.35134
... etc.</pre>
which is slightly different from that on the puzzle link in that it does not have an integer at the start giving the total number of points. (We get that from the length of the list of points).
Finally we need to revisit the function <span class="lang:haskell decode:true crayon-inline">makeCircle :: Point -&gt; Point -&gt; Point -&gt; Circle</span> which is really an exercise in coordinate geometry. Take the general equation of a circle, plug in the points and solve the resulting equations. The <a href="http://www.ambrsoft.com/TrigoCalc/Circle3D.htm">page</a> show the details which I have implemented directly below.
<pre class="lang:haskell decode:true">makeCircle :: Point -&gt; Point -&gt; Point -&gt; Circle
makeCircle (x1, y1) (x2, y2) (x3, y3) = ((x, y), r) where
       k = 2 * (x1 * (y2 - y3) - y1 * (x2 - x3) + x2 * y3 - x3 * y2)
       x = ((x1^2 + y1^2) * (y2 - y3) + (x2^2 + y2^2) * (y3 - y1) + (x3^2 + y3^2) * (y1 - y2)) / k
       y = ((x1^2 + y1^2) * (x3 - x2) + (x2^2 + y2^2) * (x1 - x3) + (x3^2 + y3^2) * (x2 - x1)) / k
       r = sqrt ((x - x1)^2 + (y - y1))
</pre>
Here is the complete code:
<pre class="lang:haskell decode:true ">import Data.Ord
import Data.List (sortBy)
-- x,y
type Point  = (Float, Float)
-- center (x,y) and radius.
type Circle = (Point, Float)  

makeCircle :: Point -&gt; Point -&gt; Point -&gt; Circle
makeCircle (x1, y1) (x2, y2) (x3, y3) = ((x, y), r) where
       k = 2 * (x1 * (y2 - y3) - y1 * (x2 - x3) + x2 * y3 - x3 * y2)
       x = ((x1^2 + y1^2) * (y2 - y3) + (x2^2 + y2^2) * (y3 - y1) + (x3^2 + y3^2) * (y1 - y2)) / k
       y = ((x1^2 + y1^2) * (x3 - x2) + (x2^2 + y2^2) * (x1 - x3) + (x3^2 + y3^2) * (x2 - x1)) / k
       r = sqrt ((x - x1)^2 + (y - y1))


isInCircle :: Point -&gt; Circle -&gt; Bool
isInCircle (x, y) ((cx, cy), r) = 
    (x - cx)^2 + (y - cy)^2 - r^2 &lt; 0.0001

allCircles :: [Point] -&gt; [Circle]
allCircles ps = [makeCircle p1 p2 p3 | p1 &lt;- ps, p2 &lt;- ps, p3 &lt;- ps, p1 /= p2 &amp;&amp; p1 /= p3 &amp;&amp; p2 /= p3]  ++ [makeCircle p1 p2 p2| p1 &lt;- ps, p2 &lt;- ps, p1 /= p2 &amp;&amp; p2 /= p1]

countPointsIn :: [Point] -&gt; Circle -&gt; Int
countPointsIn [] _ = 0
countPointsIn (p:ps) c 
    | isInCircle p c = 1 + countPointsIn ps c
    | otherwise      =     countPointsIn ps c

isMinCircle :: [Point] -&gt; Circle -&gt; Bool
isMinCircle ps c = countPointsIn ps c == min where min = div (length ps) 2

getMinCircle :: [Point] -&gt; Maybe Circle
getMinCircle ps = result where
    circs = sortBy (comparing snd) . allCircles $ ps
    maybeList =  dropWhile (\x -&gt; not $ isMinCircle  ps x ) circs
    result = f maybeList where
        f [] = Nothing
        f ls = Just (head ls)

toPoint :: [String] -&gt; Point
toPoint (x:y:[]) = (read x :: Float, read y :: Float)

showResult :: Maybe Circle -&gt; String
showResult c = 
    case c of
        Nothing           -&gt; "Nothing"
        Just ((x, y), d ) -&gt; show x ++ " " ++ show y ++ " " ++ show d
   
main :: IO ()
main = do 
    input &lt;- readFile "points.txt"
    let points = map toPoint .  map words . lines $ input
    print $  showResult . getMinCircle $ points
</pre>
Test input of 50 points.
<pre class="lang:haskell decode:true ">0.17741 0.3643
0.70953 0.64191
0.8507 0.40874
0.56568 0.35134
0.56568 0.76805
0.96336 0.43929
0.91685 0.67614
0.61353 0.74929
0.32178 0.57213
0.90087 0.04679
0.47266 0.80022
0.35282 0.5678
0.12193 0.78317
0.27018 0.70962
0.60974 0.67577
0.00205 0.78244
0.30361 0.38056
0.88588 0.01516
0.85813 0.46199
0.16135 0.02435
0.09417 0.19733
0.96364 0.07113
0.97072 0.12663
0.70131 0.12721
0.63128 0.28139
0.08434 0.72938
0.73983 0.58714
0.08425 0.0076
0.50742 0.65794
0.9657 0.36221
0.98576 0.90219
0.94785 0.12917
0.10915 0.31807
0.61073 0.6223
0.17409 0.72275
0.53869 0.77369
0.10818 0.73822
0.57002 0.0511
0.18551 0.40974
0.95258 0.76844
0.77214 0.06548
0.93926 0.62406
0.8117 0.78065
0.61618 0.70183
0.51838 0.48385
0.20152 0.23971
0.68713 0.89812
0.65714 0.91779
0.95237 0.93368
0.45216 0.16395</pre>
and the output of running the code.
<pre class="lang:haskell decode:true ">"0.65400547 0.5596951 0.37332553"
[Finished in 3.1s]</pre>
I checked these results using the <a href="https://jsfiddle.net/gjkdc8hL/">link</a> from the puzzle and it passed! :)

<img class="alignnone wp-image-968" src="http://gitcommit.co.uk/wp-content/uploads/2017/07/Screen-Shot-2017-07-08-at-09.42.02-300x223.png" alt="" width="669" height="497" />

This is certainly not and optimised solution but what I like about it is that the problem can be 'seen' in the solution. It also shows how Haskell lets you build up a solution by writing small, very focussed functions, then composing them to solve a non-trivial problem. Overall I really enjoyed doing this and may well explore extending it to three or more dimensions - maybe exploiting the potential abstraction that may be gained by taking a vector based approach. As usual the code is on <a href="https://github.com/banditpig/minimumCircle">Github</a>.

Thanks for reading!