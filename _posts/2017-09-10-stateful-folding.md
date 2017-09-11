---
ID: 1098
post_title: Stateful Folding.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/09/10/stateful-folding/
published: true
post_date: 2017-09-10 19:59:14
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />This is not a monad tutorial. These are my notes and code to help me grok monads. :)
As part of learning Haskell there is for some, perhaps many - including me, the ascent of Mount Monad!
I'm finding the way to '<em>get it'</em> is to keep chipping away at Monads, read, absorb, write some code then rinse and repeat! This creates a familiarity with the symbols, with the <em>do</em>, &gt;&gt;= and &gt;&gt; being different faces of the same coin.
As part of this launderette of code I started writing simple Haskell functions and then pulled them apart and rewrote them using the State Monad. Just as a learning exercise.

Let's start with a very simple function to sum a list of integers.
<pre class="lang:haskell decode:true ">--
sumList :: [Int] -&gt; Int
sumList [] = 0
sumList (x:xs) = x + sumList xs</pre>
Looking at this from a stateful perspective, to determine what changes, we see the function is called repeatedly with a list of decreasing length whilst the accumulated sum increases.
This suggests that the State can be modelled using a tuple of Int and a list of Int. i.e. giving the signature
<pre class="lang:haskell decode:true ">sumListS :: State (Int, [Int]) Int
sumListS = undefined
</pre>
For the moment the implementation is undefined and we can write the following function that will compile.
<pre class="lang:haskell decode:true ">sumList' :: [Int] -&gt; Int
sumList' xs = evalState sumListS (0, xs)</pre>
The function takes a list of Int and returns an Int and internally it evaluates the state from the initial state that has an accumulated sum of 0 and an initial list that is the  supplied list of Int.
So, what must the implementation of <em>sumListS</em> look like? Well, we need to get the current state and update the running total, then, if we've finished just call the return function with the updated total as the argument, otherwise update the state and put it back and continue. First let's write it using <em>do</em> notation.
<pre class="lang:haskell decode:true">-- 
sumListS :: State (Int, [Int]) Int
sumListS = do
  (sm, (x:xs)) &lt;- get
  if xs == [] 
    then return (sm +x)
  else
    do
     put (sm + x, xs)
     sumListS
</pre>
Line 4 gets the current state.
Line 5 checks the termination condition and calls the return function  with the updated sum.
Otherwise we execute line 9 and then line 10.
Line 9 puts an updated state - which is an increased total and a reduced list.
Line 10 recurses.

Notice that with lines 8,9 and 10 we are actually sequencing two actions so can write it as
<pre class="lang:haskell decode:true">--
put (sm + x, xs)  &gt;&gt;= \_ -&gt; sumListS</pre>
i.e. the result of <em>put</em> is ignored in the invoked lambda expression \_ -&gt; <em>sumList </em>and because it's ignored we can write it  more succinctly as
<pre class="lang:haskell decode:true">--
put (sm + x, xs) &gt;&gt; sumListS</pre>
Also the line (sm, (x:xs)) &lt;- get can be written as get &gt;&gt;= \(sm, (x:xs)) -&gt; ....
So, using bind notation the completed function is
<pre class="lang:haskell decode:true">--
sumListS :: State (Int, [Int]) Int
sumListS = get &gt;&gt;= \(sm, (x:xs)) -&gt; if xs == [] 
                             then return (sm +x)
                           else
                             put (sm + x, xs) &gt;&gt; sumListS

sumList' :: [Int] -&gt; Int
sumList' xs = evalState sumListS (0, xs)
</pre>
How about the factorial function?
Here's a simple implementation
<pre class="lang:haskell decode:true ">--
fact :: Int -&gt; Int 
fact 0 = 1
fact n = n * fact (n -1)</pre>
And in State Monad style this becomes
<pre class="lang:haskell decode:true ">--
factS :: State Int Int
factS = get &gt;&gt;= \x -&gt; if x == 0
                       then return 1
                      else 
                        put (x - 1) &gt;&gt; fmap (*x) factS
                     
fact :: Int -&gt; Int                     
fact = evalState factS</pre>
Notice the use of <em>fmap</em> to lift the function  (*x) into the monad. I won't add any more but it is instructive to just try it as an exercise.

Now, to get to the title of this post! How can we write the <em>foldr</em> function using the State Monad? First checkout the signature of <em>foldr </em>when applied to a list.
<pre class="lang:haskell decode:true ">--
  foldr :: (a -&gt; b -&gt; b) -&gt; b -&gt; [a] -&gt; b</pre>
this suggest that the state can be modelled as a tuple of a function, a 'b' and a list of 'a'
i.e. <span class="lang:haskell decode:true crayon-inline ">foldrS :: (Eq a) =&gt; State ((a -&gt; b -&gt; b), b, [a] ) b</span>
And given what we've just done I think the implementation is fairly self explanatory:
<pre class="lang:haskell decode:true ">--
foldrS :: (Eq a) =&gt; State ((a -&gt; b -&gt; b), b, [a] ) b
foldrS = get &gt;&gt;= \(f, y, (x:xs)) -&gt; if xs == []
                                         then return $ f x y
                                       else
                                         put (f, f x y, xs) &gt;&gt; foldrS

foldr' f y xs =  evalState foldrS (f, y, xs) 
</pre>
First we get the state tuple - in particular split the list according to<em> (x:xs)</em>. If this is the last element to handle (<em> xs == []</em> ) then we call the <em>return</em> function with the result of a final application of the supplied function, <em>f</em>. Otherwise we update the state by applying <em>f x y</em> and reducing the list. Then we recurse. And, finally we have the <em>foldr</em>' function which evaluates the state starting with the initial state. :)

A couple of examples of the usual foldr and the stateful foldr' should show the results to be consistent.

Sum of all numbers from 1 to 10.
<pre class="lang:haskell decode:true ">--
λ-&gt; foldr (+) 0 [1..10]
55
*Main
λ-&gt; foldr' (+) 0 [1..10]
55
*Main</pre>
And the product...
<pre class="lang:haskell decode:true ">--
λ-&gt; foldr (*) 1 [1..10]
3628800
*Main
λ-&gt; foldr' (*) 1 [1..10]
3628800
*Main</pre>
Of course much of this is just an academic exercise but I really found it worthwhile and instructive to do and it helped me understand Monads a bit more and extend, just a little, my horizon into the satisfying world of Haskell coding!

Thanks for reading...