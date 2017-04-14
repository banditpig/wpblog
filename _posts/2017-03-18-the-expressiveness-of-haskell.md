---
ID: 107
post_title: 'The expressiveness of Haskell &#8211; Sorting.'
author: BanditPig
post_date: 2017-03-18 11:55:31
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/03/18/the-expressiveness-of-haskell/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />What first excited me about Haskell was the expressiveness of the language. It somehow encapsulates thought and the problem being solved can often be seen clearly in the solution.
Take, for example, sorting a list containing elements of a general type ‘a’. For these to be placed in order it must be possible to compare them. So in Haskell the sort function can be defined as:
<pre class="lang:haskell decode:true ">sort :: (Ord a) =&gt;  [a] -&gt; [a]
</pre>
i.e. the sort function takes a list of ‘a’ and returns a list of ‘a’ and the restriction on the type of ‘a’ is that it must be an instance of the Ord class – this allows comparison of two ‘a’ s. The function can be implemented as
<pre class="lang:haskell decode:true ">sort :: (Ord a) =&gt;  [a] -&gt; [a]
sort []     = []
sort (x:xs) = sort left ++ [x] ++ sort right 
              where
               left  = [x' | x' &lt;- xs, x' &lt;= x]
               right = [x' | x' &lt;- xs, x'  &gt; x]</pre>
If you don’t know Haskell at all then just stare at it for a while and you may get an intuition of what’s happening!
Line 2 states that sorting an empty list just gives an empty list.
Otherwise, line 3, take the first item in the list and put it in a list [x]. Then prepend the recursively sorted list of all items that are less than or equal to x, i.e. <em>sort left</em> and append to [x] the recursively sorted list of all items that are greater than x, i.e. <em>sort right</em>. In these two lines:
<pre class="lang:haskell decode:true ">left  = [x' | x' &lt;- xs, x' &lt;= x]
right = [x' | x' &lt;- xs, x'  &gt; x]</pre>
left and right are known as list comprehensions. For example left means get all x’ from the list xs such that x’ &lt;= x and similarly for right.

It might not be efficient but it is very, very pleasing.