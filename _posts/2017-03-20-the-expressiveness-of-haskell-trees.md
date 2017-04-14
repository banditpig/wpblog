---
ID: 186
post_title: 'The expressiveness of Haskell &#8211; Trees.'
author: BanditPig
post_date: 2017-03-20 19:20:50
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/03/20/the-expressiveness-of-haskell-trees/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />A simple recursive data declaration is enough to define a tree.
<pre class="lang:haskell decode:true ">data Tree a = Empty | Node a (Tree a) (Tree a)</pre>
This simply states that a Tree of 'a's is either Empty or ('|') it has a Node that has an 'a' and two other trees, a left subtree and a right subtree.
The code to add an item to a tree is equally simple and recursive.
<pre class="lang:haskell decode:true ">add :: (Ord a) =&gt;  Tree a -&gt; a -&gt; Tree a
add Empty x = Node x Empty Empty 
add  (Node n left right) x
    | x &lt;=  n = Node n (add left x) right
    | otherwise  = Node n left (add right x)</pre>
The function signature
<pre class="lang:haskell decode:true ">add :: (Ord a) =&gt; Tree a -&gt; a -&gt; Tree a
</pre>
states that 'add' takes a Tree of 'a', a value to add and returns a Tree of 'a'. The only restriction on 'a' being that it must be of type 'Ord' meaning two 'a's can be compared to determine that larger one.

Line 2 of the add function shows that adding an item to an Empty tree is just a matter of creating a Node with the item along with two Empty subtrees.

When adding an item in the more general case then if the item is less than the entry in the current Node then the item is added, recursively, to the left subtree otherwise it is recursively added to the right subtree.

Done!