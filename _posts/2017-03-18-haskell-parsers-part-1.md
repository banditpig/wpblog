---
ID: 34
post_title: Haskell Parsers Part 1.
author: BanditPig
post_date: 2017-03-18 13:52:31
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/03/18/haskell-parsers-part-1/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />I really enjoy using Haskell. It's probably the most intellectually satisfying programming language ever! Parsing is one of its many strengths and these next few posts will be about creating simple parsers from first principles and expressing them in two different ways.

For parsing, start with a newtype definition for a Parser and a data constructor P that has a function String -&gt; [(a, String)] which will parse the string for an 'a'. The result of the parse is a list of tuples. The first element of a tuple being an 'a' and the other is whatever is left of the string after parsing an 'a'. The convention is that if the parser fails then an empty list is returned. (In this context failure means failing to parse an 'a' rather than some programmatic error.)
<pre class="lang:haskell decode:true ">newtype Parser a = P (String -&gt; [(a,String)])
</pre>
A parse function can now be defined as
<pre class="lang:haskell decode:true">parse :: Parser a -&gt; String -&gt; [(a,String)]
parse (P p) str = p str</pre>
In other words the parse function takes a Parser of 'a' and a String and returns a list (possibly empty) of 'a' and whatever of the string is not consumed by the process of parsing.
This can be written in point-free style as
<pre class="lang:haskell decode:true ">parse (P p) = p
</pre>
(point-free is a Haskell style where function arguments can often be omitted - it can give cleaner code but sometimes it can produce obfuscated code!). This leads to the first parser, ch.
<pre class="lang:haskell decode:true">ch :: Parser Char
ch = P (\s -&gt; case s of
                    []     -&gt; []
                    (x:xs) -&gt; [(x, xs)])

</pre>
ch is a Parser of Char and either fails if the input string is empty or succeeds returning the first character in the string. If the above snippets are put into Haskell source file:
<pre class="lang:haskell decode:true ">import Data.Char

newtype Parser a = P (String -&gt; [(a,String)])

parse :: Parser a -&gt; String -&gt; [(a,String)]
parse (P p)  = p

ch :: Parser Char
ch = P (\s -&gt; case s of
                    []     -&gt; []
                    (x:xs) -&gt; [(x, xs)])</pre>
and loaded into ghci it can be run interactively by calling the parse function with the ch parser and a string to parse.
<pre class="lang:haskell decode:true ">λ-&gt; parse ch "abcd"
[('a',"bcd")]</pre>
If there is no character to parse then an empty list is returned.
<pre class="lang:haskell decode:true ">λ-&gt; parse ch ""
[]</pre>
If there's just one character in the string then that is returned and the string is completely consumed.
<pre class="lang:haskell decode:true ">λ-&gt; parse ch "a"
[('a',"")]</pre>
Or maybe map the parser over a list of strings...
<pre class="lang:haskell decode:true ">λ-&gt; map (parse ch) ["abcd", "defg", "hijk"]
[[('a',"bcd")],[('d',"efg")],[('h',"ijk")]]</pre>
The <a href="http://gitcommit.co.uk/2017/03/30/haskell-parsers-2/">next part in this series</a> will look at how to parse a specified character and the parsing of numbers etc.