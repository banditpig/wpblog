---
ID: 240
post_title: Haskell Parsers 2.
author: BanditPig
post_date: 2017-03-30 11:03:16
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/03/30/haskell-parsers-2/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />This post continues from <a href="http://gitcommit.co.uk/2017/03/18/haskell-parsers-part-1/">Haskell Parsers Part 1</a> and here we look at elevating the parser first to a Functor then an Applicative and finally to a Monad. Creating Functor, Applicative and Monad instances for the parser will provide useful operations and allow parsers to be combined in interesting and useful ways.
For a <strong>Functor</strong> we need to implement the fmap function and in the context of a Parser the signature of fmap is
<pre class="lang:haskell decode:true ">fmap :: (a -&gt; b) -&gt; Parser a -&gt; Parser b</pre>
in other words "<em>A function a -&gt; b applied to the result of a parser of 'a' gives a parser of 'b'" </em>and the implementation is P with a lambda expression (\s -&gt; ...
<pre class="lang:haskell decode:true ">instance Functor Parser where
    -- fmap :: (a -&gt; b) -&gt; Parser a -&gt; Parser b
    fmap f p  = P (\s -&gt; case parse p s of -- run parser p with s...
                             -- failed so the result is []
                             [] -&gt; [] 
                             -- successful so apply f to x
                             [(x, xs)] -&gt; [(f x, xs)])  
</pre>
Next is <strong>Applicative</strong>. Whereas Functor applies a function to a value in a context, Applicative takes a function in a context and applies it to a value in a context. Applicative has two key functions to implement.
<pre class="lang:haskell decode:true ">pure :: a -&gt; Parser a
(&lt;*&gt;) :: Parser (a -&gt; b) -&gt; Parser a -&gt; Parser b</pre>
Pure is <em>"the minimum needed to put a value in a context"</em> and &lt;*&gt; known as 'apply' or 'tie-fighter' and it <em>"applies a parser that gives a function and applies that function to the result of applying another parser and the result of the function application is in the parser that's returned" </em> (Sounds convoluted but later a few examples will clarify things.) Anyway I think the implementation is much clearer than the 'hand wavy' description!
<pre class="lang:haskell decode:true">instance Applicative Parser where
  -- pure :: a -&gt; Parser a
  pure x = P (\s -&gt; [(x, s)])
  -- (&lt;*&gt;) :: Parser (a -&gt; b) -&gt; Parser a -&gt; Parser b
  pab &lt;*&gt; pa = P (\s -&gt; case parse pab s of
                    [] -&gt; []
                    [(fab, res)] -&gt; parse (fmap fab pa) res)
</pre>
and notice that <span class="lang:haskell decode:true crayon-inline ">fmap fab pa</span> is using fmap (from Functor) to apply the function (fab) 'into' the pa (the Parser a) which ends up being  Parser b.
Finally we look at a<strong> Monad</strong> for parsers. Having a Monad will allow us to sequence parsers using '&gt;&gt;', '&gt;&gt;=' or with the 'do' notation.
<pre class="lang:haskell decode:true">instance Monad Parser where
   -- return :: a -&gt; Parser a
   return x = P (\s -&gt; [(x, s)])
   -- (&gt;&gt;=) :: Parser a -&gt; (a -&gt; Parser b) -&gt; Parser b
   pa &gt;&gt;= f = P (\s -&gt; case parse pa s of
                  [] -&gt; []
                  [(a, res)] -&gt; parse (f a) res)</pre>
The return function is the same as the pure function in Applicative. The bind function (&gt;&gt;=) takes a Parser of 'a' , a  function that takes an 'a' returning a Parser of 'b' and returns a Parser of 'b'.

With the scene set lets create some simple parsers and 'mix n match' them into more sophisticated parsers. This first one is 'satisfy', a parser that succeeds if a condition is met and fails otherwise.
<pre class="lang:haskell decode:true">satisfy :: (Char -&gt; Bool) -&gt; Parser Char
satisfy p = do
    x &lt;- ch
    if p x then return x else P (\_ -&gt; [])</pre>
remembering that 'ch', on line 3, is a parser that  parses the first char from the input string, then the above applies 'ch' and sets the result to an empty parser if the predicate, p, fails and a parser of Char if the predicate succeeds.
This can also be written in a slightly more succinct way as
<pre class="lang:haskell decode:true">satisfy' :: (Char -&gt; Bool) -&gt; Parser Char
satisfy' p = ch &gt;&gt;= (\x -&gt; if p x then return x else P (\_ -&gt; []))
</pre>
It is often a matter of personal preference as to which style is preferred.
So with the 'satisfy' parser in place we can make a parser for a specific character.
<pre class="lang:haskell decode:true ">-- parse a given char
char :: Char -&gt; Parser Char
char x = satisfy (==x) 
</pre>
and trying these in ghci:
<pre class="lang:haskell decode:true ">λ-&gt; parse (char 'x') "abc"
[]
*Main
λ-&gt; parse (char 'x') "xabc"
[('x',"abc")]
*Main</pre>
Here are a couple of predicates and a number of parsers that use a predicate as input to the 'satisfy' parser.
<pre class="lang:haskell decode:true ">char :: Char -&gt; Parser Char
char x = satisfy (==x) 

-- couple of predicates
isVowel :: Char -&gt; Bool 
isVowel c = elem (toLower c) ['a','e', 'i', 'o', 'u']

isConsonant :: Char -&gt; Bool
isConsonant  = not . isVowel 

digit :: Parser Char
digit = satisfy isDigit

lowerCase :: Parser Char
lowerCase = satisfy isLower

upperCase :: Parser Char
upperCase = satisfy isUpper

alpha :: Parser Char
alpha = satisfy isAlpha

alphaNum  :: Parser Char
alphaNum  = satisfy isAlphaNum

vowel :: Parser Char
vowel = satisfy isVowel

consonant :: Parser Char
consonant = satisfy isConsonant
</pre>
So we have 'small' parsers that will parse a single character with or without some restriction on the character to be parsed. Next we will look at how to parse for a string. The idea is to keep calling the 'char' parser with the head of the input whilst removing the head each time. Here's an implementation in the 'Monadic style'.
<pre class="lang:haskell decode:true ">string :: String -&gt; Parser String
string [] = return []
string s@(x:xs) = do
           -- parse for first but ignore the result 
           -- its not needed.
           char x
           -- again with remainder
           string xs
           -- all succeeded so result is the original
           -- input string
           return s
</pre>
The same string parsing function can be written in the applicative style like this:
<pre class="lang:haskell decode:true ">string' :: String -&gt; Parser String
string' [] = return []
string' s@(x:xs) = char x *&gt; string' xs *&gt; return s</pre>
Here are some examples in ghci:
<pre class="lang:haskell decode:true ">λ-&gt; parse (string "hello") "xhello"
[]
*Main
λ-&gt; parse (string "hello") "hello123"
[("hello","123")]
*Main
λ-&gt; parse (string' "hello") "xhello"
[]
*Main
λ-&gt; parse (string' "hello") "hello123"
[("hello","123")]</pre>
We have parsers that can parse a particular string or, using a predicate, parse for certain characters. However we don't yet have a way of taking the input string and, for example, trying a number of parsers until one works or there are no parsers left. For example imagine we're parsing some log file that has HTTP verbs at the start of a line and the verb could be one of "GET", "POST", "DELETE" followed by a ',' and then the rest of the line.
<pre class="lang:haskell decode:true">GET | POST | DELETE, Rest of the line etc.</pre>
Now we can parse any <strong>one</strong> of these by using a suitable string parser - e.g.<span class="lang:haskell decode:true crayon-inline ">string "GET"</span> <strong>or</strong> <span class="lang:haskell decode:true crayon-inline ">string "POST"</span> <strong>or</strong> <span class="lang:haskell decode:true crayon-inline ">string "DELETE"</span>
So what we need to do is write code to give a choice between the three parsers. If the first for "GET" works then done otherwise try the next one and so on. Haskell has a typeclass to provide such behaviour!
<pre class="lang:haskell decode:true ">class Applicative f =&gt; Alternative f where
    -- | The identity of '&lt;|&gt;'
    empty :: f a
    -- | An associative binary operation
    (&lt;|&gt;) :: f a -&gt; f a -&gt; f a
</pre>
And its Parser based incantation is
<pre class="lang:haskell decode:true ">instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\_ -&gt; [])
    -- (&lt;|&gt;) :: Parser a-&gt; Parser a -&gt; Parser a   
    -- if p1 works the that one otherwise p2 
    p1 &lt;|&gt; p2 = P (\s -&gt; case parse p1 s of
                        []  -&gt; parse p2 s
                        res -&gt; res)</pre>
So using this we can define a parser for some of the HTTP verbs.
<pre class="lang:haskell decode:true ">httpVerb :: Parser String
httpVerb = string "GET" &lt;|&gt; string "POST" &lt;|&gt; string "DELETE"
</pre>
And some ghci examples:
<pre class="lang:haskell decode:true ">λ-&gt; parse httpVerb "GET, sdsdsdsd"
[("GET",", sdsdsdsd")]
*Main
λ-&gt; parse httpVerb "GET, Rest of the line etc."
[("GET",", Rest of the line etc.")]
*Main
λ-&gt; parse httpVerb "POST, Rest of the line etc."
[("POST",", Rest of the line etc.")]
*Main
λ-&gt; parse httpVerb "DELETE, Rest of the line etc."
[("DELETE",", Rest of the line etc.")]
*Main
λ-&gt; parse httpVerb "XXX, Rest of the line etc."
[]</pre>
Noticing that the hypothetical line of log output has a ',' after the http verb we can parse the ',' away in either monadic style
<pre class="lang:haskell decode:true ">httpPreamble :: Parser String
httpPreamble = do
  verb &lt;- httpVerb
  char ','
  return verb</pre>
or in an applicative fashion
<pre class="lang:haskell decode:true ">httpPreamble' :: Parser String
httpPreamble' = httpVerb &lt;* char ','</pre>
In both styles the ',' is silently dropped.
Having the 'or' or 'choice' operator, '&lt;|&gt;', will now allow us to expand the sophistication of our parsers.  To add more capability to our parsers consider the motivating problem  of how to parse a number given that our 'digit' parser can only take one char from the input string?. i.e. in ghci
<pre class="lang:haskell decode:true ">λ-&gt; parse digit "1234"
[('1',"234")]</pre>
The result of parsing is '1' and the remainder is "234". What we'd like to do is apply the digit parser repeatedly until it fails. To achieve this we define two mutually recursive parsers, 'many' i.e. zero or more and 'many1' - i.e. at least 1.
<pre class="lang:haskell decode:true">many :: Parser a -&gt; Parser [a]   
many p = many1 p &lt;|&gt; pure []

many1 :: Parser a -&gt; Parser [a] 
many1 p = pure (:) &lt;*&gt; p &lt;*&gt; many p</pre>
These two are a 'push me pull you' sort of thing. Start with '<em>many</em>' it calls '<em>many1</em>'  which puts ':' (list concatenation)  into a parser context and applies 'p'. Control then goes back to '<em>many</em>' - it then calls '<em>many1</em>' this continues and I imagine it as building up a bunch of ':' operations until the parser 'p' fails and at that point the whole lot gets resolved.

Now we can create a parser for (unsigned) integers. The idea is to keep applying the 'digit' parser and then use the 'read' function to make an integer from a string. In Monadic  and also Applicative form.
<pre class="lang:haskell decode:true ">natural :: Parser Int 
natural = do
  xs &lt;- many1 digit
  return (read xs)   

natural' :: Parser Int 
natural' = pure read &lt;*&gt; many1 digit</pre>
and some examples in ghci.
<pre class="lang:haskell decode:true ">λ-&gt; parse natural "12"
[(12,"")]
*Main
λ-&gt; parse natural "00042"
[(42,"")]
*Main
λ-&gt; parse natural "42XYZ"
[(42,"XYZ")]</pre>
All the parsers we've looked at are typically <span class="lang:haskell decode:true crayon-inline ">Parser String</span> or <span class="lang:haskell decode:true crayon-inline ">Parser Char</span>
In the next (third)  post of this series we'll look at creating more parsers and parser combinations with a view, in the fourth part, to parsing into data structures.