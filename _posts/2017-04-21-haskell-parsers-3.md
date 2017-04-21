---
ID: 367
post_title: Haskell Parsers 3.
author: BanditPig
post_date: 2017-04-21 10:54:23
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/04/21/haskell-parsers-3/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />This is the third part of parsing in Haskell. The previous posts are available at <a href="http://gitcommit.co.uk/2017/03/18/haskell-parsers-part-1/">Haskell Parsers Part 1</a> and <a href="http://gitcommit.co.uk/2017/03/30/haskell-parsers-2/">Haskell Parsers Part 2</a>. In part one we defined a parser as a type and derived a very simple parser, a parser of char.  Part two extended the ideas and created monadic, functorial and applicative instances of the parser type.  This allowed us to combine parsers monadically - using 'do' or in an applicative style - e.g. using '&lt;*&gt;' etc. Which to use is often a stylistic choice.

In this post we'll look at creating a  few extra functions to allow more varied combinations of parsers and we can then use them in a more practical setting which I'll describe in the final post.  Here is the 'finished' module of parser functions.
<pre class="lang:haskell decode:true ">{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Char 
import Control.Applicative hiding (many)

newtype Parser a = P (String -&gt; [(a,String)])

parse :: Parser a -&gt; String -&gt; [(a,String)]
parse (P p)  = p

ch :: Parser Char
ch = P (\s -&gt; case s of
                    []     -&gt; []
                    (x:xs) -&gt; [(x, xs)])


-- empty list denotes failure, 
-- and a singleton list denotes success

-- That is, fmap applies a function to the 
-- result value of a parser if the parser succeeds, 
--     and propagates the failure otherwise.

instance Functor Parser where
    -- fmap :: (a -&gt; b) -&gt; Parser a -&gt; Parser b
    fmap f p  = P (\s -&gt; case parse p s of
                             [] -&gt; []
                             [(x, xs)] -&gt; [(f x, xs)])

 
instance Applicative Parser where
  -- pure :: a -&gt; Parser a
  pure x = P (\s -&gt; [(x, s)])
  -- (&lt;*&gt;) :: Parser (a -&gt; b) -&gt; Parser a -&gt; Parser b
  pab &lt;*&gt; pa = P (\s -&gt; case parse pab s of
                    [] -&gt; []
                    [(fab, res)] -&gt; parse (fmap fab pa) res)



instance Monad Parser where
   -- return :: a -&gt; Parser a
   return x = P (\s -&gt; [(x, s)])
   -- (&gt;&gt;=) :: Parser a -&gt; (a -&gt; Parser b) -&gt; Parser b
   pa &gt;&gt;= f = P (\s -&gt; case parse pa s of
                  [] -&gt; []
                  [(a, res)] -&gt; parse (f a) res)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\_ -&gt; [])
    -- (&lt;|&gt;) :: Parser a-&gt; Parser a -&gt; Parser a   
    -- if p1 works the that one otherwise p2 
    p1 &lt;|&gt; p2 = P (\s -&gt; case parse p1 s of
                        []  -&gt; parse p2 s
                        res -&gt; res)
-- First of all, we define a parser satisfy p for 
-- single characters that satisfyisfy the predicate p

satisfy :: (Char -&gt; Bool) -&gt; Parser Char
satisfy p = do
    x &lt;- ch
    if p x then return x else P (\_ -&gt; [])

satisfy' :: (Char -&gt; Bool) -&gt; Parser Char
satisfy' p  = ch &gt;&gt;= (\x -&gt; if p x then return x else P (\_ -&gt; []))


-- parse a given char
char :: Char -&gt; Parser Char
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

string :: String -&gt; Parser String
string [] = return []
string s@(x:xs) = do
           char x
           string xs
           return s

string' :: String -&gt; Parser String
string' [] = return []
string' s@(x:xs) = char x *&gt; string' xs *&gt; return s

httpVerb :: Parser String
httpVerb = string "GET" &lt;|&gt; string "POST"  &lt;|&gt; string "DELETE"

httpPreamble :: Parser String
httpPreamble = do
  verb &lt;- httpVerb
  char ','
  return verb

-- or
httpPreamble' :: Parser String
httpPreamble' = httpVerb &lt;* char ','


many :: Parser a -&gt; Parser [a]   
many p = many1 p &lt;|&gt; pure []

many1 :: Parser a -&gt; Parser [a] 
many1 p = pure (:) &lt;*&gt; p &lt;*&gt; many p

ident :: Parser String
ident = do
    x  &lt;- lowerCase
    xs &lt;- many alphaNum 
    return (x:xs) 

ident' :: Parser String
ident' = pure (:) &lt;*&gt; lowerCase &lt;*&gt; many alphaNum 
    
natural :: Parser Int 
natural = do
  xs &lt;- many1 digit
  return (read xs)   

natural' :: Parser Int 
natural' = pure read &lt;*&gt; many1 digit

space :: Parser ()
space = do
    many (satisfy isSpace )
    return ()

space' :: Parser ()
space' = many (satisfy isSpace ) *&gt; return ()

int :: Parser Int
int = do
    char '-'
    n &lt;- natural
    return (-n)
    &lt;|&gt; natural

int' :: Parser Int
int' = char '-' *&gt; pure ((-1)*) &lt;*&gt; natural &lt;|&gt; natural

dropSpaces :: Parser a -&gt; Parser a
dropSpaces p = do 
    space 
    v &lt;- p
    space
    return v

dropSpaces' :: Parser a -&gt; Parser a
dropSpaces' p = space *&gt; p &lt;* space

identifier :: Parser String
identifier = dropSpaces ident

integer :: Parser Int
integer = dropSpaces int

literal :: String -&gt; Parser String
literal xs = dropSpaces (string xs)

anyString'  :: Parser String
anyString' = do
  x &lt;- alphaNum
  xs &lt;- many alphaNum
  return (x:xs)

anyString :: Parser String
anyString = dropSpaces anyString'

comment :: String -&gt;  Parser ()
comment cmnt = do
      string cmnt 
      many (satisfy ( /='\n') )
      return ()

comment' :: String -&gt;  Parser ()
comment' cmnt = string cmnt *&gt; many (satisfy (/='\n') ) *&gt; return ()

blockComment :: String -&gt; String -&gt; Parser ()
blockComment stS endS= do
  string stS
  manyUntil ch (string endS)
  return ()

blockComment' :: String -&gt; String -&gt;  Parser () 
blockComment' stS endS = string stS &gt;&gt; manyUntil ch (string endS) &gt;&gt; return ()

manyUntil :: Parser a -&gt; Parser b -&gt; Parser [a]
manyUntil p endp = go where
  go = do 
          endp
          return []
       &lt;|&gt;
         do
          x &lt;- p
          xs &lt;- go
          return (x:xs)

manyUntil' :: Parser a -&gt; Parser b -&gt; Parser [a]
manyUntil' p endp = scan' where 
  scan' = endp *&gt; return [] &lt;|&gt; pure (:) &lt;*&gt; p &lt;*&gt; scan'
</pre>
Many of the details in the above Haskell file have been discussed but quite a few haven't!
The '<em>many</em>' and '<em>many1</em>' parsers are particularly interesting. They're like the 'push-me-pull-you' of parsers and are mutually recursive. The '<em>many</em>' tries to run a parser zero or more times and '<em>many1</em>' tries to run the parser at least once.
<pre class="lang:haskell decode:true ">many :: Parser a -&gt; Parser [a]   
many p = many1 p &lt;|&gt; pure []
 
many1 :: Parser a -&gt; Parser [a] 
many1 p = pure (:) &lt;*&gt; p &lt;*&gt; many p
</pre>
If we first try '<em>many</em>' it first tries '<em>many1</em>' and if that fails it puts [] in context and that is the result. When '<em>many1</em>' is called it 'loads up' list concatenation operator and applies the parser so we the applied parser with a partially applied ':' function. Then '<em>many</em>' is called and that way they go back and forth until '<em>many</em>' fails, pure [] is run and the list gets resolved. i.e. Parser [a]

Our first use of '<em>many</em>'/'<em>many1</em>' is parsing for an identifier where an identifier is, arbitrarily, defined as starting with a lowercase letter then followed by any number of letters/digits.
<pre class="lang:haskell decode:true ">ident :: Parser String
ident = do
    x  &lt;- lowerCase
    xs &lt;- many alphaNum 
    return (x:xs)</pre>
and then there is the corresponding applicative style:
<pre class="lang:haskell decode:true ">ident' :: Parser String
ident' = pure (:) &lt;*&gt; lowerCase &lt;*&gt; many alphaNum</pre>
Next we define a space parser that just consumes leading spaces from the input string. Here it is in monadic and applicative style.
<pre class="lang:haskell decode:true ">space :: Parser ()
space = do
    many (satisfy isSpace )
    return ()
 
space' :: Parser ()
space' = many (satisfy isSpace ) *&gt; return ()</pre>
and in action
<pre class="lang:haskell decode:true ">λ-&gt; parse space " a1BC  "
[((),"a1BC  ")]</pre>
Notice that the leading spaces in " a1BC " are consumed and the trailing spaces are not as the parser stops, as it should, when getting to a non-space character.
To handle leading and trailing spaces for any parser we define '<em>dropSpaces</em>'
<pre class="lang:haskell decode:true ">dropSpaces :: Parser a -&gt; Parser a
dropSpaces p = do 
    space 
    v &lt;- p
    space
    return v
 
dropSpaces' :: Parser a -&gt; Parser a
dropSpaces' p = space *&gt; p &lt;* space</pre>
as can be seen '<em>dropSpaces</em>' parses spaces then runs the supplied parser and then runs the space parser again. And in ghci...
<pre class="lang:haskell decode:true ">λ-&gt; parse ( dropSpaces (many1 alphaNum ))  " a1BC  "
[("a1BC","")]</pre>
Finally the '<em>manyUntil</em>' is a slightly involved parser...
<pre class="lang:haskell decode:true ">manyUntil :: Parser a -&gt; Parser b -&gt; Parser [a]
manyUntil p endp = go where
  go = do 
          endp
          return []
       &lt;|&gt;
         do
          x &lt;- p
          xs &lt;- go
          return (x:xs)

manyUntil' :: Parser a -&gt; Parser b -&gt; Parser [a]
manyUntil' p endp = scan' where 
  scan' = endp *&gt; return [] &lt;|&gt; pure (:) &lt;*&gt; p &lt;*&gt; scan'</pre>
It runs a parser '<em>p</em>' until a 'terminating' parser '<em>endp</em>' succeeds. i.e 'keep parsing for something until you're able to parse something else'. Internally it uses the recursive '<em>go</em>' function to alternate between trying the 'termination' parser '<em>endp</em>' and building up the result by running the '<em>p</em>' parser and calling '<em>go</em>' again.
Having '<em>manyUntil</em>' will now allow us to create parsers for block comments.
<pre class="lang:haskell decode:true ">blockComment :: String -&gt; String -&gt; Parser ()
blockComment stS endS= do
  string stS
  manyUntil ch (string endS)
  return ()
 
blockComment' :: String -&gt; String -&gt;  Parser () 
blockComment' stS endS = string stS &gt;&gt; manyUntil ch (string endS) &gt;&gt; return ()
</pre>
and the comment is delimited by the start and end strings supplied to the parser.

Well, thanks for getting this far! In the next post we'll look at using some of what's been written and parsing strings into data and actually doing something with that data.