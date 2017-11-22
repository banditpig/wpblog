---
ID: 1218
post_title: Parsers to Fractions to Square Roots!
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/11/22/parsers-to-fractions-to-square-roots/
published: true
post_date: 2017-11-22 19:11:42
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />The earlier post <a href="http://gitcommit.co.uk/2017/11/16/fractions-to-phi-to-fibonacci/"><em>Fractions to Phi to Fibonacci!</em></a> showed a simple structure for Continued Fractions (CF) and used a CF representation of the Golden Ratio to derive the terms of the Fibonacci Sequence. It also alluded to a particular notation to express a CF and here we will describe that notation, create a parser for it and calculate the square roots of some integers from their specific CFs and finally derive a general CF that can be used to find the square root of any integer.

&nbsp;
<h2 style="text-align: center;">CF List Notation</h2>
Essentially the CF is written as a list of ints like this [a0; a1, a2, a3...]. The first entry is followed by a ';' and others by a ','. The terms a1, a2, a3... taken together, are the 'repeating' terms and it is necessary to only write one 'cycle' of the repeating terms. For example root 2 as a CF is

<img class="alignnone size-full wp-image-1195" src="http://gitcommit.co.uk/wp-content/uploads/2017/11/root2.png" alt="" width="278" height="162" />

and as a list this would be [1;2]

or

<img class="alignnone size-medium wp-image-1171" src="http://gitcommit.co.uk/wp-content/uploads/2017/11/phi-300x177.gif" alt="" width="300" height="177" />

would be [1;1].
As for square roots of integers, here's a list  in CF list form. Again, for more details, please see

<a href="http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/cfINTRO.html#section6.3">http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/cfINTRO.html#section6.3 </a>

√1 [ 1; ]
√2 [ 1; 2 ]
√3 [ 1; 1, 2 ]
√4 [ 2; ]
√5 [ 2; 4 ]
√6 [ 2; 2, 4 ]
√7 [ 2; 1, 1, 1, 4 ]
√8 [ 2; 1, 4 ]
√9 [ 3; ]
√10 [ 3; 6 ]

&nbsp;
<h2 style="text-align: center;">Haskell Abstraction</h2>
We can represent this list notation as a first <em>Digit</em>  and then a list of the repeated <em>Digit</em> like this
<pre class="lang:haskell decode:true">--
--
type Digit  = Integer
type First  = Digit
type Repeat = [Digit]

-- [a1;a2,a3,a4...] =&gt; first = a1, repeat = [a2,a3,a4]
data FracStruc = FracStruc { first :: First, repeat :: Repeat} deriving (Show)</pre>
really just some type synonyms and a <em>FracStruc</em> using record syntax.

Writing functions that will parse a string into a <em>FracStruc</em> is fairly straightforward I really quite enjoy this sort of thing with Haskell. So, using <em>Megaparsec</em> a few functions to do this are
<pre class="lang:haskell decode:true">--
--

-- right and left bird :). Put the parser in the middle
dropSpaces :: Parser a -&gt; Parser a
dropSpaces p = space *&gt; p &lt;* space

-- A number followed by a separator char - for Fractions this will be ; or ,
digitSep :: Char -&gt; Parser Digit
digitSep = dropSpaces . digitSep1 where
        digitSep1 :: Char -&gt; Parser Digit
        digitSep1  ch = do
            d &lt;- some digitChar
            space
            many . char $ ch
            return (read d)

-- Parse out the a1 in [a1;a2,a3...]
firstP :: Parser Digit
firstP = dropSpaces . digitSep $ ';'

-- Parse out the a2, a3... in [a1;a2, a3...]
repeatP :: Parser Repeat
repeatP = many . digitSep $ ','

-- Parse a FracStruc from "[a1;a2, a3...]"
fracStrucP :: Parser FracStruc
fracStrucP = char '[' *&gt;
       (firstP &gt;&gt;= \firstDig -&gt; repeatP &gt;&gt;= \rep -&gt; return $ FracStruc firstDig rep)
                   &lt;* char ']'
</pre>
I think most of this is fairly clear. The <em>dropSpace</em> uses the Applicative sequence actions where *&gt; ignores the value of the first argument and &lt;* ignores the value of the second argument, so dropping spaces 'before and after'. The <em>fracStrucP</em> parser pull this together and we can test run it in GHCI like this:
<pre class="lang:haskell decode:true ">--
--
λ-&gt; runParser fracStrucP "" "[1;2]"
Right (FracStruc {first = 1, repeat = [2]})</pre>
and we see that the number 1 goes into first and the repeat part is a list with just 2 in it. A larger one:
<pre class="lang:haskell decode:true ">--
--
λ-&gt; runParser fracStrucP "" "[1;1,2,3,4,5]"
Right (FracStruc {first = 1, repeat = [1,2,3,4,5]})</pre>
and overall the parser is fairly forgiving of spurious spaces:
<pre class="lang:haskell decode:true ">--
--
λ-&gt; runParser fracStrucP "" "[ 1 ; 1  , 2, 3,4,5   ]"
Right (FracStruc {first = 1, repeat = [1,2,3,4,5]})</pre>
<em>FracStruc</em> has enough information to supply the values of the 'a's in a CF and we're taking the 'b's to be fixed at 1. A function that takes a <em>FracStruc</em> and returns a function of Integer to Fraction is:
<pre class="lang:haskell decode:true">--
--
-- The fa function in contFrac fa fb is fully defined by the values in a FracStruc
genFa ::  FracStruc -&gt; (Integer -&gt; Fraction)
genFa (FracStruc fs rep) =
    \n -&gt; if n == 0 then Numbr fs else Numbr (g n) where
        g n = rep !! ix where
            ix = rem (fromIntegral (n - 1)) (length rep)
</pre>
The returned function is specified by the lambda expression \<em>n -&gt; ...</em> such that if <em>n</em> is 0 then the value of <em>first</em> is returned. Otherwise the <em>repeat</em> list is indexed into in a circular fashion based on <em>n</em> and the length of the <em>repeat</em> list. We can now define this function:
<pre class="lang:haskell decode:true">--
-- Create a fraction to the supplied depth using the given FracStruc
-- fb assumed fixed at 1
frac :: Integer -&gt; FracStruc -&gt; Fraction
frac depth struc@(FracStruc fs rep)
    | null rep = Numbr fs
    | otherwise = contFrac fa fb  depth where
        fa = genFa struc
        fb = const 1</pre>
The function <em>frac</em> takes a depth and a <em>FracStruc</em> and returns a <em>Fraction</em>. The 'a's are generated by <em>genFa</em> <em>struc</em> and the 'b's are fixed at 1. If there's no repeating structure  then the <em>first</em> from <em>FracStruc</em> is returned otherwise the <em>contFrac</em> (<a href="http://gitcommit.co.uk/2017/11/16/fractions-to-phi-to-fibonacci/">from the previous post</a>) is called. We are now able to write a couple of functions to create a <em>Fraction</em> from a list:
<pre class="lang:haskell decode:true">--
--
-- From the supplied list and the given depth perhaps create a Fraction
fracFromList :: Integer -&gt;  String -&gt; Maybe Fraction
fracFromList depth s =
    case makeStruct s of
        Nothing    -&gt; Nothing
        Just struc -&gt; Just $ frac depth struc

-- Runs fracStrucP to maybe make a FracStruc
makeStruct :: String -&gt; Maybe FracStruc
makeStruct txt =
    case runParser fracStrucP "" txt of
        Right s -&gt; Just s
        _       -&gt; Nothing</pre>
And now we are able to write expression like these
<pre class="lang:haskell decode:true">--
--
λ-&gt; fracFromList 25  "[1;1]"  (phi)
Just 121393/75025
*FractionParser
λ-&gt; fracFromList 25  "[1;2]"   (root 2)
Just 1855077841/1311738121</pre>
As can be seen the result is in the <em>Maybe</em> monad and we can do this sort of thing  <span class="lang:haskell decode:true crayon-inline ">evalFrac &lt;$&gt; (fracFromList 25 "[1;2]")</span>  to give <span class="lang:haskell decode:true crayon-inline ">Just 1.4142137</span>

or we can write
<pre class="lang:haskell decode:true ">--
--
evalFracM :: Integer -&gt; String -&gt; Maybe Float
evalFracM depth s = fracFromList depth s &gt;&gt;= Just . evalFrac
</pre>
so tying things back to the CF for root 2  and running in GHCI
<pre class="lang:haskell decode:true">--
--
λ-&gt; z = [evalFracM n "[1;2]" | n &lt;- [0..15]]
*FractionParser
λ-&gt; z
[Just Infinity,Just 1.0,Just 1.5,Just 1.4,Just 1.4166666,Just 1.4137931,Just 1.4142857,Just 1.4142011,Just 1.4142157,Just 1.4142132,
Just 1.4142137,Just 1.4142135,Just 1.4142135,Just 1.4142135,Just 1.4142135,Just 1.4142135]</pre>
we see the results in a monadic context. Or, if we wanted it in <em>Maybe</em> fraction form
<pre class="lang:haskell decode:true">--
--
λ-&gt; z  = [fracFromList n "[1;2]" | n &lt;- [0..15]]
*FractionParser
λ-&gt; z
[Just 1/0,Just 1/1,Just 3/2,Just 7/5,Just 17/12,Just 41/29,Just 99/70,Just 239/169,Just 577/408,Just 1393/985,Just 3363/2378,
Just 8119/5741,Just 19601/13860,Just 47321/33461,Just 114243/80782,Just 275807/195025]</pre>
And, just for phun - here's the same phing for phis with the <em>denom</em> function fmapped over them.
<pre class="lang:haskell decode:true">--
--
λ-&gt;  phis  = [fracFromList n "[1;1]" | n &lt;- [0..15]]
*FractionParser
λ-&gt; fmap (fmap denom ) phis
[Just 0,Just 1,Just 1,Just 2,Just 3,Just 5,Just 8,Just 13,Just 21,Just 34,Just 55,Just 89,Just 144,Just 233,Just 377,Just 610]</pre>
Here is the list of the roots of 1..10 in CF list format

√1 [ 1; ]
√2 [ 1; 2 ]
√3 [ 1; 1, 2 ]
√4 [ 2; ]
√5 [ 2; 4 ]
√6 [ 2; 2, 4 ]
√7 [ 2; 1, 1, 1, 4 ]
√8 [ 2; 1, 4 ]
√9 [ 3; ]
√10 [ 3; 6 ]

and ignoring the √x chars we have a list like this ["[1;]", "[1;2]", "[1;1,2]", "[2;]", "[2;4]", "[2;2,4]", "[2;1,1,1,4]", "[2;1,4]", "[3;]", "[3;6]"] and  we can map <em>evalFracM</em> over it, to a depth of say 15,  to give the square roots of 1..10 :)
<pre class="lang:haskell decode:true">--
--
λ-&gt; fmap (evalFracM 15)  ["[1;]","[1;2]","[1;1,2]","[2;]","[2;4]","[2;2,4]","[2;1,1,1,4]","[2;1,4]","[3;]","[3;6]"]
[Just 1.0,Just 1.4142135,Just 1.7320508,Just 2.0,Just 2.236068,Just 2.4494898,Just 2.6457512,Just 2.828427,Just 3.0,Just 3.1622777]</pre>
&nbsp;
<h2 style="text-align: center;">And finally</h2>
...it can be shown that

<img class="alignnone size-full wp-image-1248" src="http://gitcommit.co.uk/wp-content/uploads/2017/11/rootx1.png" alt="" width="185" height="63" />

can be written as

<img class="alignnone size-full wp-image-1249" src="http://gitcommit.co.uk/wp-content/uploads/2017/11/rootx2.png" alt="" width="293" height="155" />

clearly the CF list for this will be [1;2] and the function to generate the 'b's will simply be (x - 1). So a couple of simple function for this...
<pre class="lang:haskell decode:true ">--
--
-- A function for the 'a's for a 'fixed' FracStruc 1 [2]
fa12 :: Integer -&gt; Fraction
fa12 = genFa (FracStruc 1 [2])

-- The number and the depth
root :: Integer -&gt; Integer -&gt; Fraction
root n = contFrac fa fb where
    fa   = fa12
    fb _ = n - 1</pre>
Here we use a  'fixed' <em>FracStruc 1 [2]  </em>to create the 'a's. In the function <em>root</em> the function for the 'b's is a simple (n - 1). We can generate the roots of 1..10 by

<em>fmap evalFrac [root n 10 | n &lt;- [1..10]] </em>which gives
<h2 style="text-align: center;">[1.0,1.4142137,1.7320755,2.0000677,2.2340426,2.4503307,2.6476114,2.8319218,3.005865,3.1713445]</h2>
and is a lot simpler than the techniques discussed in the 'Root of the Problem' <a href="http://gitcommit.co.uk/2017/08/25/the-root-of-the-problem-part-1/">part 1</a> and <a href="http://gitcommit.co.uk/2017/09/15/the-root-of-the-problem-part-2/">part 2</a>.

All the code is on <a href="https://github.com/banditpig/Fractions/tree/post2">Github</a>.

Thanks for reading...!

&nbsp;