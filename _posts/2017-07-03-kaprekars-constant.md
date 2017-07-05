---
ID: 876
post_title: 'Kaprekar&#8217;s Constant.'
author: BanditPig
post_date: 2017-07-03 16:03:49
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/07/03/kaprekars-constant/
published: true
---
[latexpage]
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />Just recently I came across '<a href="https://en.wikipedia.org/wiki/6174_(number)">Kaprekar's Constant</a>' and maybe Mr Kaprekar had too much spare time... but still, it is quite interesting. The idea is to take a 4 digit number where the digits are not all the same then make the largest and smallest numbers possible with these digits, subtract the smaller from the larger then rinse and repeat with the result of the subtraction.

e.g start with 4123
\begin{align*}
4321 - 1234 = 3087 \\
8730 - 0378 = 8352 \\
8532 - 2358 = 6174 \\
7641 - 1467 = 6174 \\
... repeats...
\end{align*}

and in fact all 4 digits 'converge' to 6174!
Now this is too good an opportunity for some Haskell...

First let's take an integer and extract its digits into a list.
<pre class="lang:haskell decode:true ">toDigs :: Integer -&gt; [Integer]
toDigs = map (read . return) . show
</pre>
This looks a little terse because it is! It's in 'point free' style so there's really an implicit <em>Integer</em> argument. Dissecting it a little...What does <em>show</em> do? <span class="lang:haskell decode:true crayon-inline"> show :: a -&gt; String</span> So we're mapping <span class="lang:haskell decode:true crayon-inline ">read . return</span> over a string. What does <em>read</em> do? Well <em>read</em> is the 'opposite' of <em>show</em>. <span class="lang:haskell decode:true crayon-inline ">read :: Read a =&gt; String -&gt; a</span> The <em>return</em> function puts a value into a monadic context - in this case the list monad. So the end result is a list of digits.

Here's the rest of the code along with an implementation of the Kaprekar algorithm.
<pre class="lang:haskell decode:true ">import Control.Monad
import Data.List

toDigs :: Integer -&gt; [Integer]
toDigs = map (read . return) . show

fromDigs :: [Integer] -&gt; Integer
fromDigs = read . join . map show 

sortDesc :: (Ord a ) =&gt; [a] -&gt; [a]
sortDesc = sortBy (flip compare)

kap :: Integer -&gt; [Integer]
kap 6174 = [6174]
kap n = n : kap n' where 
    digs = toDigs n 
    n1'  = fromDigs . sortDesc $ digs
    n2'  = fromDigs . sort     $ digs
    n'   = abs (n1' - n2')
</pre>
The <em>kap</em> function is recursive and we use our prior knowledge of the fact that once the value 6174 is reached for a 4 digit input there's no need to continue - line 14. And the remainder of the algorithm is very much how I described it. We sort the digits - ascending and descending to give min and max values and then get the difference. All done recursively via this line <span class="lang:haskell decode:true crayon-inline ">kap n = n : kap n' </span>
And here are some example runs of <em>kap</em>.
<pre class="lang:haskell decode:true">λ-&gt; kap 4123
[4123,3087,8352,6174]
*Main Data.List
λ-&gt; kap 4511
[4511,4266,4176,6174]
*Main Data.List
λ-&gt; kap 9991
[9991,7992,7173,6354,3087,8352,6174]
*Main Data.List
λ-&gt; kap 1119
[1119,7992,7173,6354,3087,8352,6174]</pre>
We can in fact generalise this to numbers of more than 4 digits. i.e. We won't use '6174' as a terminating condition but rather we would test that the number to be added to a list of generated numbers has not actually been 'seen' before. This is<em> kap'</em>
<pre class="lang:haskell decode:true ">kap' :: Integer -&gt; [Integer] -&gt; [Integer]
kap' n ns
 | ns == [] = kap' n [n]
 | n' `elem` ns  = ns
 | otherwise = k
  where
    k    = kap' n'  (ns ++ [n'])         
    digs = toDigs n 
    n1'  = fromDigs . sortDesc $ digs
    n2'  = fromDigs . sort     $ digs
    n'   = abs (n1' - n2')</pre>
Very much the same except we have a different terminating condition - we check for repetition rather than a particular value - and we accumulate the values in an initially empty list. So here's a few examples.
<pre class="lang:haskell decode:true ">λ-&gt; kap' 234453432 []
[234453432,321098877,976395321,874296522,765197433,844197552,863098632,
965296431,873197622,865395432,753098643,954197541,883098612,976494321,
874197522,865296432,763197633,844296552,762098733,964395531]</pre>
And because we're using <em>Integer</em> rather than <em>Int</em> we can try some really big numbers...
<pre class="lang:haskell decode:true ">λ-&gt; kap' 2344534321239000984646473221221323523235987344232498494935344 []
[2344534321239000984646473221221323523235987344232498494935344,9998886665544333321111111111109888888888888766665544333111001,
9998777777777777776433333321109888766666653222222222222221001,9998777655555555555555433333319766666654444444444444432221001,
9988755543333332111111111111109888888888888887666666544421101,9987777777777777776543333321109888766666543222222222222222101,
9987776555555555555555433333209876666654444444444444443222101,9987655543333321111111111111109888888888888888766666544432101]</pre>
Or some <strong>really</strong> big numbers...
<pre class="lang:haskell decode:true">λ-&gt; kap' 23445343212390009846464732212213235232359873442324984949353442344534321239000984646473221221323523235987344232498494935344234453432123900098464647322122132352323598734423249849493534423445343212390009846464732212213235232359873442324984949353442344534321239000984646473221221323523235987344232498494935344234453432123900098464647322122132352323598734423249849493534423445343212390009846464732212213235232359873442324984949353442344534321239000984646473221221323523235987344232498494935344234453432123900098464647322122132352323598734423249849493534423445343212390009846464732212213235232359873442324984949353442344534321239000984646473221221323523235987344232498494935344234453432123900098464647322122132352323598734423249849493534423445343212390009846464732212213235232359873442324984949353442344534321239000984646473221221323523235987344232498494935344 []

[23445343212390009846464732212213235232359873442324984949353442344534321239000984646473221221323523235987344232498494935344234453432123900098464647322122132352323598734423249849493534423445343212390009846464732212213235232359873442324984949353442344534321239000984646473221221323523235987344232498494935344234453432123900098464647322122132352323598734423249849493534423445343212390009846464732212213235232359873442324984949353442344534321239000984646473221221323523235987344232498494935344234453432123900098464647322122132352323598734423249849493534423445343212390009846464732212213235232359873442324984949353442344534321239000984646473221221323523235987344232498494935344234453432123900098464647322122132352323598734423249849493534423445343212390009846464732212213235232359873442324984949353442344534321239000984646473221221323523235987344232498494935344,99999999999999999999999999999999999999999988888888888888888888888888888888888888888866666666666666666666666666666666666666666655555555555555555555555555554444444444444444444444444444333333333333333333333333333333333333333333333333333333332222222222222211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111099999999999999888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888777777777777776666666666666666666666666666666666666666666666666666666655555555555555555555555555554444444444444444444444444444333333333333333333333333333333333333333333111111111111111111111111111111111111111111000000000000000000000000000000000000000001,99999999999999999999999999999999999999999988888888888888777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777766666666666666444444444444443333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222211111111111111111111111111111111111111111099999999999999888888888888888888888888888888888888888888777777777777776666666666666666666666666666666666666666666666666666666666666666666666666666666666665555555555555533333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222211111111111111000000000000000000000000000000000000000001,99999999999999999999999999999999999999999988888888888888777777777777777777777777777777777777777777666666666666665555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555444444444444443333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222199999999999999777777777777776666666666666666666666666666666666666666666666666666666666666666666666666666666666665555555555555544444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444443333333333333322222222222222222222222222222222222222222211111111111111000000000000000000000000000000000000000001,99999999999999999999999999999999999999999888888888888888766666666666665555555555555555555555555555555555555555554444444444444433333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111099999999999999888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888877777777777777666666666666666666666666666666666666666666666666666666666666666666666666666666666666555555555555554444444444444444444444444444444444444444443333333333333211111111111111100000000000000000000000000000000000000001,99999999999999999999999999999999999999999888888888888887777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777766666666666666544444444444444333333333333333333333333333333333333333333333333333333333333333333333333333333333332222222222222211111111111111111111111111111111111111111099999999999999888888888888888888888888888888888888888888777777777777776666666666666666666666666666666666666666666666666666666666666666666666666666666666655555555555555433333333333333222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222221111111111111100000000000000000000000000000000000000001,99999999999999999999999999999999999999999888888888888887777777777777777777777777777777777777777776666666666666655555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555444444444444443333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222099999999999999877777777777777666666666666666666666666666666666666666666666666666666666666666666666666666666666665555555555555544444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444333333333333332222222222222222222222222222222222222222221111111111111100000000000000000000000000000000000000001,99999999999999999999999999999999999999999888888888888887666666666666665555555555555555555555555555555555555555554444444444444433333333333333333333333333333333333333333333333333333333333333333333333333333333333222222222222221111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111099999999999999888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888887777777777777766666666666666666666666666666666666666666666666666666666666666666666666666666666666555555555555554444444444444444444444444444444444444444443333333333333321111111111111100000000000000000000000000000000000000001]</pre>
So why does the sequence sooner or later hit a cycle and from then on it repeats? Well each number in the series uniquely defines the next number and as there are only a finite - 'tho possibly large number of alternatives - then sooner or later the numbers will repeat.

Well that's it really - just an interesting exercise about something that's probably not very useful! Here's the whole code in one block.
<pre class="lang:haskell decode:true ">import Control.Monad
import Data.List

toDigs :: Integer -&gt; [Integer]
toDigs = map (read . return) . show

fromDigs :: [Integer] -&gt; Integer
fromDigs = read . join . map show 

sortDesc :: (Ord a ) =&gt; [a] -&gt; [a]
sortDesc = sortBy (flip compare)

kap :: Integer -&gt; [Integer]
kap 6174 = [6174]
kap n = n : kap n' where 
    digs = toDigs n 
    n1'  = fromDigs . sortDesc $ digs
    n2'  = fromDigs . sort     $ digs
    n'   = abs (n1' - n2')


kap' :: Integer -&gt; [Integer] -&gt; [Integer]
kap' n ns
 | ns == [] = kap' n [n]
 | n' `elem` ns  = ns
 | otherwise = k
  where
    k    = kap' n'  (ns ++ [n'])         
    digs = toDigs n 
    n1'  = fromDigs . sortDesc $ digs
    n2'  = fromDigs . sort     $ digs
    n'   = abs (n1' - n2')

</pre>
The code is on Github <a href="https://github.com/banditpig/kaprekar">here</a>.

Thanks for reading...