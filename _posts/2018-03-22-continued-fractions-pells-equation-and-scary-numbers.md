---
ID: 1360
post_title: 'Continued Fractions, Pell&#8217;s Equation and Scary Numbers.'
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/03/22/continued-fractions-pells-equation-and-scary-numbers/
published: true
post_date: 2018-03-22 07:49:49
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />[latexpage]This is Pell's equation:

\begin{align}
x^2 -ny^2 = 1
\end{align}

where n is a positive integer that isn't a perfect square. Only integer solutions for x and y are sought and if n is not a perfect square then there are infinitely many integer solutions.

It can be shown that the convergents of the continued fraction (CF) for the square root of n contains a solution known as the Fundamental Solution (FS). In practice this fundamental solution is the first convergent that satisfies the equation under consideration. Once this solution is known then all other solutions can be calculated from a simple recurrence relationship. i.e. If the fundamental solution is (x1, y1) then
\begin{align}
x_{k+1} = x_1 x_k + n y_1 y_k \\
y_{k+1} = x_1 y_k + y_1 x_k
\end{align}

Note that a more rigorous treatment of the above formulae can be found on <a href="http://mathworld.wolfram.com/PellEquation.html">WolframMathWorld</a> and on <a href="https://en.wikipedia.org/wiki/Pell%27s_equation">Wikipedia</a>.

and now a bit of Haskell... :)

First we'll look at getting the FS and from that we can derive all others. To determine the FS we first calculate the continued fraction for root n in list format. We then get the convergents from that list. This allows us look at each convergent until we find one that is a solution of Pell's equation for the given n.  Expressing this in Haskell gives...
<pre class="lang:haskell decode:true">--
-- Pells eqn. x^2 - dy^2 = 1
notPellSolution d x y = x*x - d*y*y /= 1

-- First or fundamental solution.
solvePell :: Integer -&gt; (Integer, Integer)
solvePell d = (x, y) where
    F x (Numbr y) = head . dropWhile (\(F p (Numbr q)) -&gt; notPellSolution d p q) . convergents . cfListSqrtN $ d
</pre>
The function <em>notPellSolution</em> is self explanatory. Function <em>solvePell</em> does as described above and uses <em>dropWhile</em> with the convergents (which are Fractions) deconstructed into a numerator and a denominator. And of course <em>dropWhile</em> stops once a a solution is found. So, for example we can do
<pre class="lang:haskell decode:true ">--
λ-&gt; fmap solvePell [n | n &lt;- [1..30], not . isSquare $ n]
[(3,2),(2,1),(9,4),(5,2),(8,3),(3,1),(19,6),(10,3),(7,2),(649,180),
(15,4),(4,1),(33,8),(17,4),(170,39),(9,2),(55,12),(197,42),(24,5),
(5,1),(51,10),(26,5),(127,24),(9801,1820),(11,2)]</pre>
which gives us solutions for n=1 to 30 excluding perfect squares.

Now that we has the FS we can determines all solutions using the recurrence relationship in (2) which translates quite neatly into Haskell as
<pre class="lang:haskell decode:true ">--
solvePellAll :: Integer -&gt; [(Integer, Integer)]
solvePellAll n  = [ (x', y') | (x', y') &lt;- terms] where
    (x1, y1) = solvePell n
    terms = iterate nextTerm (x1, y1)
    nextTerm  (x, y) = (x1*x + n*y1*y, x1*y + y1*x)
</pre>
This will generate an infinite list so we need to take only what we need. For example
<pre class="lang:haskell decode:true ">--
take 10 . solvePellAll $ 2
[(3,2),(17,12),(99,70),(577,408),(3363,2378),(19601,13860),
(114243,80782),(665857,470832),(3880899,2744210),(22619537,15994428)]</pre>
The number 313 is quite interesting as its FS is quite a large number and of course subsequent solutions are larger.
<pre class="lang:haskell decode:true ">--
λ-&gt; take 10 . solvePellAll $ 313
[(32188120829134849,1819380158564160),
(2072150245021969438104715652505601,117124856755987405647781716823680),
(133397244925777070843301583468593656588455084443649,7540058082713667504003446125203741470945194284480),
(8587613275889215956205760812400537768252316119048299561978672742401,485400601250164750241979240919394389707542655611270208094258863360),
(552838267516409253358132372595800070340504977256013299427006204808514294767053621249,31248266407150014445246836522457089108081590057422041460298879378912896332616180800),
(35589649907575512949683118180592295081104812959739643203170044246509454322979616378140150639712870401,2011645949628680343938876702025571608272932504171012584239961562146859543269279641586725590870535040),(2291127902983297043932882111208285958482850013112619943323215226572383368566066417365984575746425870500545879525787649,129502205784175358078366637982063851870278500370429747185264239080252656315084479877425851084711680132306965263037120),(147494203552457422250999050769724422085238852223624448240603833808548278159192331532923238687939042367781491546074138217281388206489601,8336865294841044729797193714381361311471901035649994354226618088000595780455260077164686222498140399778891910238570509336264674654720),(9495122491087019986295751886146637957090318779289098951658921191256010350856453669766954527153318400685693030891841329782217315469739613130632764622849,536696054893128952118899495968789859806502348955287780052903169842694604177160150235843605191335524123957663766039149706059957156167842033585525637440),(611260300061089765290452358797649111747868219774099693352155112284516581380271568568955436420714147732946283933596233822458874295669215785289056537857048566408688640001,34550474926840048637975152193629767460121832182655614238934875308610922091600717475113271132871577576801099662888752644282306197379612623395665866737056545304211638400)]</pre>
In the tenth solution x is a 169 digit number and y has 168 digits!

I find it stunning that something  simple and benign looking as

\begin{align}
x^2 -313y^2 = 1
\end{align}

hides such monstrous numbers! We can open it up even more with a couple of helper functions. One to count the number of digits in a number and one to get the nth solution as (x, y) and then count the digits in each. Like this
<pre class="lang:haskell decode:true ">--
countDigits :: Integer -&gt; Integer
countDigits n = foldr (\_ a -&gt; a + 1) 0  (show n)

nthSolutionSize :: Int -&gt; Integer -&gt; (Integer, Integer)
nthSolutionSize nth n = (countDigits x, countDigits y) where
    (x, y) = last . take nth . solvePellAll $ n</pre>
We can now use the above on this innocent looking equation :)

\begin{align}
x^2 -2y^2 = 1
\end{align}

For the 10 000th solution...
<pre class="lang:haskell decode:true">--
λ-&gt; nthSolutionSize 10000 2
(7656,7656)
(0.04 secs, 111,177,848 bytes)</pre>
showing that x and y both have over 7000 digits.

Even bigger, 1 000 000th solution...
<pre class="lang:haskell decode:true">--
λ-&gt; nthSolutionSize 1000000 2
(765552,765551)
(712.01 secs, 975,124,466,168 bytes)</pre>
showing that x and y each have over three quarters of a million digits each! These aren't numbers, they're beasts from the Abyss!

The last time I counted, the universe had around 10^80 atoms - give or take -  that's an 81 digit number. And yet

\begin{align}
x^2 -2y^2 = 1
\end{align}

hides numbers that are truly incomprehensible.

All of the code is in <a href="https://github.com/banditpig/Fractions">Github</a>.

Thanks for reading...!

&nbsp;

&nbsp;

&nbsp;