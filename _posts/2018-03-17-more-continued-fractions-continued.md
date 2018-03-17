---
ID: 1353
post_title: More Continued Fractions Continued.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/03/17/more-continued-fractions-continued/
published: true
post_date: 2018-03-17 12:08:32
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />In the post<strong><a href="http://gitcommit.co.uk/2017/11/22/parsers-to-fractions-to-square-roots/"> Parsers to Fractions to Square Roots!</a>  </strong>we looked at continued fractions (CFs) expressed in list format and used that format to calculate the square root of integers. The technique didn't really explore an effective way of generating the list form for a CF. In this short post we'll
<ol>
 	<li> look at how to calculate the CF  list format for the square root of an integer.</li>
 	<li>Then, from the list, calculate the convergents, each of which gives a better and better value for the square root. (The details of convergents have been covered in <strong><a href="http://gitcommit.co.uk/2018/01/31/continued-fractions-continued/">Continued Fractions Continued</a>.</strong>)</li>
</ol>
With a few helper functions,  along with items 1 and 2, we will  be able to write something like
<span class="lang:haskell decode:true crayon-inline ">take 20 . map evalFrac . convergents . cfListSqrtN $ 234</span>  which will give the first twenty approximations to the square root of 234.

Wikepedia has a very good reference and explanation of the i<a href="https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion">terative formulae for the CF list of an integer</a>.  An implementation in Haskell is shown below.
<pre class="lang:haskell decode:true">--
cfListSqrtN :: Integer -&gt; CFList
cfListSqrtN n
 | isSquare n = [floor $ sqrt (fromIntegral n :: Double)]
 | otherwise = cfListSqrtN' n ++ [a02] where
    a0 = truncate . sqrt . fromIntegral $ n
    a02 = a0 * 2
    cfListSqrtN' n = [ ai | (_, _, ai) &lt;- terms ] where
      m0 = 0
      d0 = 1
      a0 = truncate . sqrt . fromIntegral $ n
      terms = iterate nextTerm (m0, d0, a0)
      nextTerm (mi, di, ai) = (mj, dj, aj) where
            mj = di * ai - mi
            dj = (n - mj * mj) `div` di
            aj = (a0 + mj) `div` dj</pre>
The above code is slightly different to the referenced formulae  in that it handles the degenerate case of the input being a perfect square.
Also the term a02 = a0 * 2 is included  because
<ul>
 	<li>It can be shown that all CF lists for the square roots of integers are infinite and  periodic.</li>
 	<li>The period or cycle ends when the current term equals twice the first term</li>
</ul>
so <span class="lang:haskell decode:true crayon-inline ">cfListSqrtN :: Integer -&gt; CFList</span>  - which will provide an infinite list - can be modified to produce a finite implicitly periodic list. The modification involves using <em>takeWhile</em> like this (note that <em>CFList</em> is just a synonym for [<em>Integer</em>] )
<pre class="lang:haskell decode:true">--
cfListSqrtNFinite :: Integer -&gt; CFList
cfListSqrtNFinite n
 | isSquare n = [floor $ sqrt (fromIntegral n :: Double)]
 | otherwise = takeWhile (/= a02) (cfListSqrtN' n)  ++ [a02] where
    a0 = truncate . sqrt . fromIntegral $ n
    a02 = a0 * 2
    cfListSqrtN' n = [ ai | (_, _, ai) &lt;- terms ] where
      m0 = 0
      d0 = 1
      a0 = truncate . sqrt . fromIntegral $ n
      terms = iterate nextTerm (m0, d0, a0)
      nextTerm (mi, di, ai) = (mj, dj, aj) where
            mj = di * ai - mi
            dj = (n - mj * mj) `div` di
            aj = (a0 + mj) `div` dj</pre>
Both functions are essentially the same - it's just a matter of where the 'no longer taking happens' . e.g.
<pre class="lang:haskell decode:true ">--
λ-&gt; take 20 . cfListSqrtN $ 76
[8,1,2,1,1,5,4,5,1,1,2,1,16,1,2,1,1,5,4,5]
*Fractions
λ-&gt; cfListSqrtNFinite 76
[8,1,2,1,1,5,4,5,1,1,2,1,16]</pre>
As can be seen the list repeats on twice the first term i.e at 16. Which to use? It's just a question of whether you want a finite but implicitly infinite list or an explicitly infinite list. In what follows I'll use the explicit form and just take however many terms are needed.

My post <strong><a href="http://gitcommit.co.uk/2018/01/31/continued-fractions-continued/">Continued Fractions Continued</a> </strong>described how to generate the convergents from a CFList - and I've copied it in here.
<pre class="lang:haskell decode:true">--
type CFList = [Integer]
type Convergents = [Fraction]

convergents :: CFList -&gt; Convergents
convergents [x] = [F x (Numbr 1)]
convergents (a0 : a1 : as) = map (\(n, d) -&gt; F n (Numbr d) ) terms
    where
      p0 = a0
      q0 = 1
      p1 = a1 * a0 + 1
      q1 = a1
      terms = (p0, q0) : (p1, q1) : zipWith3 nextConv terms (tail terms) as
      nextConv (pi, qi) (pj, qj) ak = (pk, qk)
          where
            pk = ak * pj + pi
            qk = ak * qj + qi</pre>
So playing around with these functions...taking the first 10 convergents of the CF list for the root of 101 we have
<pre class="lang:haskell decode:true">--
λ-&gt; take 10 . convergents . cfListSqrtN $ 101
[10/1,201/20,4030/401,80801/8040,1620050/161201,
32481801/3232060,651256070/64802401,
13057603201/1299280080,261803320090/26050404001,
5249124005001/522307360100]</pre>
We have the first approximation of root 101 is 10/1, the second being 201/20 and so on. Quite quickly developing into the ratio of fairly large Integers. So just for fun if we do
<pre class="lang:haskell decode:true ">last . take 1000 . convergents . cfListSqrtN $ 101</pre>
which will give the 1000th convergent we get two very, very large Integers :)
<pre class="lang:haskell decode:true">λ-&gt; last . take 1000 . convergents . cfListSqrtN $ 101
646625510718057636609482290888627535027268481102001213587115523747319324080161345141988885295826655625346331663466632913721458027762188068392073608863051643257381479833535454226088940180415621087964583998879254147492958295108780911180067795020467516448137112696765044914443729482410320021833790527951088713106557219862334138276078109925661405024422169579987053910334976686769403924018377896571364496304136981927949836220429147430789851901468514039225011616666515189657228229082161184084461357863499707697983602729898237874510363591735175457647765784277920910204891654528621642860418956995963836866718830055759863403553320267496515018858317160719473834862470296698431023091860435728048115123631165744682694262461731527046873988147499968591556396186470485784788661987391279135411481218823756181238179749831999332086228303289961350274920432123054163009614039448032131439438368246108781540496427787758054625993295198083943531170290164107041035732362086608656785609417927115791339836817079272263669797062872421356536386417621339224570207507827414921096612558671076645452222533312110347280309933631826408879228722639580826114811459924079950642199674245937929998711326646923658684038768225819272733583333500262547993767206547220566975058698672373823608050005456175766807470226459384964236009727007865050000001
/
64341643130299528508474151885087542509310104345100962885123168072078982006714460473265233641716944933049695165274315311194920519501279461070382407398397433540955913569009479623861986916168373135474187797598254242885053970669174861286314710227088125444862732861873885579198389271987378616216567739423561153511670051884863375952497571938711067286801483327844904634104295131278892175659719844490749048453128850454661186260960222736228989707076776215031150042308861077081473425522485533772250499784765359656758519873091382278477641579905360529821474020142949619020002879250740905709012034551588289462661217157422676672668892145747060317641075796706853928275005755400998695539050224708364652697669278961017799842342024499148917636130930808612874293342538464176400415614506233774568698928712914234793446646701329277704510117519018864957135774626981398159437134688364270144467384992736527999795943878449674110689820324622891225865292499328632737999092004382535911190794084184768938903340437544195834125319915076005259973971801695625972975171135477594513784043304551586121891940957149687705005230073184677828392889867811400520627358584550788260758739512744613553616472352976563587194644936349744935915574443296900677526277263880092161654636293749649673572539359192015878136478747308153085939722339457386010000</pre>
and these are around 1300 digits each :)

Being more realistic we can evaluate the first 20 or so convergents by mapping <em>evalFrac</em> over the list like this
<pre class="lang:haskell decode:true">--
λ-&gt; fmap evalFrac . take 20  . convergents . cfListSqrtN $ 101
[10.0,10.05,10.049875,10.049875,10.049875,10.049875,10.049875,
10.049875,10.049876,10.049875,10.049875,10.049875,10.049876,
10.049875,10.049876,10.049875,10.049876,10.049875,10.049875,10.049876]
</pre>
which quite quickly converges to an accurate value - compare with Haskells sqrt function which gives 10.04987562112089

If we, for the sake of argument, say that  for a given integer the 10th convergent is <strong>the value</strong> we are accepting as the square root of the integer then we can compare each convergent to this value and see how they change and notice, quite interestingly, that the difference between <strong>the value</strong> and the convergent alternates above and below <strong>the value. </strong> To do this we use this simple function
<pre class="lang:haskell decode:true ">--
convergentDeltas :: Convergents -&gt; [(Fraction, Float)]
convergentDeltas  cs = map (\ fr -&gt; (fr, abs f - evalFrac fr)) cs where
    f = evalFrac (last cs)</pre>
and applying it
<pre class="lang:haskell decode:true ">--
λ-&gt; convergentDeltas $  take 10 . convergents . cfListSqrtN $ 345
[(18/1,0.5741749),
(19/1,-0.42582512),
(37/2,7.417488e-2),
(93/5,-2.58255e-2),
(130/7,2.746582e-3),
(873/47,-2.937317e-4),
(1003/54,1.0108948e-4),
(2879/155,-1.9073486e-5),
(3882/209,1.1444092e-5),
(6761/364,0.0)]</pre>
As can be seen the differences (second term of tuple) alternate +ve and -ve.

I had originally planned this post to be about Pells Equation and its solution in Haskell using CFs and convergents. However, central to solving Pell is the expression of the square root of an integer as a CF. And, given my previous posts about calculating square roots I thought this technique warranted its own small post. So, the next post in this series on CFs will explore Pells equation and provide a neat Haskell implementation of its solution. All this code is in <a href="https://github.com/banditpig/Fractions">GitHub</a>.

&nbsp;

Thanks for reading...!

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;