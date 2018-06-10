---
ID: 1547
post_title: Ford gets Complex!
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/06/10/ford-gets-complex/
published: true
post_date: 2018-06-10 13:49:36
---
<img class="size-full wp-image-317 alignleft" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />[latexpage]

&nbsp;

&nbsp;

Not too complicated and just a different view of Ford's circles and a way of morphing
them along with a bit of animation.

[video poster="http://gitcommit.co.uk/wp-content/uploads/2018/06/rotated-1_thumb189.jpg" width="2880" height="1800" mp4="http://gitcommit.co.uk/wp-content/uploads/2018/06/rotated-1.mp4"][/video]

It's a continuation of the <a href="http://gitcommit.co.uk/2018/06/05/ford-and-his-circles/">previous post</a> and there are two parts to it - the real bit and the imaginary part.
<h4>The Real Part</h4>
To start with we take fractions not between 0 and 1 but rather between -n and n. A rough and ready way is
<pre class="lang:haskell decode:true ">--
fractionsN :: Integer -&gt; [Fraction]
fractionsN n = nub  [ reduce (F p q) | p &lt;- [-n..n], q &lt;- [-n..n]]</pre>
where we take all possible pairs and reduce them. Note we allow 0 as a denominator so as to be consistent with the Farey sequence.

For example
<pre class="lang:haskell decode:true ">--
λ-&gt; fractionsN 5
[1/1,5/4,5/3,5/2,5/1,-5/0,-5/1,-5/2,-5/3,-5/4,-1/1,4/5,4/3,2/1,4/1,-4/0,-4/1,-2/1,-4/3,-4/5,3/5,3/4,3/2,3/1,-3/0,-3/1,-3/2,-3/4,-3/5,2/5,1/2,2/3,-2/0,-2/3,-1/2,-2/5,1/5,1/4,1/3,-1/0,-1/3,-1/4,-1/5,0/1,0/0,1/0,2/0,3/0,4/0,5/0]</pre>
The snippets of code below will take a list of <em>Fraction</em> and generate Ford circles.
<pre class="lang:haskell decode:true">--
fordCircles :: [Fraction] -&gt;  [FordCircle]
fordCircles = fmap fordCircle

fordCircle :: Fraction -&gt; FordCircle
fordCircle  (F p 0) = (0, 0, 0)
fordCircle  (F p q) = (r, fromIntegral p / fromIntegral q, r ) where
    r = 1/fromIntegral (2*q*q)

circ :: FordCircle -&gt; Picture
circ c@(r, x, y) =  translate x y . color (newColour c) . Circle $ r

makeCircles :: ([FordCircle] -&gt; [FordCircle]) -&gt; [Fraction] -&gt;   Picture
makeCircles f =   Pictures . fmap circ . f . fordCircles
</pre>
The key function is <em>makeCircles</em> - notice that its signature is
<span class="lang:haskell decode:true crayon-inline">makeCircles :: ([FordCircle] -&gt; [FordCircle]) -&gt; [Fraction] -&gt; Picture
</span>

It takes a function that maps [<em>FordCircle</em>] to [<em>FordCircle</em>]. The purpose of this will be shown later - at the moment we can just use the identity function so that it has no effect on the list of <em>FordCircle</em>. And, using the identity function and polishing to a Gloss finish we can write:
<pre class="lang:haskell decode:true ">--
main :: IO ()
main = do
      let cs =  fractionsN  20
      display
         (InWindow "Window" (1400, 800) (0, 0))
         (greyN 0.2)
         (Pictures [scale 100 100 $ makeCircles id cs ])</pre>
with output:

<img class="wp-image-1558 aligncenter" src="http://gitcommit.co.uk/wp-content/uploads/2018/06/Screen-Shot-2018-06-10-at-10.12.22.png" alt="" width="1225" height="614" />

Very much like the Ford circles in the previous post but 'longer'. Now the for the
<h4>Imaginary Part</h4>
With a suitable import (<em>Data.Complex</em>)  we can quite easily use Complex numbers in Haskell code and doing so often makes things simpler.
<pre class="lang:haskell decode:true">--
type Cmplx = Complex Float

i :: Cmplx
i = 0 :+ 1

fz :: Cmplx -&gt; Cmplx
fz z = (z - i)/(z + i)</pre>
Here <em>z</em> is a Complex number consisting of a Real and Imaginary part - i.e. $x + iy$ and the function <em>fz</em> is a particular type of complex function (<a href="https://en.wikipedia.org/wiki/M%C3%B6bius_transformation">Mobius Transform</a>) that will map a circle onto another circle. Taking a list of circles we can determine three points on the circle, apply <em>fz</em> to each point and then determine the new circle that lies on those three points. (Does anyone know if  there's  a simpler or better way to do this?)  i.e.
<pre class="lang:haskell decode:true">--
makeCircle :: Cmplx -&gt; Cmplx -&gt; Cmplx -&gt; FordCircle
makeCircle z1 z2 z3 = (r, x, y) where
    x1 = realPart z1
    x2 = realPart z2
    x3 = realPart z3
    y1 = imagPart z1
    y2 = imagPart z2
    y3 = imagPart z3
    k = 2 * (x1 * (y2 - y3) - y1 * (x2 - x3) + x2 * y3 - x3 * y2)
    x = ((x1^2 + y1^2) * (y2 - y3) + (x2^2 + y2^2) * (y3 - y1) + (x3^2 + y3^2) * (y1 - y2)) / k
    y = ((x1^2 + y1^2) * (x3 - x2) + (x2^2 + y2^2) * (x1 - x3) + (x3^2 + y3^2) * (x2 - x1)) / k
    r = sqrt ((x - x1)^2 + (y - y1)^2)


planeMap :: [FordCircle] -&gt; [FordCircle]
planeMap  = fmap f  where
    -- radius, center x, center y
    f (r, x, y) = makeCircle (fz z1) (fz z2) (fz z3) where
        z1 = (x + r) :+ y
        z2 = (x - r) :+ y
        z3 =  x :+ (y + r)</pre>
&nbsp;

In the function <em>planeMap z1, z2</em> and <em>z3</em> are all on the circle and their image under <em>fz</em> is used to determine a new circle. The function <em>makeCircle</em> is just an exercise in <a href="http://www.ambrsoft.com/TrigoCalc/Circle3D.htm">coordinate geometry</a>. Now we can invoke <em>makeCircles</em> but this time give it <em>planeMap</em> rather than the <em>id</em> function.
<pre class="lang:haskell decode:true">main :: IO ()
main = do
      let cs =  fractionsN  20
      display
         (InWindow "Window" (1400, 800) (0, 0))
         (greyN 0.2)
         (Pictures [scale 100 100 $ makeCircles planeMap cs, translate 0 (-250) $ scale 100 100 $ makeCircles  id cs])
</pre>
Here we have the original set of circles and above them, the result of the transform :)

<img class="aligncenter size-full wp-image-1568" src="http://gitcommit.co.uk/wp-content/uploads/2018/06/Screen-Shot-2018-06-10-at-10.58.47.png" alt="" width="2654" height="1440" />
To get the animation shown at the beginning we define another complex function, <em>fzz (</em>naming things is difficult...)
<pre class="lang:haskell decode:true ">--
fzz :: Float -&gt; Cmplx -&gt; Cmplx
fzz flt z=  (z - i)/(z + i)**(flt :+ 0)</pre>
When the Float value in <em>fzz</em>  is 1.0 then <em>fzz</em> is a Mobius Transform - it is identical to our first <em>fz</em> function. When the Float value is not 1.0 then I'm not sure it has a particular name, its just a mapping :) We can apply it along with the animation capability of Gloss to create a 'morphing' of the Ford circles into the final disk.

&nbsp;
<pre class="lang:haskell decode:true">--

planeMap' :: Float -&gt; [FordCircle] -&gt; [FordCircle]
planeMap' fl = fmap f where
    f (r, x, y) = makeCircle (fzz fl z1) (fzz fl z2) (fzz fl z3) where
        z1 = (x + r) :+ y
        z2 = (x - r) :+ y
        z3 =  x :+ (y + r)

frame :: [Fraction] -&gt; Float -&gt; Picture
frame cs fl = rotate (fl*50) $ scale 250 250 $ makeCircles (planeMap' step) cs  where
  step = if fl * 0.1 &gt;= 1.0
      then 1.0
      else fl * 0.1


main :: IO ()
main = do
      let cs =  fractionsN  20
      display
         (InWindow "Window" (1400, 800) (0, 0))
         (greyN 0.2)
         (Pictures [scale 100 100 $ makeCircles planeMap cs, translate 0 (-250) $ scale 100 100 $ makeCircles  id cs])


main :: IO ()
main = do
      let cs = fractionsN 20 -- [Fraction]
      animate
         FullScreen
         (greyN 0.2)
         $ frame cs</pre>
which produce the animation at the top of the page.

&nbsp;

For added fun we can just let the value of <em>step</em> that get passed into planeMap' increase unbounded and see how the original line of circles morphs into something intriguing...

[video width="2880" height="1800" mp4="http://gitcommit.co.uk/wp-content/uploads/2018/06/morph.mp4"][/video]

&nbsp;

All the code is in <a href="https://github.com/banditpig/Farey/tree/monoidFractions">Github</a> and thanks for reading!

&nbsp;