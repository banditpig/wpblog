---
ID: 503
post_title: 'Haskell, Vectors and Simple Mechanics &#8211; part 2.'
author: BanditPig
post_date: 2017-05-20 17:29:43
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/05/20/haskell-vectors-and-simple-mechanics-part-2/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />Here we will continue the <a href="http://gitcommit.co.uk/2017/05/07/haskell-vectors-and-simple-mechanics-part-1/">previous</a> ideas about vectors and take a look at basic rendering of Vectors  - for which we will use the Haskell Gloss package at <a href="https://hackage.haskell.org/package/gloss">https://hackage.haskell.org/package/gloss</a>

Gloss claims that

<em>"Gloss hides the pain of drawing simple vector graphics behind a nice data type and a few display functions. Gloss uses OpenGL under the hood, but you won't need to worry about any of that. Get something cool on the screen in under 10 minutes."</em>

and to be fair I found it very easy to use but not without problems when installing. However I believe these problems are primarily macOS related and not due to issues with Gloss. I found that no matter what I did I could not get gloss to work from within ghci - it always gave this error

<em> 'Dynamics.hs: user error (unknown GLUT entry glutInit)'</em>

But using stack to run ghci is fine if in stack.yaml you add

ghc-options:
GLUT: -optl-Wl,-framework,GLUT

and then doing 'stack ghci' everything should be ok.
<h4>Using Gloss</h4>
At the core of Gloss is the Picture type.
<pre class="lang:haskell decode:true">-- | A blank picture, with nothing in it.
blank :: Picture
blank   = Blank

-- | A convex polygon filled with a solid color.
polygon :: Path -&gt; Picture
polygon = Polygon

-- | A line along an arbitrary path.
line :: Path -&gt; Picture
line    = Line

-- | A circle with the given radius.
circle  :: Float  -&gt; Picture
circle  = Circle

-- | A circle with the given thickness and radius.
--   If the thickness is 0 then this is equivalent to `Circle`.
thickCircle  :: Float -&gt; Float -&gt; Picture
thickCircle = ThickCircle

-- Etc. Etc...


</pre>
and what's really nice is that Picture is also a Monoid and so Picture can be combined and the result is also a Picture. This allows for a simple interface for rendering images.

Here's a very simple program that will display x and y axes over a grey background in a large window.
<pre class="lang:haskell decode:true">{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
import Graphics.Gloss   
import Data.Monoid

window :: Display
window = InWindow "Window" (1400, 800) (10, 10)

background :: Color
background = greyN 0.7

axes :: Picture
axes = color red (line [ (-10000, 0), (10000,  0) ]) &lt;&gt;
       color red (line [ (0, -10000), (0,  10000) ])
      

main :: IO ()
main = display window background axes
</pre>
We have a window located at (10,10) of size (1400, 800). Its background is greyish (0 would be black and 1 would be white). The axes are two lines, both red. Both ranging from +/- 10000.  Notice that <span class="lang:haskell decode:true crayon-inline ">color red (line [ (-10000, 0), (10000, 0) ])</span> is of type Picture and so too is <span class="lang:haskell decode:true crayon-inline ">color red (line [ (0, -10000), (0, 10000) ])</span> and that they are combined with the Monoid mappend operator <span class="lang:haskell decode:true crayon-inline ">&lt;&gt;</span> to give a composite Picture.

If we are to write a Haskell module to render our vectors using Gloss then some points to note:
<ul>
 	<li>We will need functions to convert a Vector into something that Gloss understands for rendering</li>
 	<li>Vectors are 3-D and Gloss is 2-D and so we will be ignoring the z component in Vector.</li>
 	<li>A Vector has a magnitude and a direction but no intrinsic notion of position. So when rendering we will either assume the location to be the origin or supply a position vector along with the vector to render.</li>
 	<li>To indicate the direction the rendering needs to include an arrow at the relevant end.</li>
</ul>
This function
<pre class="lang:haskell decode:true">lineVector :: V.Vector -&gt; V.Vector -&gt; Picture
lineVector  pv@(V (xp, yc, _)) v@(V (x, y, _)) 
   = Line 
     [(xp, yc),  (x, y)]  
     &lt;&gt; arrowHead pv v
</pre>
takes a Vector v at position pv and creates a Line between the two points. Then, using the Monoid &lt;&gt; operator, combines the Line with an arrow head. The arrow head is provided by this function.
<pre class="lang:haskell decode:true ">arrowHead :: V.Vector -&gt; V.Vector -&gt; Picture 
arrowHead pv v = polygonFromVectors [v, v ^+^ v', v ^+^ v''] where 
    vdiff   = neg . normalise $ v ^-^ pv
    v'      = 10 *^ rotateXY (pi/8) vdiff
    v''     = 10 *^ rotateXY ( (-1)*pi/8) vdiff

</pre>
The <em>arrowHead</em> function is quite interesting and shows how using Vectors abstracts away much of the details of coordinates when dealing with points and lines in space. The <em>polygonFromVectors</em> function draws a filled in polygon using the list of Vectors as points in space. The <em>arrowHead</em> function takes the Vector v and 'shifts' it in such a way that the supplied position vector, pv, becomes the origin - i.e. <span class="lang:has kell de crayon-inline">v ^-^ pv</span>. The function <span class="lang:haskell decode:true crayon-inline ">neg . normalise</span> then 'shrinks' it to have a length of one unit and reverses it. So we have a unit vector in the opposite direction to the original. The lines
<pre class="lang:haskell decode:true ">    
 v'      = 10 *^ rotateXY (pi/8) vdiff
 v''     = 10 *^ rotateXY ( (-1)*pi/8) vdiff
</pre>
then give the 'left' and 'right' parts of the arrow and finally the three Vectors <span class="lang:haskell decode:true crayon-inline ">v, v ^+^ v', v ^+^ v''</span>, which are the three points of the arrow head, are wrapped in a list and used by <em>polygonFromVectors</em> to actually do the drawing and fill in the arrow head. The function <em>rotateXY</em> is one I added to the Vectors module described in part 1. It's just a 2-D rotation matrix expressed as a vector.
<pre class="lang:haskell decode:true ">-- rotate in the X Y plane.
rotateXY :: Scalar -&gt; Vector -&gt; Vector
rotateXY theta (V (x, y, z ))  = V (x', y', z) where
  x' = x * cos theta - y * sin theta
  y' = x * sin theta + y * cos theta</pre>
Here's the whole Haskell module.
<pre class="lang:haskell decode:true ">{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}
module Views where
import Graphics.Gloss   
import Vectors as V
import Data.Monoid


lineVector :: V.Vector -&gt; V.Vector -&gt; Picture
lineVector  pv@(V (xp, yc, _)) v@(V (x, y, _)) 
   = Line 
     [(xp, yc),  (x, y)]  
     &lt;&gt; arrowHead pv v

lineVectorO ::  V.Vector -&gt; Picture
lineVectorO = lineVector V.origin 

arrowHead :: V.Vector -&gt; V.Vector -&gt; Picture 
arrowHead pv v = polygonFromVectors [v, v ^+^ v', v ^+^ v''] where 
    vdiff   = neg . normalise $ v ^-^ pv
    v'      = 10 *^ rotateXY (pi/8) vdiff
    v''     = 10 *^ rotateXY ( (-1)*pi/8) vdiff

polygonFromVectors :: [V.Vector] -&gt; Picture
polygonFromVectors vs = Polygon pts where
    pts = map f vs where
        f (V (x, y, _) ) = (x, y)

drawIt :: [Picture] -&gt; IO ()
drawIt ps = display window background (axes &lt;&gt; mconcat ps) 

window :: Display
window = InWindow "Window" (1400, 800) (10, 10)

background :: Color
background = greyN 0.7

axes :: Picture
axes = color red (line [ (-10000, 0), (10000,  0) ]) &lt;&gt;
       color red (line [ (0, -10000), (0,  10000) ])
      
</pre>
To try this out lets define a function to repeatedly draw a vector at the origin, rotate it and repeat.
<pre class="lang:haskell decode:true ">vecsAtOrigin :: Int -&gt; V.Vector -&gt;  IO ()
vecsAtOrigin n = 
    drawPics 
    . take n 
    . map lineVectorO
    . iterate (V.rotateXY (2*pi / fromIntegral n)) 
</pre>
This works by dividing the circle up into <span class="lang:haskell decode:true crayon-inline ">(2*pi / fromIntegral n)</span> pieces and iterating the rotate function. i.e.
<span class="lang:haskell decode:true crayon-inline ">iterate (V.rotateXY (2*pi / fromIntegral n)) </span>.
We then create a Picture type from each Vector in the (lazily infinite) list of vectors using <span class="lang:haskell decode:true crayon-inline">map lineVectorO </span>.  We then take however many we want from this list using <span class="lang:haskell decode:true crayon-inline ">take n </span> and then call <span class="lang:haskell decode:true crayon-inline ">drawPics</span> to render them. Simple, elegant and pleasing function composition.
Here's some examples:
<pre class="lang:haskell decode:true ">v = V (150, 0, 0)
vecsAtOrigin 36 v</pre>
<img class="alignnone size-medium wp-image-546" src="http://gitcommit.co.uk/wp-content/uploads/2017/05/vectors-300x241.png" alt="" width="300" height="241" />

or this one showing more vectors

<img class="alignnone size-medium wp-image-548" src="http://gitcommit.co.uk/wp-content/uploads/2017/05/Screen-Shot-2017-05-20-at-17.45.22-300x244.png" alt="" width="300" height="244" />

I think that's enough for the moment and again all of this is available on <a href="https://github.com/banditpig/vectors">Github</a>. Next time we'll look at some simple  dynamics equations expressed in Vector form and look a visualising their solutions.

Thanks for reading!

&nbsp;