---
ID: 630
post_title: 'Haskell, Vectors and Simple Mechanics &#8211; part 4.'
author: BanditPig
post_date: 2017-06-12 19:28:09
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/06/12/haskell-vectors-and-simple-mechanics-part-4/
published: true
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />I think this post will wrap up the series on Vectors and Simple Mechanics and we'll look at Simple Harmonic Motion (SHM) and compare the numerical solutions to SHM using the naive step function from the previous post - aka the '<em>Euler</em>' step and the more accurate '<em>Euler-Cromer</em>' method.

Here's the Euler Step <a href="http://gitcommit.co.uk/2017/05/28/haskell-vectors-and-simple-mechanics-part-3/">from last time</a>.
<pre class="lang:haskell decode:true">step :: Accnf -&gt; Float -&gt; State -&gt; State
step f dt st@(t, r, v) = (t', r', v') where
    t' = t + dt             
    r' = r ^+^  v ^* dt      
    v' = v ^+^  f st ^* dt</pre>
a very simple change to the above yields the  '<em>Euler-Cromer</em>' step where the '<em>new velocity</em>' rather than the '<em>old</em>' is used to determine the '<em>new</em>' position.
<pre class="lang:haskell decode:true">ecStep :: Accnf -&gt; Float -&gt; State -&gt; State
ecStep f dt st@(t, r, v) = (t', r', v') where
    t' = t + dt             
    r' = r ^+^  v' ^* dt      
    v' = v ^+^  f st ^* dt    
</pre>
These two functions have the same signature, <span class="lang:haskell decode:true crayon-inline "> Accnf -&gt; Float -&gt; State -&gt; State</span> which allows us to generalise a solution based on a step function:
<pre class="lang:haskell decode:true">solutionWithStep :: ( Accnf -&gt; Float -&gt; State -&gt; State )-&gt;  Accnf -&gt; Float -&gt; State -&gt; [State]
solutionWithStep stp a dt = iterate (stp a dt) 
</pre>
which simply iterates the supplied step function to produce a list of <em>State</em>. This will now allow us to calculate solutions for both '<em>Euler</em>' and '<em>Euler-Cromer</em>'.

A detailed discussion of SHM can be found <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">here</a> but essentially the force is proportional to the displacement and is directed towards the origin. This translates quite easily to one of our acceleration functions as
<pre class="lang:haskell decode:true ">-- Harmonic oscillator where the force is -kx
hosc :: Scalar -&gt;  (Time, Displacement, Velocity)  -&gt; V.Vector 
hosc k (_, r , _) = (-1)*k *^ r</pre>
And, if we are to plot velocity (or displacement) against time for a given list of <em>State</em> then we need to extract the velocity (or displacement) from the <em>States</em>. This is achieved by these two simple functions:
<pre class="lang:haskell decode:true ">timeDistanceX :: State -&gt; Scalar
timeDistanceX (_, V (x, _, _), _) = x

timeDistanceXS :: [State] -&gt; Path
timeDistanceXS sts = zip [1..] (map timeDistanceX sts)</pre>
The <em>timeDistanceX </em>function simply pattern matches on the x-component of the velocity vector within the state and the <em>timeDistanceXS</em> function zips up values, while there still are values, from the result of mapping <em>timeDistanceX</em> over all <em>States</em>. (Isn't Haskell satisfying?).
Now we create the function
<pre class="lang:haskell decode:true ">oneHosc :: Color -&gt;  (Accnf -&gt; Float -&gt; State -&gt; State) -&gt; Picture
oneHosc c stp = pathPlot c . timeVelocityXS . take  50000 $ solutionWithStep stp (hosc 2.0) 0.01 (0, p0, v0) where
   p0 = V (0, 0, 0)
   v0 = V (50, 0, 0)
</pre>
which plots a path, in a given color, of a harmonic oscillator using the supplied step function. And if we are to compare the results of step functions then we need to produce a combined picture of each step function as shown in the <em>spring </em>function.
<pre class="lang:haskell decode:true ">spring :: IO ()
spring =  drawPics  [oneHosc red step,
                     oneHosc blue ecStep, 
                     color black (line [ (0, 50), (50000,  50) ])]
</pre>
In other words we'll plot the 'Euler' solution in red and the 'Euler-Cromer' output in blue along with a horizontal line showing the start state. Here are such results!

The first graph of the solutions shows the first couple of oscillations and already the '<em>Euler</em>' solution (in red) is diverging - as can be seen by the gap between it and the horizontal line.
<img class="alignnone wp-image-637" src="http://gitcommit.co.uk/wp-content/uploads/2017/06/Screen-Shot-2017-06-12-at-16.20.23-300x128.png" alt="" width="945" height="403" />

After a few more oscillations the difference becomes more pronounced whilst the '<em>Euler-Cromer</em>' solution looks to be stable.
<img class="alignnone wp-image-639" src="http://gitcommit.co.uk/wp-content/uploads/2017/06/Screen-Shot-2017-06-12-at-16.21.17-300x161.png" alt="" width="943" height="506" />
and finally we can see that the '<em>Euler</em>' solution diverges completely whilst the '<em>Euler-Cromer</em>' solution remains stable!
<img class="alignnone wp-image-640" src="http://gitcommit.co.uk/wp-content/uploads/2017/06/Screen-Shot-2017-06-12-at-16.21.43-300x173.png" alt="" width="942" height="543" />

This behaviour - i.e. the '<em>Euler</em>' solution diverging and the '<em>Euler-Cromer</em>' solution remaining stable is to be expected and there are many references and discussions about this behaviour <a href="https://www.reddit.com/r/CFD/comments/4t88de/why_is_the_eulercromer_method_more_accurate/">e.g.</a>

All the code is on <a href="https://github.com/banditpig/vectors">Github</a> and includes functions to plot a damped harmonic oscillator for both 'Euler' and 'Euler-Cromer'.  I would have liked to write the 'definitive' numerical method -<a href="https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods"> Runge-Kutta</a> but time etc. but if anyone wants to contribute this then please fork from <a href="https://github.com/banditpig/vectors">Github</a> and let me know how it goes!

Thanks for reading!