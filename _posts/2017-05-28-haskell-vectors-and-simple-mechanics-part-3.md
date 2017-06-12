---
ID: 570
post_title: 'Haskell, Vectors and Simple Mechanics &#8211; part 3.'
author: BanditPig
post_date: 2017-05-28 11:37:00
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/05/28/haskell-vectors-and-simple-mechanics-part-3/
published: true
---
[latexpage]
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />In this post we'll continue the <a href="http://gitcommit.co.uk/2017/05/20/haskell-vectors-and-simple-mechanics-part-2/">previous</a> one about vectors and take a look at calculating the path of a projectile and rendering that path to the screen.
Imagine a single particle in three dimensional space, we can characterise a state for it as its position and velocity at some instant in time.
<span class="lang:haskell decode:true crayon-inline ">type State = (Time, Displacement, Velocity) </span>

From basic mechanics and Newtons laws we know that if no forces act on it then not a lot happens really! Time will increase and depending on your point of view, not much else will change. However, if some forces are acting then things become more interesting. Knowing the forces acting is equivalent to knowing the acceleration of the particle and we can suggest that acceleration is a function of State.
<pre class="lang:haskell decode:true ">type Accnf = State -&gt; Vector</pre>
We know from simple dynamics the relationships between velocity, acceleration and displacement
<ol>
 	<li>$d' = d + vt$</li>
 	<li>$v' = v + at$</li>
</ol>
Converting this to our Vector notation and taking small time intervals we can derive a function that takes an <em>Accnf</em>  function, a small time interval and a <em>State</em> and returns a new <em>State</em>.
<pre class="lang:haskell decode:true ">step :: Accnf -&gt; Float -&gt; State -&gt; State
step f dt st@(t, r, v) = (t', r', v') where
    t' = t + dt              -- advance t by a small ammount
    r' = r ^+^  v ^* dt      -- from equation 1 above
    v' = v ^+^  f st ^* dt   -- from equation 2 above
</pre>
So for a given acceleration function, a (small) time step and a start <em>State</em> we can approximate a numerical solution with
<pre class="lang:haskell decode:true ">solution :: Accnf -&gt; Float -&gt; State -&gt; [State]
solution a dt  = iterate (step a dt)  
</pre>
this gives a lazily infinite list of <em>State</em> and so at some point we use some form  of '<em>take</em>' function to collect a list of <em>State</em> that can be further processed and plotted.

As a simple example of applying this consider a projectile fired from a gun with velocity <em>v</em> at an angle <em>theta</em> to the horizontal. A detailed analytic treatment can be found <a href="https://en.wikipedia.org/wiki/Trajectory_of_a_projectile">here</a>.

For our purposes we will ignore  air resistance and assume  a mass of 1 unit so  that the only force acting on the particle is the acceleration due to gravity - this gives
<pre class="lang:haskell decode:true ">-- Only accn due to gravity acting down
projectile :: Accnf   -- i.e. State -&gt; Vector
projectile (_, _, _) = V (0, -9.8, 0)
</pre>
To initialise the projectile with velocity <em>v</em> at angle <em>theta</em> we need to consider the horizontal and vertical components of the velocity as shown here
<pre class="lang:haskell decode:true">-- Projectile fire at angle theta with velocity v has x, y componenst v cos and v sin
projectileInit :: Float -&gt; Float -&gt; V.Vector
-- 0.0174533 is conversion factor degrees to radians
projectileInit v theta = V (v * cos (theta * 0.0174533), v * sin (theta * 0.0174533), 0)
</pre>
We can now create a solution giving a list of <em>State</em>
<pre class="lang:haskell decode:true ">-- Projectile at velocity v, angle theta. Keep evaluating while the projectile is above ground
projectileAtVandTheta :: Float -&gt; Float -&gt; [State]
projectileAtVandTheta v theta = takeWhile  heightPositive  $ solution projectile 0.01 (0, p0, projInit) where
    projInit = projectileInit v theta
    p0 = V (0, 0 ,0)</pre>
which computes a solution and just takes values while the projectile is above ground.
For display purposes I want to 'fire' several projectiles and vary the angle to the horizontal keeping the initial velocity the same. This function will do that for us
<pre class="lang:haskell decode:true ">-- calculate projectile at v theta and keep reducing theta until &lt; 0
severalProjectiles :: Float -&gt; Float -&gt; Float -&gt; [[State]]
severalProjectiles v theta dtheta 
    | theta &gt; 0 = projectileAtVandTheta v theta : severalProjectiles v (theta -  dtheta) dtheta
    | otherwise = []</pre>
Rather than go through the code function by function I'll put the 'finished' code below.
<pre class="lang:haskell decode:true">{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-type-defaults #-}

module Dynamics where
import Text.Printf
import Vectors as V
import Graphics.Gloss 
import Views

type Time = Float
type Displacement = V.Vector
type Velocity = V.Vector

type State = (Time, Displacement, Velocity) 
type Accnf = State -&gt; V.Vector

-- get the x, y component from the Displacement vector
displacement :: State -&gt; Point
displacement (_, V (x, y, _) , V (_, _, _)) = (x, y)

visualPathFromStates :: [State] -&gt; Path
visualPathFromStates  = map displacement 

-- get the x, y component from the Velocity vector
velocity :: State -&gt; Point
velocity (_, V (_, _, _) , V (velx, vely, _)) = (velx, vely)

visualVelFromStates :: [State] -&gt; Path
visualVelFromStates  = map velocity 


step :: Accnf -&gt; Float -&gt; State -&gt; State
step f dt st@(t, r, v) = (t', r', v') where
    t' = t + dt             
    r' = r ^+^  v ^* dt      
    v' = v ^+^  f st ^* dt   

solution :: Accnf -&gt; Float -&gt; State -&gt; [State]
solution a dt  = iterate (step a dt) 


compareYVal :: V.Vector -&gt; Float -&gt; (Float -&gt; Float -&gt; Bool) -&gt; Bool
compareYVal (V (_, y, _)) d p = p y d

yNZero :: V.Vector -&gt; Bool
yNZero v = compareYVal v 0 (&gt;=)

heightPositive :: State -&gt; Bool
heightPositive (_, d, _) = yNZero d

displayStates :: [State] -&gt; String
displayStates   = foldr f ""  where 
    f (t, p, v) str = str ++ "T:" ++ (printf "%.4f" t :: String) ++ " P:" ++ show p ++ " V:" ++ show v ++ "\n"


-- Projectile fire at angle theta with velocity v has x, y componenst v cos and v sin
projectileInit :: Float -&gt; Float -&gt; V.Vector
projectileInit v theta = V (v * cos (theta * 0.0174533), v * sin (theta * 0.0174533), 0)

-- Only accn due to gravity acting down
projectile :: Accnf
projectile (_, _, _) = V (0, -9.8, 0)

-- Projectile at velocity v, angle theta. Keep evaluating while the projectile is above ground
projectileAtVandTheta :: Float -&gt; Float -&gt; [State]
projectileAtVandTheta v theta = takeWhile  heightPositive  $ solution projectile 0.01 (0, p0, projInit) where
    projInit = projectileInit v theta
    p0 = V (0, 0 ,0) 

-- calculate projectile at v theta and keep reducing theta until &lt; 0
severalProjectiles :: Float -&gt; Float -&gt; Float -&gt; [[State]]
severalProjectiles v theta dtheta 
    | theta &gt; 0 = projectileAtVandTheta v theta : severalProjectiles v (theta -  dtheta) dtheta
    | otherwise = []

paths :: [[State]]  -&gt; [Path]
paths = map visualPathFromStates 

-- make a picture from each path
pathPlots :: [Path] -&gt; [Picture]
pathPlots  = map (color blue . Line )

plotSeveralProjectiles :: Float -&gt; Float -&gt; Float -&gt; IO ()
plotSeveralProjectiles v theta dtheta = drawPics . pathPlots .  paths $  severalProjectiles v theta dtheta 
 
plot :: IO ()
plot = plotSeveralProjectiles 50 90 5

</pre>
The 'plot'  function calculates and shows the path of a particle fired at angles from 90 to 5 degrees varying by 5 degrees. and here are the results!
<img class="alignnone size-medium wp-image-598" src="http://gitcommit.co.uk/wp-content/uploads/2017/05/Screen-Shot-2017-05-28-at-09.51.56-300x163.png" alt="" width="1000" height="600" />

which agrees quite well with an analytical solution that would show an angle of 45 degrees having the maximum range and complementary angles giving the same range.

And just for the pretty picture doing <span class="lang:haskell decode:true crayon-inline ">plotSeveralProjectiles 50 180 5</span> gives

<img class="alignnone size-medium wp-image-604" src="http://gitcommit.co.uk/wp-content/uploads/2017/05/Screen-Shot-2017-05-28-at-10.04.02-300x110.png" alt="" width="1800" height="600" />

And finally... In the last post I showed a function
<pre class="lang:haskell decode:true ">vecsAtOrigin :: Int -&gt; V.Vector -&gt;  IO ()
vecsAtOrigin n = 
    drawPics 
    . take n 
    . map lineVectorO
    . iterate (V.rotateXY (2*pi / fromIntegral n))</pre>
that would render<em> n</em> equally spaced vectors about the origin.

Because <em>vecsAtOrigin</em>  is a specific case of a more general one of rendering vectors about a point and then translating them all to the origin it became an 'itch' that I really needed to 'scratch'! Here's the version after scratching the itch.
<pre class="lang:haskell decode:true ">vecsAtOrigin :: Int -&gt; V.Vector -&gt;  IO ()
vecsAtOrigin n = vecsAtPos n origin
   
vecsAtPos  :: Int -&gt; V.Vector -&gt;  V.Vector -&gt; IO ()
vecsAtPos n p =
    drawPics 
        . take n
        . map (\x -&gt; lineVector p (x ^+^ p))
        . iterate (V.rotateXY (2*pi / fromIntegral n) )</pre>
and here is an example using <em>vecsAtPos</em>

<img class="alignnone size-medium wp-image-615" src="http://gitcommit.co.uk/wp-content/uploads/2017/05/Screen-Shot-2017-05-28-at-12.08.13-300x242.png" alt="" width="300" height="242" />

I think that's enough for now. <a href="http://gitcommit.co.uk/2017/06/12/haskell-vectors-and-simple-mechanics-part-4/">Next time</a> we'll maybe look at other, slightly more complex, forces and see if the naive step function is capable of handling them. As usual all the code is available of <a href="https://github.com/banditpig/vectors">GitHub</a>.

Thanks for reading!