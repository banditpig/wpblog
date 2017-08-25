---
ID: 982
post_title: The Root of the Problem. Part 1.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2017/08/25/the-root-of-the-problem-part-1/
published: true
post_date: 2017-08-25 16:16:14
---
<img src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" class="alignnone size-full wp-image-317" />[latexpage]I suppose since the advent of cheap calculators the task of working out the square root of a number has become simply a matter of pressing a few buttons and reading off the answer. But there was a time when we didn't have calculators or <a href="https://en.wikipedia.org/wiki/Slide_rule">slide rules</a> &nbsp;or log tables - like these!

<img class="alignnone wp-image-983" src="http://gitcommit.co.uk/wp-content/uploads/2017/08/log-4f-201x300.gif" alt="" width="703" height="1049" />

Before all of these we used one of <a href="https://en.wikipedia.org/wiki/Methods_of_computing_square_roots">several ways</a> to calculate a square root. One technique, which I'll describe, is a bit like long division and works like this...

First off group the numbers in pairs starting from the least significant digit before any decimal point and starting from the most significant digit if there is a decimal point. An odd number of digits allows a 'pair' of just one digit. So, for example, 561 would group as 5 and 61.
Then, taking the first pair, in this case 5, find the smallest number whose square is &lt;= 5. In this case, it is 2. So, put 2 into the result R. i.e. R=2
Then square R to give 4 and subtract this from the first pair (which is 5) and bring down the next pair next to the result of this subtraction. i.e.

R=2
&nbsp;561
<u>-4
</u>&nbsp;&nbsp;1 61

Then double R to give 4 and work out the largest number N such that 4N x N &lt;= 161. i.e forty something multiplied by ‘something’ &lt;= 161.

Here N=3 and 43 x 3 = 129. Append 3 to the result R to give R=23 and subtract 129 from 161 and bring down the next pair, here there are no more pairs so we use ‘00’. This gives

R=23
&nbsp;&nbsp;161
<u>-129
</u>&nbsp;&nbsp;&nbsp;&nbsp;32 00

Now double 23 to give 46 and find N such that

46N x N &lt;= 3200. Here N=6. Append 6 to the result.

466 x 6 = 2796. Subtract 2796 from 3200 and bring down the next pair ‘00’. i.e.

R=23.6
&nbsp;&nbsp;161
<u>-129
</u>&nbsp;&nbsp;&nbsp;&nbsp;3200
&nbsp;&nbsp;&nbsp;<u>-2796</u>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;404 00

&nbsp;

Now double 23.6 ignoring the decimal. This gives 472. Now find N such that 472N x N &lt;= 404 00. This gives N as 8. Append 8 to the result. 4728 x 8 = 37824. Subtract 37824 from 404 00 and bring down the next pair, “00” and so on…

And to 2 decimals we have 23.68.

The key thing to note is that on the first pass we square the value in the result and then subtract values and on subsequent passes we double the result and subtract values. If you try a few examples with pen and paper it should become clear and in the next post I'll work through an implementation in Haskell.

Thanks for reading!