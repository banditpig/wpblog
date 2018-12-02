---
ID: 1680
post_title: 'Haskell and F# &#8211; Advent of Code Day 1.'
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/12/02/haskell-and-f-advent-of-code-day-1/
published: true
post_date: 2018-12-02 20:34:19
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />This is the first in a series of occasional posts comparing Haskell and F# when solving the <a href="https://adventofcode.com/2018">2018 Advent of Code puzzles  </a>(AoC). One puzzle is available on each of the 25 days of December leading up to the 25th. Each puzzle is in two parts and the second part is only available once the first part has been solved!

I've been learning Haskell for a few years now - I 'got' monads about a year ago then thought  'oh duh!' . My knowledge of F# is embryonic - I've only just started - and I thought it might be informative to solve some problems using both languages.
<h2>Day 1. Part 1.</h2>
Reduces to reading in a list of integers, positive and negative and summing them. Full details are <a href="https://adventofcode.com/2018/day/1">here</a>.
<h3>In Haskell...</h3>
In Haskell reading in a file is done monadically in the IO Monad. For AoC  last year I made a small library of parser utilities to help in reading a file of ints or strings etc.  This function is at the core of it
<pre class="lang:haskell decode:true">--
withData :: FilePath -&gt; Parser a -&gt; IO a
withData path p = do
    result &lt;- parseFromFile (p &lt;* eof) path
    either (error . show) return result</pre>
Just give <em>withData</em> a suitable parser and it will return a  parsed data in the IO Monad. A suitable parser can be easily constructed in an applicative style like this
<pre class="lang:haskell decode:true ">--
parserListInt :: Parser [Int]
parserListInt = many ( int &lt;* newline)

int :: Parser Int
int = char '-' *&gt; pure ((-1)*) &lt;*&gt; natural &lt;|&gt; char '+' *&gt; pure ((1)*) &lt;*&gt; natural &lt;|&gt; natural

natural :: Parser Int
natural = pure read &lt;*&gt; many1 digit
</pre>
Which will parse from a newline separated file of strings to a list of ints with the strings having an explicit  '+' char rather than an assumed positive sign. i.e. +4 rather than 4. I wrote the above just for the fun of applicative parsing and really a simpler way is to use <em>readFile</em> then the <em>lines</em> function and finally <em>map</em> a <em>read</em> over everything whilst handling the wrinkle of an explicit '+' which confuses <em>read</em>. Like this
<pre class="lang:haskell decode:true ">--
main :: IO ()
main = do 
       contents &lt;- readFile "Day1.txt"
       let d =  map (\s -&gt; read (replace "+" "" s)  :: Int) $ lines contents
       ...</pre>
Once the input has been obtained the solution is very much a one-liner! (Well, two if the function signature is included).
<pre class="lang:haskell decode:true ">--
answer1 :: [Int] -&gt; Int
answer1 = sum</pre>
<h3>In F#...</h3>
Reading from the input file and  parsing it as  a list of integers was slightly simpler in F# as there's no need to handle the '+' char as a special case. And using the pipeline operator we can write:
<pre class="lang:haskell decode:true">--
File.ReadLines("Day1.txt") |&gt; Seq.map int |&gt; Seq.sum</pre>
Here the individual numbers (strings) are mapped over with the <em>int</em> function and are then summed. A very simple one liner!
<h2>Part 2.</h2>
Part two involved repeatedly summing the data in the list, taking each entry in turn, and looking for the first running total to appear twice. If the end of the list is reached then continue again from the start. Again, see <a href="https://adventofcode.com/2018/day/1">here</a>.
<h3>In Haskell...</h3>
The code is quite simple and a <em>Map</em> is used to maintain the individual running totals as they occur (key and value being the same). As soon as the current total is seen to be already in the Map then it terminates.  i.e.
<pre class="lang:haskell decode:true">--

itemAt :: Int -&gt; [Int] -&gt; Int
itemAt ix xs = xs !! (ix `mod` length xs)

search :: Int -&gt; Int -&gt; M.Map Int Int -&gt; [Int] -&gt; Int
search ix total seenMap xs = 
    if M.member nextVal seenMap
        then nextVal
        else search (ix + 1) nextVal (M.insert nextVal nextVal seenMap) xs  
        where nextVal = nextTotal total ix xs 
   
nextTotal :: Int -&gt; Int -&gt; [Int] -&gt; Int
nextTotal total ix xs 
    | ix == 0 = itemAt ix xs
    | otherwise = total + itemAt ix xs

answer2 :: [Int] -&gt; Int
answer2 = search 0 0 M.empty</pre>
The <em>search</em> starts with an empty map and repeatedly generates the next total using the <em>itemAt</em> function which does list lookup modulo the length of the list. The search terminates when the current total is already in the map.
<h3>In F#...</h3>
The F# version is pretty much the same.
<pre class="lang:haskell decode:true">--
let itemAt ix (arr : array&lt;'a&gt;) = arr.[ix % Array.length arr]

let nextTotal total ix arry =
    match ix with
    | 0 -&gt; itemAt ix arry
    | _ -&gt; total + itemAt ix arry

let rec search (ix : int) (total : int) (seenMap : Map&lt;int, int&gt;) list =
    let nextVal = nextTotal total ix list
    match seenMap.ContainsKey nextVal with
    | true  -&gt; nextVal
    | false -&gt; search (ix + 1) nextVal (seenMap.Add(nextVal, nextVal)) list


let answer2 =
    search 0 0 (Map.ofList []) (input |&gt; Seq.toArray)
</pre>
Well, that's about it! My initial impressions of F# are essentially positive. It is slightly more verbose than Haskell - especially the function signatures. To be honest I'm glad I learnt Haskell before F# because Haskell enforces a disciplined way of coding. F#  offers easy access to imperative style idioms and Haskell makes such crutches slightly difficult to use.

I did solve the problems in F# before using Haskell in an attempt to not just convert Haskell to F#. Being a beginner in F# I would think what I have is perhaps not too idiomatic but the idioms, with experience and learning, should come.

Thanks for reading and all the code is <a href="https://github.com/banditpig/AoC_2018_Haskell">here (Haskell)</a> and <a href="https://github.com/banditpig/AoC_2018_FSharp">here (F#)</a>.

&nbsp;

&nbsp;

&nbsp;