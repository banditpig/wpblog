---
ID: 1319
post_title: Synacor Virtual Machine.
author: BanditPig
post_excerpt: ""
layout: post
permalink: >
  http://gitcommit.co.uk/2018/02/02/synacor-virtual-machine/
published: true
post_date: 2018-02-02 12:00:28
---
<img class="alignnone size-full wp-image-317" src="http://gitcommit.co.uk/wp-content/uploads/2017/03/refresh.png" alt="" width="128" height="128" />Just recently I came across the <a href="https://challenge.synacor.com/">'Synacor VM'</a> challenge. The problem is an interesting exercise in implementing a virtual machine using a supplied architecture definition and then running the VM using the given binary file as input. When run the program is an adventure type game which many people have completed but I have, so far, resisted that temptation. So here's the spec.
<pre class="lang:haskell decode:true">--
--
== Synacor Challenge ==
In this challenge, your job is to use this architecture spec to create a
virtual machine capable of running the included binary. Along the way,
you will find codes; submit these to the challenge website to track
your progress. Good luck!


== architecture ==
- three storage regions
- memory with 15-bit address space storing 16-bit values
- eight registers
- an unbounded stack which holds individual 16-bit values
- all numbers are unsigned integers 0..32767 (15-bit)
- all math is modulo 32768; 32758 + 15 =&gt; 5

== binary format ==
- each number is stored as a 16-bit little-endian pair (low byte, high byte)
- numbers 0..32767 mean a literal value
- numbers 32768..32775 instead mean registers 0..7
- numbers 32776..65535 are invalid
- programs are loaded into memory starting at address 0
- address 0 is the first 16-bit value, address 1 is the second 16-bit value, etc

== execution ==
- After an operation is executed, the next instruction to read is immediately after the last argument of the 
  current operation. If a jump was performed, the next operation is instead the exact destination of the jump.
- Encountering a register as an operation argument should be taken as reading from the register or setting into the register as appropriate.

== hints ==
- Start with operations 0, 19, and 21.
- Here's a code for the challenge website: mzMXMdUpOZeH
- The program "9,32768,32769,4,19,32768" occupies six memory addresses and should:
- Store into register 0 the sum of 4 and the value contained in register 1.
- Output to the terminal the character with the ascii code contained in register 0.

== opcode listing ==
halt: 0
stop execution and terminate the program
set: 1 a b
set register &lt;a&gt; to the value of &lt;b&gt;
push: 2 a
push &lt;a&gt; onto the stack
pop: 3 a
remove the top element from the stack and write it into &lt;a&gt;; empty stack = error
eq: 4 a b c
set &lt;a&gt; to 1 if &lt;b&gt; is equal to &lt;c&gt;; set it to 0 otherwise
gt: 5 a b c
set &lt;a&gt; to 1 if &lt;b&gt; is greater than &lt;c&gt;; set it to 0 otherwise
jmp: 6 a
jump to &lt;a&gt;
jt: 7 a b
if &lt;a&gt; is nonzero, jump to &lt;b&gt;
jf: 8 a b
if &lt;a&gt; is zero, jump to &lt;b&gt;
add: 9 a b c
assign into &lt;a&gt; the sum of &lt;b&gt; and &lt;c&gt; (modulo 32768)
mult: 10 a b c
store into &lt;a&gt; the product of &lt;b&gt; and &lt;c&gt; (modulo 32768)
mod: 11 a b c
store into &lt;a&gt; the remainder of &lt;b&gt; divided by &lt;c&gt;
and: 12 a b c
stores into &lt;a&gt; the bitwise and of &lt;b&gt; and &lt;c&gt;
or: 13 a b c
stores into &lt;a&gt; the bitwise or of &lt;b&gt; and &lt;c&gt;
not: 14 a b
stores 15-bit bitwise inverse of &lt;b&gt; in &lt;a&gt;
rmem: 15 a b
read memory at address &lt;b&gt; and write it to &lt;a&gt;
wmem: 16 a b
write the value from &lt;b&gt; into memory at address &lt;a&gt;
call: 17 a
write the address of the next instruction to the stack and jump to &lt;a&gt;
ret: 18
remove the top element from the stack and jump to it; empty stack = halt
out: 19 a
write the character represented by ascii code &lt;a&gt; to the terminal
in: 20 a
read a character from the terminal and write its ascii code to &lt;a&gt;; it can be assumed that once
input starts, it will continue until a newline is encountered; this means that you can safely 
read whole lines from the keyboard and trust that they will be fully read
noop: 21
no operation

</pre>
&nbsp;

I had a couple of attempts at this - in Haskell - and  the main thing I learnt from this was...

Yes, the Haskell type system is very expressive and allows for a sophisticated modelling of a problem. However too much can lead to unnecessarily complex programs and a very confused developer!

At first I tried 'modelling everything' - registers, memory, stack, program counters, ops, instructions.... and so on! I really couldn't see the wood for the trees.

So, after deleting everything and then using git to restore - I didn't want to type in all the ops again! I tried a much simpler representation.
<ul>
 	<li>Memory - uses a Map - ok it was flattened to a list occasionally but that's ok.</li>
 	<li>Stack - a simple list will do fine.</li>
 	<li>Extracting operations and parameters out of memory - no, you don't need Parsers. Pattern matching is perfect for this and simple.</li>
 	<li>Registers - yes a Sum type seemed a good idea - but really registers are just part of the memory (Map).</li>
</ul>
The operations, <em>Push</em>, <em>Pop</em>, etc I modelled as a Sum type each with 0, 1, 2 or 3 Word16s as appropriate. That way the 'parsing' is a simple pattern match using the operations code and the number of expected parameters. Here's what I mean.
<pre class="lang:haskell decode:true">--
--
data Operation =
    Halt                      | --  0        = stop execution and terminate the program
    Set  Word16 Word16        | --  1 a b    = set register &lt;a&gt; to the value of &lt;b&gt;
    Push Word16               | --  2 a      = push &lt;a&gt; onto the stack
    Pop  Word16               | --  3 a      = remove the top element from the stack and write it into &lt;a&gt;; empty stack = error
    Eq   Word16 Word16 Word16 | --  4 a b c  = set &lt;a&gt; to 1 if &lt;b&gt; is equal to &lt;c&gt;; set it to 0 otherwise
    Gt   Word16 Word16 Word16 | --  5 a b c  = set &lt;a&gt; to 1 if &lt;b&gt; is greater than &lt;c&gt;; set it to 0 otherwise
    Jmp  Word16               | --  6 a      = jump to &lt;a&gt;
    Jt   Word16 Word16        | --  7 a b    = if &lt;a&gt; is nonzero, jump to &lt;b&gt;
    Jf   Word16 Word16        | --  8 a b    = if &lt;a&gt; is zero, jump to &lt;b&gt;
    Add  Word16 Word16 Word16 | --  9 a b c  = assign into &lt;a&gt; the sum of &lt;b&gt; and &lt;c&gt; (modulo 32768)
    Mult Word16 Word16 Word16 | --  10 a b c = store into &lt;a&gt; the product of &lt;b&gt; and &lt;c&gt; (modulo 32768)
    Mod  Word16 Word16 Word16 | --  11 a b c = store into &lt;a&gt; the remainder of &lt;b&gt; divided by &lt;c&gt;
    And  Word16 Word16 Word16 | --  12 a b c = stores into &lt;a&gt; the bitwise and of &lt;b&gt; and &lt;c&gt;
    Or   Word16 Word16 Word16 | --  13 a b c = stores into &lt;a&gt; the bitwise or of &lt;b&gt; and &lt;c&gt;
    Not  Word16 Word16        | --  14 a b   = stores 15-bit bitwise inverse of &lt;b&gt; in &lt;a&gt;
    Rmem Word16 Word16        | --  15 a b   = read memory at address &lt;b&gt; and write it to &lt;a&gt;
    Wmem Word16 Word16        | --  16 a b   = write the value from &lt;b&gt; into memory at address &lt;a&gt;
    Call Word16               | --  17 a     = write the address of the next instruction to the stack and jump to &lt;a&gt;
    Ret                       | --  18       = remove the top element from the stack and jump to it; empty stack = halt
    Out  Word16               | --  19 a     = write the character represented by ascii code &lt;a&gt; to the terminal
    In   Word16               | --  20 a     = read a character from the terminal and write its ascii code to &lt;a&gt;; 
                                --             it can be assumed that once input starts, it will continue until a newline is encountered; 
                                --             this means that you can safely read whole lines from the keyboard and trust that they will be fully read
    Noop                       --  21        = no operation
     deriving (Show, Eq)
</pre>
Then extracting successive operations from memory was just a matter of applying a patter matching function on the memory (in list format) at a location given by the program counter.

Here's the parse function.
<pre class="lang:haskell decode:true">--
--
parseOperation :: [Word16] -&gt; (Operation, Int)
parseOperation ( 0:xs      ) = (Halt      , 0)
parseOperation ( 1:a:b:xs  ) = (Set    a b, 2)
parseOperation ( 2:a:xs    ) = (Push     a, 1)
parseOperation ( 3:a:xs    ) = (Pop      a, 1)
parseOperation ( 4:a:b:c:xs) = (Eq   a b c, 3)
parseOperation ( 5:a:b:c:xs) = (Gt   a b c, 3)
parseOperation ( 6:a:xs    ) = (Jmp      a, 1)
parseOperation ( 7:a:b:xs  ) = (Jt     a b, 2)
parseOperation ( 8:a:b:xs  ) = (Jf     a b, 2)
parseOperation ( 9:a:b:c:xs) = (Add  a b c, 3)
parseOperation (10:a:b:c:xs) = (Mult a b c, 3)
parseOperation (11:a:b:c:xs) = (Mod  a b c, 3)
parseOperation (12:a:b:c:xs) = (And  a b c, 3)
parseOperation (13:a:b:c:xs) = (Or   a b c, 3)
parseOperation (14:a:b:xs  ) = (Not    a b, 2)
parseOperation (15:a:b:xs  ) = (Rmem   a b, 2)
parseOperation (16:a:b:xs  ) = (Wmem   a b, 2)
parseOperation (17:a:xs    ) = (Call     a, 1)
parseOperation (18:xs      ) = (Ret       , 0)
parseOperation (19:a:xs    ) = (Out      a, 1)
parseOperation (20:a:xs    ) = (In       a, 1)
parseOperation (21:xs      ) = (Noop      , 0)
</pre>
The stack, memory etc I represented like this:
<pre class="lang:haskell decode:true ">--
--
type Memory = M.Map Word16 Word16
data CPU = CPU {
            mem   :: Memory,
            pc    :: Int,
        currentOp :: Operation,
            stack :: [Word16] } deriving Show
</pre>
&nbsp;

Running the program - ignoring details of  initialisation - resolved to 'running' the cpu until the <em>Halt</em> instruction is found. i.e.
<pre class="lang:haskell decode:true">--
--
execute :: CPU -&gt; IO CPU
execute cpu@(CPU _ _ Halt _)    = return cpu
execute cpu@(CPU mem pc op stk) = executeNextOp cpu &gt;&gt;= execute
</pre>
and the function <em>executeNextOp</em> gets the next instruction from memory using the <em>parseOperation</em> function and then executes it.

There are a few helper functions for reading/writing memory etc. and type conversions but the whole program is around 250 lines and is shown below.
<pre class="lang:haskell decode:true ">--
--
import           Control.Monad
import           Data.Binary.Get
import           Data.Bits
import           Data.ByteString.Lazy (ByteString, length, readFile)
import qualified Data.ByteString.Lazy as BSL
import           Data.Int
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Vector          as V
import           Data.Word
import           Foreign.Storable


data Operation =
    Halt                      | --  0        = stop execution and terminate the program
    Set  Word16 Word16        | --  1 a b    = set register &lt;a&gt; to the value of &lt;b&gt;
    Push Word16               | --  2 a      = push &lt;a&gt; onto the stack
    Pop  Word16               | --  3 a      = remove the top element from the stack and write it into &lt;a&gt;; empty stack = error
    Eq   Word16 Word16 Word16 | --  4 a b c  = set &lt;a&gt; to 1 if &lt;b&gt; is equal to &lt;c&gt;; set it to 0 otherwise
    Gt   Word16 Word16 Word16 | --  5 a b c  = set &lt;a&gt; to 1 if &lt;b&gt; is greater than &lt;c&gt;; set it to 0 otherwise
    Jmp  Word16               | --  6 a      = jump to &lt;a&gt;
    Jt   Word16 Word16        | --  7 a b    = if &lt;a&gt; is nonzero, jump to &lt;b&gt;
    Jf   Word16 Word16        | --  8 a b    = if &lt;a&gt; is zero, jump to &lt;b&gt;
    Add  Word16 Word16 Word16 | --  9 a b c  = assign into &lt;a&gt; the sum of &lt;b&gt; and &lt;c&gt; (modulo 32768)
    Mult Word16 Word16 Word16 | --  10 a b c = store into &lt;a&gt; the product of &lt;b&gt; and &lt;c&gt; (modulo 32768)
    Mod  Word16 Word16 Word16 | --  11 a b c = store into &lt;a&gt; the remainder of &lt;b&gt; divided by &lt;c&gt;
    And  Word16 Word16 Word16 | --  12 a b c = stores into &lt;a&gt; the bitwise and of &lt;b&gt; and &lt;c&gt;
    Or   Word16 Word16 Word16 | --  13 a b c = stores into &lt;a&gt; the bitwise or of &lt;b&gt; and &lt;c&gt;
    Not  Word16 Word16        | --  14 a b   = stores 15-bit bitwise inverse of &lt;b&gt; in &lt;a&gt;
    Rmem Word16 Word16        | --  15 a b   = read memory at address &lt;b&gt; and write it to &lt;a&gt;
    Wmem Word16 Word16        | --  16 a b   = write the value from &lt;b&gt; into memory at address &lt;a&gt;
    Call Word16               | --  17 a     = write the address of the next instruction to the stack and jump to &lt;a&gt;
    Ret                       | --  18       = remove the top element from the stack and jump to it; empty stack = halt
    Out  Word16               | --  19 a     = write the character represented by ascii code &lt;a&gt; to the terminal
    In   Word16               | --  20 a     = read a character from the terminal and write its ascii code to &lt;a&gt;; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
    Noop                       --  21      = no operation
     deriving (Show, Eq)

-- ------------------------------------------------
parseOperation :: [Word16] -&gt; (Operation, Int)
parseOperation ( 0:xs      ) = (Halt      , 0)
parseOperation ( 1:a:b:xs  ) = (Set    a b, 2)
parseOperation ( 2:a:xs    ) = (Push     a, 1)
parseOperation ( 3:a:xs    ) = (Pop      a, 1)
parseOperation ( 4:a:b:c:xs) = (Eq   a b c, 3)
parseOperation ( 5:a:b:c:xs) = (Gt   a b c, 3)
parseOperation ( 6:a:xs    ) = (Jmp      a, 1)
parseOperation ( 7:a:b:xs  ) = (Jt     a b, 2)
parseOperation ( 8:a:b:xs  ) = (Jf     a b, 2)
parseOperation ( 9:a:b:c:xs) = (Add  a b c, 3)
parseOperation (10:a:b:c:xs) = (Mult a b c, 3)
parseOperation (11:a:b:c:xs) = (Mod  a b c, 3)
parseOperation (12:a:b:c:xs) = (And  a b c, 3)
parseOperation (13:a:b:c:xs) = (Or   a b c, 3)
parseOperation (14:a:b:xs  ) = (Not    a b, 2)
parseOperation (15:a:b:xs  ) = (Rmem   a b, 2)
parseOperation (16:a:b:xs  ) = (Wmem   a b, 2)
parseOperation (17:a:xs    ) = (Call     a, 1)
parseOperation (18:xs      ) = (Ret       , 0)
parseOperation (19:a:xs    ) = (Out      a, 1)
parseOperation (20:a:xs    ) = (In       a, 1)
parseOperation (21:xs      ) = (Noop      , 0)

-- - numbers 0..32767 mean a literal value
-- - numbers 32768..32775 instead mean registers 0..7
-- - numbers 32776..65535 are invalid
-- ------------------------------------------------

type Memory = M.Map Word16 Word16
data CPU = CPU {
            mem   :: Memory,
            pc    :: Int,
        currentOp :: Operation,
            stack :: [Word16] } deriving Show

maxAddress :: Word16
maxAddress = 32767
-- ------------------------------------------------
rawContent :: ByteString -&gt; [Word16]
rawContent contents = runGet (Prelude.map fromIntegral `fmap` replicateM count getInt16le) contents where
    count = fromIntegral (BSL.length contents) `div` sizeOf (1 :: Word16)


readWord :: Memory -&gt; Word16 -&gt; Word16
readWord mem ix
    | ix &lt;= maxAddress = ix     -- literal
    | otherwise =  (M.!) mem ix -- register

writeWord ::  Word16 -&gt; Word16 -&gt; Memory -&gt; Memory
writeWord = M.insert

printChar :: Word16 -&gt; IO ()
printChar n = putChar (toEnum' n') where
    n' = fromIntegral n::Int
    toEnum' n = toEnum n :: Char

-- 32768..32775 - regs
initMem :: [Word16] -&gt; Memory
initMem ints = M.union m1 m2 where
   m1 = M.fromList . zip [0..] $ ints
   m2 = M.fromList . zip [32768..32775] $ repeat 0

toInt :: Word16 -&gt; Int
toInt w = fromIntegral  w :: Int

execute :: CPU -&gt; IO CPU
execute cpu@(CPU _ _ Halt _)    = return cpu
execute cpu@(CPU mem pc op stk) = executeNextOp cpu &gt;&gt;= execute

executeNextOp cpu@(CPU mem pc op stk) =
    case op'  of
        Halt  -&gt; return $ CPU mem (pc + pc' + 1) Halt stk

        Noop  -&gt; return $ CPU mem (pc + pc' + 1) Noop stk
        -- set register &lt;a&gt; to the value of &lt;b&gt;
        Set a b -&gt;  return $ CPU mem' (pc + pc' + 1) op' stk where
                        mem' = writeWord a (readWord mem b) mem

        -- push &lt;a&gt; onto the stack
        Push a  -&gt;  return $ CPU mem (pc + pc' + 1) op' stk' where
                        stk' = readWord mem a : stk

        -- remove the top element from the stack and write it into &lt;a&gt;; empty stack = error
        Pop a   -&gt; return $ CPU mem' (pc + pc' + 1) op' stk' where
                        (val:stk')  = stk
                        mem' = writeWord a val mem

        -- set &lt;a&gt; to 1 if &lt;b&gt; is equal to &lt;c&gt;; set it to 0 otherwise
        Eq a b c -&gt; return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = if readWord mem b == readWord mem c
                                then 1
                                else 0
                        mem' = writeWord a val mem


        -- set &lt;a&gt; to 1 if &lt;b&gt; is greater than &lt;c&gt;; set it to 0 otherwise
        Gt a b c -&gt; return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = if readWord mem b &gt; readWord mem c
                            then 1
                            else 0
                        mem' = writeWord a val mem

        -- jump to &lt;a&gt;
        Jmp a    -&gt; return $ CPU mem jmp op' stk where -- maybe use Word16 as program counter
                        jmp = fromIntegral (readWord mem a)::Int


        -- if &lt;a&gt; is nonzero, jump to &lt;b&gt;
        Jt a b   -&gt;  return $ CPU mem jmp op' stk where
                        jmp = if toInt (readWord mem a) /= 0
                                    then toInt (readWord mem b)
                                    else pc + pc' + 1


        -- if &lt;a&gt; is zero, jump to &lt;b&gt;
        Jf a b   -&gt; return $ CPU mem jmp op' stk where
                        jmp = if toInt (readWord mem a) == 0
                            then toInt (readWord mem b)
                            else pc + pc' + 1


        -- assign into &lt;a&gt; the sum of &lt;b&gt; and &lt;c&gt; (modulo 32768)
        Add a b c -&gt; return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = (readWord mem b + readWord mem c) `mod` (32768::Word16)
                        mem' = writeWord a val mem


        -- store into &lt;a&gt; the product of &lt;b&gt; and &lt;c&gt; (modulo 32768)
        Mult a b c -&gt; return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = (readWord mem b * readWord mem c) `mod` (32768::Word16)
                        mem' = writeWord a val mem


        -- a b c = store into &lt;a&gt; the remainder of &lt;b&gt; divided by &lt;c&gt;
        Mod a b c  -&gt; return $ CPU mem' (pc + pc' + 1) op' stk where
                         val = readWord mem b `rem` readWord mem c
                         mem' = writeWord a val mem

        -- a b c = stores into &lt;a&gt; the bitwise and of &lt;b&gt; and &lt;c&gt;
        And a b c  -&gt; return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = (.&amp;.) (readWord mem b)  (readWord mem c)
                        mem' = writeWord a val mem


        -- stores into &lt;a&gt; the bitwise or of &lt;b&gt; and &lt;c&gt;
        Or a b c  -&gt; return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = (.|.) (readWord mem b)  (readWord mem c)
                        mem' = writeWord a val mem

        -- stores 15-bit bitwise inverse of &lt;b&gt; in &lt;a&gt;
        Not a b   -&gt; return $ CPU mem' (pc + pc' + 1) op' stk where
                        val = complement (readWord mem b) `xor` (32768 :: Word16)
                        mem' = writeWord a val mem

        -- read memory at address &lt;b&gt; and write it to &lt;a&gt;
        Rmem a b  -&gt;  return $ CPU mem' (pc + pc' + 1) op' stk where
                        isReg = maxAddress &lt; b
                        ix = if isReg then readWord mem b else b
                        val = (M.!) mem ix
                        mem' = writeWord a val mem

        -- write the value from &lt;b&gt; into memory at address &lt;a&gt;
        Wmem a b  -&gt; return $ CPU mem' (pc + pc' + 1) op' stk where
                        isReg = maxAddress &lt; a
                        a' = if isReg then readWord mem a  else a
                        b' = readWord mem b
                        mem' = writeWord a' b' mem

        -- write the address of the next instruction to the stack and jump to &lt;a&gt;
        Call a    -&gt; return $ CPU mem (toInt newPc) op' stk' where
                            stk' = (fromIntegral (pc + pc' + 1)::Word16):stk
                            newPc = readWord mem a

        -- remove the top element from the stack and jump to it; empty stack = halt
        Ret         -&gt;  if stk == []
                            then
                                return $ CPU mem 0 Halt stk
                            else do
                                let (val:stk') = stk
                                return $ CPU mem (toInt val) op' stk'

        -- write the character represented by ascii code &lt;a&gt; to the terminal
        Out a       -&gt; printChar (readWord mem a) &gt;&gt; return (CPU mem (pc + pc' + 1) op' stk)


        -- read a character from the terminal and write its ascii code to &lt;a&gt;;
        -- it can be assumed that once input starts,
        -- it will continue until a newline is encountered;
        -- this means that you can safely read whole lines from the
        -- keyboard and trust that they will be fully read
        In a        -&gt; do
                        c &lt;- getChar
                        let mem' = writeWord a (fromIntegral (fromEnum c)::Word16) mem
                        return $ CPU mem' (pc + pc' + 1) op' stk
        where
            (op', pc') = nextOp pc mem
            nextOp :: Int -&gt; Memory -&gt; (Operation, Int)
            nextOp ix = parseOperation . map snd . drop ix . M.toList

main = do
    contents &lt;- BSL.readFile "challenge.bin"
    let mem  = initMem . rawContent $ contents
    let cpu = CPU mem 0 Noop []
    execute cpu
    print "Done."
</pre>
When the code is run a number of self tests are executed so some feedback is given on correctness and once all the self tests complete correctly this dialog appears :)

&nbsp;

<strong><em>== Foothills ==</em></strong>
<strong><em> You find yourself standing at the base of an enormous mountain. At its base to the north, there is a massive doorway. A sign nearby reads "Keep out! Definitely no treasure within!"</em></strong>

<strong><em>Things of interest here:</em></strong>
<strong><em> - tablet</em></strong>

<strong><em>There are 2 exits:</em></strong>
<strong><em> - doorway</em></strong>
<strong><em> - south</em></strong>

<strong><em>What do you do?</em></strong>

Overall I really enjoyed this problem especially once I'd realised that less is often more when modelling a problem.

The code is <a href="https://github.com/banditpig/SynacorVM">here on Github</a>.

And thanks for reading!

&nbsp;