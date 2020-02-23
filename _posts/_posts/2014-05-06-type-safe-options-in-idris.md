---
id: 788
title: Type-safe options in Idris
date: 2014-05-06T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/05/06/type-safe-options-in-idris/
permalink: /2014/05/06/type-safe-options-in-idris/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Inspiration for this blog post came from my work on [volgenmodel-nipype](https://github.com/carlohamalainen/volgenmodel-nipype), a neuroimaging workflow written using [Nipype](http://nipy.sourceforge.net/nipype/). The Nipype package allows one to wrap legacy command line applications using traits to model the various command line options. Here’s a snippet for mincextract taken from [nipypeminc.py](https://github.com/carlohamalainen/volgenmodel-nipype/blob/master/nipypeminc.py): 

<pre>class ExtractInputSpec(StdOutCommandLineInputSpec):
    input_file = File(
                    desc='input file',
                    exists=True,
                    mandatory=True,
                    argstr='%s',
                    position=-2,)

    output_file = File(
                    desc='output file',
                    position=-1)

    _xor_write = ('write_ascii', 'write_byte',
                  'write_short', 'write_int', 'write_long',
                  'write_float', 'write_double', 'write_signed',
                  'write_unsigned',)

    write_ascii = traits.Bool(
                desc='Write out data as ascii strings (default).',
                argstr='-ascii',
                xor=_xor_write)

    write_byte = traits.Bool(
                desc='Write out data as bytes.',
                argstr='-byte',
                xor=_xor_write)

    # snipped...
</pre>

Note the xor condition, which means that the user cannot set write\_ascii and write\_byte at the same time. So this would be ok: 

<pre>prog = Extract(input_file='/tmp/foo.mnc', write_ascii=True)
</pre>

but this would be rejected: 

<pre>prog = Extract(input_file='/tmp/foo.mnc', write_ascii=True, write_byte=True)
</pre>

A few times I made mistakes with these xor specifications which resulted in run time errors. Recently I learned that Haskell’s type system can be used to make certain errors [compile time errors](/blog/2013/11/13/tic-tac-toe-and-haskell-type-classes) instead of run-time errors, and I naturally wondered if an xor-type of condition could be encoded in Haskell’s type system, and if not that, perhaps in Idris.

A basic implementation in Haskell of a command line wrapper might look like this: 

<pre>&gt; module OptionsInHaskell where
</pre>

<pre>&gt; import Control.Monad (when)
&gt; import Data.List (intersect)
</pre>

A command line option has a description, argument string, and a value. For simplicity we will only consider integer values. 

<pre>&gt; data Option = MkOption { optDesc :: String
&gt;                        , optArgStr :: String
&gt;                        , optValue  :: Int
&gt;                        } deriving Show
</pre>

Again, for simplicity, two options are considered equal if their arg strings match: 

<pre>&gt; instance Eq Option where
&gt;   (MkOption _ a _) == (MkOption _ a' _) = a == a'
</pre>

Here are some example options: 

<pre>&gt; opt1 :: Option
&gt; opt1 = MkOption "Value of Foo."   "-foo"  34
&gt;
&gt; opt2 :: Option
&gt; opt2 = MkOption "Do bar."         "-bar"  99
&gt;
&gt; opt3 :: Option
&gt; opt3 = MkOption "Blah."           "-blah" 0
</pre>

A program consists of a path to a binary, a list of options, and a list of xor conditions, which are lists of options that cannot be set simultaneously. 

<pre>&gt; data Program = Program { progPath :: FilePath
&gt;                        , progOptions :: [Option]
&gt;                        , progXorOptions :: [[Option]]
&gt;                        } deriving Show
</pre>

A list of options has a clash if it intersects with any of the xor-lists in more than two elements: 

<pre>&gt; clash :: [Option] -&gt; [[Option]] -&gt; Bool
&gt; clash opts xors = any (x -&gt; length (intersect opts x) &gt;= 2) xors
</pre>

We won’t bother with the full details of spawning a process, tidying up output files, capturing stdout and stderr, and so on, so this runProgram function just prints some details and checks that the options list is acceptable: 

<pre>&gt; runProgram :: Program -&gt; IO ()
&gt; runProgram (Program path opts xorOpts) = do
&gt;   putStrLn $ "Pretending to run: " ++ path
&gt;   putStrLn $ "with options: " ++ show opts
&gt;   putStrLn $ "and xor lists: " ++ show xorOpts
&gt;   when (clash opts xorOpts) $ error "eek, options clash :("
</pre>

Here’s a program with no xor conditions; it runs ok: 

<pre>&gt; prog1 :: Program
&gt; prog1 = Program "/usr/local/bin/foo" [opt1, opt2, opt3] []
</pre>

<pre>*OptionsInHaskell&gt; runProgram prog1
Pretending to run: /usr/local/bin/foo
with options: [ MkOption {optDesc = "Value of Foo.", optArgStr = "-foo", optValue = 34}
              , MkOption {optDesc = "Do bar.", optArgStr = "-bar", optValue = 99}
              , MkOption {optDesc = "Blah.", optArgStr = "-blah", optValue = 0}
              ]
and xor lists: []
</pre>

On the other hand, this program is not valid since options 1, 2, and 3 are set, but the xor list specifies that options 1 and 2 cannot be set at the same time: 

<pre>&gt; prog2 :: Program
&gt; prog2 = Program "/usr/local/bin/foo" [opt1, opt2, opt3] [[opt1, opt2]]
</pre>

<pre>*OptionsInHaskell&gt; runProgram prog2
Pretending to run: /usr/local/bin/foo
with options: [ MkOption {optDesc = "Value of Foo.", optArgStr = "-foo", optValue = 34}
              , MkOption {optDesc = "Do bar.", optArgStr = "-bar", optValue = 99}
              , MkOption {optDesc = "Blah.", optArgStr = "-blah", optValue = 0}
              ]
and xor lists: [ [ MkOption {optDesc = "Value of Foo.", optArgStr = "-foo", optValue = 34}
                 , MkOption {optDesc = "Do bar.", optArgStr = "-bar", optValue = 99}
                 ]
               ]
*** Exception: eek, options clash :(
</pre>

I’m not sure if we can make this a compile time error in Haskell, so I’ll turn instead to Idris, where can exploit dependent types and other nice things. 

First define an option data type, the equality instance, and a few examples: 

<pre>module OptionsInIdris

%default total

data Option = MkOption String String Int

instance Show Option where
  show (MkOption x y z) = "Option " ++ show x ++ " " ++ show y ++ " " ++ show z

instance Eq Option where
  (MkOption _ a _) == (MkOption _ a' _) = a == a'

opt1 : Option
opt1 = MkOption "Value of Foo." "-foo" 34

opt2 : Option
opt2 = MkOption "Do bar." "-bar" 99

opt3 : Option
opt3 = MkOption "Blah." "-blah" 0
</pre>

Next we need to encode _in the type system_ the result of an option list clashing or not: 

<pre>data ClashValue = Clashing | NotClashing
</pre>

Checking if an option list has a clash is basically the same as in Haskell except that we return a ClashValue instead of a Bool: 

<pre>notclash : List Option -&gt; List (List Option) -&gt; ClashValue
notclash opts xors = if (any (x =&gt; length (intersect opts x) &gt;= 2) xors)
                           then Clashing
                           else NotClashing
   where intersect : Eq a =&gt; List a -&gt; List a -&gt; List a
         intersect [] _ = []
         intersect (x :: xs) ys = if x `elem` ys then x :: intersect xs ys
                                                 else intersect xs ys
</pre>

Next, the tricky bit. We create a data type IsNotClashing which has only one constructor, called Ok, that produces a value IsNotClashing NotClashing. _There is no way to produce the value IsNotClashing Clashing_.

<pre>data IsNotClashing : ClashValue -&gt; Type where
  Ok : IsNotClashing NotClashing
</pre>

Example values, used later: 

<pre>opts123 : List Option
opts123 = [opt1, opt2, opt3]

opts12 : List Option
opts12 = [opt1, opt2]

opts13 : List Option
opts13 = [opt1, opt3]

myOptions12 : List Option
myOptions12 = [opt1, opt2]

myOptions23 : List Option
myOptions23 = [opt2, opt3]

myXors23 : List (List Option)
myXors23 = [[opt2, opt3]]
</pre>

The heart of the solution is the ValidOptionList data type. We take an option list and an xor list and, if a proof can be constructed for the value Ok using the expression IsNotClashing (notclash opts xors), then we produce the actual value ValidOptionList opts. Due to the definition of Ok, this condition means that notclash opts xors must evaluate to NotClashing. Hopefully this makes it clear why the data types ClashValue and IsNotClashing were needed.

<pre>data ValidOptionList : List Option -&gt; Type where
  MkValidOptionList : (opts : List Option) -- WrappedOptionList opts
                   -&gt; (xors : List (List Option))
                   -&gt; {default Ok prf : IsNotClashing (notclash opts xors)}
                   -&gt; ValidOptionList opts
</pre>

Finally, the runProgram function takes a path to an executable and a valid list of options. _The fact that the list of options is valid is encoded in the type system_.

<pre>runProgram : String -&gt; ValidOptionList opts -&gt; String
runProgram binary (MkValidOptionList opts xorsHere) = "pretended to run the program with options: " ++ show opts
</pre>

This program has options 1 and 2 set with the xor condition saying that options 2 and 3 cannot be set at the same time, so it type checks: 

<pre>okProgram : String
okProgram = runProgram "/usr/local/prog" (MkValidOptionList myOptions12 myXors23)
</pre>

On the other hand, this program with options 2 and 3 set does not type check, as expected: 

<pre>notOkProgram : String
notOkProgram = runProgram "/usr/local/prog" (MkValidOptionList myOptions23 myXors23)
</pre>

The first part of the error is a bit scary: 

<pre>Can't unify
         IsNotClashing NotClashing
 with
         IsNotClashing (boolElim (foldrImpl (flip (.) . flip (x =&gt; y =&gt; x || Delay (Prelude.Classes.Nat instance of Prelude.Classes.Ord, method &gt; (Nat instance of Prelude.Classes.Ord, method compare (length (OptionsInIdris.notclash, intersect myOptions23 myXors23 myOptions23 y)) 2) (length (OptionsInIdris.notclash, intersect myOptions23 myXors23 myOptions23 y)) 2 || Delay (Nat instance of Prelude.Classes.Eq, method == (length (OptionsInIdris.notclash, intersect myOptions23 myXors23 myOptions23 y)) 2)))) id id myXors23 False) (Delay Clashing) (Delay NotClashing))
</pre>

but the second part has the goods: 

<pre>Specifically:
         Can't unify
                 NotClashing
         with
                 Clashing
</pre>

So there we have it. Compile-time error checking in Idris of a disjointness condition in the options for wrapping a legacy command line program. 

 **Further reading:** 

  * I stole the idea of Ok from [David Christiansen’s talk on error reflection](https://gist.github.com/david-christiansen/0ead542a7f8d2ac3f689) at the Idris [developer’s meeting](https://github.com/idris-lang/Idris-dev/wiki/Idris-Developers-Meeting,-April-May-2014).
  * [Idris: Type safe printf](http://www.youtube.com/watch?v=fVBck2Zngjo), screencast by [Brian McKenna](https://twitter.com/puffnfresh). A similar sort of problem: printf format strings can be incorrectly specified, resulting in run-time exceptions in Haskell. Here, Brian shows how to make a type-safe printf function in Idris.

Literate source for this post: [OptionsInHaskell.lhs](https://github.com/carlohamalainen/playground/blob/master/disjoint-haskell-vs-idris/OptionsInHaskell.lhs).

Idris source: [OptionsInIdris.idr](https://github.com/carlohamalainen/playground/blob/master/disjoint-haskell-vs-idris/OptionsInIdris.idr).