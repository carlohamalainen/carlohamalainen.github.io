---
author: Carlo Hamalainen

date: "2014-05-06T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2014/05/06/type-safe-options-in-idris/
title: Type-safe options in Idris
url: /2014/05/06/type-safe-options-in-idris/
---

Inspiration for this blog post came from my work on [volgenmodel-nipype](https://github.com/carlohamalainen/volgenmodel-nipype), a neuroimaging workflow written using [Nipype](http://nipy.sourceforge.net/nipype/). The Nipype package allows one to wrap legacy command line applications using traits to model the various command line options. Here's a snippet for ``mincextract`` taken from [nipypeminc.py](https://github.com/carlohamalainen/volgenmodel-nipype/blob/master/nipypeminc.py):

```python
class ExtractInputSpec(StdOutCommandLineInputSpec):
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
```

Note the xor condition, which means that the user cannot set ``write_ascii`` and ``write_byte`` at the same time. So this would be ok:

```python
prog = Extract(input_file='/tmp/foo.mnc', write_ascii=True)
```

but this would be rejected:

```python
prog = Extract(input_file='/tmp/foo.mnc', write_ascii=True, write_byte=True)
```

A few times I made mistakes with these xor specifications which resulted in run time errors. Recently I learned that Haskell's type system can be used to make certain errors [compile time errors](/2013/11/13/tic-tac-toe-and-haskell-type-classes) instead of run-time errors, and I naturally wondered if an xor-type of condition could be encoded in Haskell's type system, and if not that, perhaps in Idris.

A basic implementation in Haskell of a command line wrapper might look like this:

```haskell
module OptionsInHaskell where

import Control.Monad (when)
import Data.List (intersect)
```

A command line option has a description, argument string, and a value. For simplicity we will only consider integer values.

```haskell
data Option = MkOption { optDesc :: String
                       , optArgStr :: String
                       , optValue  :: Int
                       } deriving Show
```

Again, for simplicity, two options are considered equal if their arg strings match:

```haskell
instance Eq Option where
  (MkOption _ a _) == (MkOption _ a' _) = a == a'
```

Here are some example options:

```haskell
opt1 :: Option
opt1 = MkOption "Value of Foo."   "-foo"  34

opt2 :: Option
opt2 = MkOption "Do bar."         "-bar"  99

opt3 :: Option
opt3 = MkOption "Blah."           "-blah" 0
```

A program consists of a path to a binary, a list of options, and a
list of xor conditions, which are lists of options that cannot be set simultaneously.

```haskell
data Program = Program { progPath :: FilePath
                       , progOptions :: [Option]
                       , progXorOptions :: [[Option]]
                       } deriving Show
```

A list of options has a clash if it intersects with any of the xor-lists in
more than two elements:

```haskell
clash :: [Option] -> [[Option]] -> Bool
clash opts xors = any (\x -> length (intersect opts x) >= 2) xors
```

We won't bother with the full details of spawning a process,
tidying up output files, capturing stdout and stderr, and so on,
so this ``runProgram`` function just prints some details
and checks that the options list is acceptable:

```haskell
runProgram :: Program -> IO ()
runProgram (Program path opts xorOpts) = do
  putStrLn $ "Pretending to run: " ++ path
  putStrLn $ "with options: " ++ show opts
  putStrLn $ "and xor lists: " ++ show xorOpts
  when (clash opts xorOpts) $ error "eek, options clash :("
```

Here's a program with no xor conditions; it runs ok:

```haskell
prog1 :: Program
prog1 = Program "/usr/local/bin/foo" [opt1, opt2, opt3] []
```

```
*OptionsInHaskell> runProgram prog1
Pretending to run: /usr/local/bin/foo
with options: [ MkOption {optDesc = "Value of Foo.", optArgStr = "-foo", optValue = 34}
              , MkOption {optDesc = "Do bar.", optArgStr = "-bar", optValue = 99}
              , MkOption {optDesc = "Blah.", optArgStr = "-blah", optValue = 0}
              ]
and xor lists: []
```

On the other hand, this program is not valid since options 1, 2, and 3 are set,
but the xor list specifies that options 1 and 2 cannot be set at the same time:

```haskell
prog2 :: Program
prog2 = Program "/usr/local/bin/foo" [opt1, opt2, opt3] [[opt1, opt2]]
```

```
*OptionsInHaskell> runProgram prog2
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
```

I'm not sure if we can make this a compile time error in Haskell, so I'll turn instead to Idris, where can exploit dependent types and other nice things.

First define an option data type, the equality instance, and a few examples:

```idris
module OptionsInIdris

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
```

Next we need to encode *in the type system* the result of an option list clashing or not:

```idris
data ClashValue = Clashing | NotClashing
```

Checking if an option list has a clash is basically the same as in Haskell except that we return a ``ClashValue`` instead of a ``Bool``:

```idris
notclash : List Option -> List (List Option) -> ClashValue
notclash opts xors = if (any (\x => length (intersect opts x) >= 2) xors)
                           then Clashing
                           else NotClashing
   where intersect : Eq a => List a -> List a -> List a
         intersect [] _ = []
         intersect (x :: xs) ys = if x `elem` ys then x :: intersect xs ys
                                                 else intersect xs ys
```

Next, the tricky bit. We create a data type ``IsNotClashing`` which has only one constructor, called ``Ok``, that produces a value ``IsNotClashing NotClashing``.  *There is no way to produce the value ``IsNotClashing Clashing``*.

```idris
data IsNotClashing : ClashValue -> Type where
  Ok : IsNotClashing NotClashing
```

Example values, used later:

```idris
opts123 : List Option
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
```

The heart of the solution is the ``ValidOptionList`` data type. We take
an option list and an xor list and, if a proof can be constructed for the value ``Ok``
using the expression ``IsNotClashing (notclash opts xors)``, then we produce
the actual value ``ValidOptionList opts``. Due to the definition of ``Ok``, this condition means
that ``notclash opts xors`` must evaluate to ``NotClashing``. Hopefully this makes it clear why the
data types ``ClashValue`` and ``IsNotClashing`` were needed.

```idris
data ValidOptionList : List Option -> Type where
  MkValidOptionList : (opts : List Option) -- WrappedOptionList opts
                   -> (xors : List (List Option))
                   -> {default Ok prf : IsNotClashing (notclash opts xors)}
                   -> ValidOptionList opts
```

Finally, the ``runProgram`` function takes a path to an executable and a valid list of options. *The fact that the list of options is valid is encoded in the type system*.

```idris
runProgram : String -> ValidOptionList opts -> String
runProgram binary (MkValidOptionList opts xorsHere) = "pretended to run the program with options: " ++ show opts
```

This program has options 1 and 2 set with the xor condition saying that options 2 and 3 cannot be set at the same time, so it type checks:

```idris
okProgram : String
okProgram = runProgram "/usr/local/prog" (MkValidOptionList myOptions12 myXors23)
```

On the other hand, this program with options 2 and 3 set does not type check, as expected:

```idris
notOkProgram : String
notOkProgram = runProgram "/usr/local/prog" (MkValidOptionList myOptions23 myXors23)
```

The first part of the error is a bit scary:

```
 Can't unify
         IsNotClashing NotClashing
 with
         IsNotClashing (boolElim (foldrImpl (flip (.) . flip (\x => \y => x || Delay (Prelude.Classes.Nat instance of Prelude.Classes.Ord, method > (Nat instance of Prelude.Classes.Ord, method compare (length (OptionsInIdris.notclash, intersect myOptions23 myXors23 myOptions23 y)) 2) (length (OptionsInIdris.notclash, intersect myOptions23 myXors23 myOptions23 y)) 2 || Delay (Nat instance of Prelude.Classes.Eq, method == (length (OptionsInIdris.notclash, intersect myOptions23 myXors23 myOptions23 y)) 2)))) id id myXors23 False) (Delay Clashing) (Delay NotClashing))
```

but the second part has the goods:

```
 Specifically:
         Can't unify
                 NotClashing
         with
                 Clashing
```

So there we have it. Compile-time error checking in Idris of a disjointness condition in the options for wrapping a legacy command line program.

**Further reading:**

* I stole the idea of ``Ok`` from [David Christiansen's talk on error reflection](https://gist.github.com/david-christiansen/0ead542a7f8d2ac3f689) at the Idris [developer's meeting](https://github.com/idris-lang/Idris-dev/wiki/Idris-Developers-Meeting,-April-May-2014).

* [Idris: Type safe printf](http://www.youtube.com/watch?v=fVBck2Zngjo), screencast by [Brian McKenna](https://twitter.com/puffnfresh). A similar sort of problem: printf format strings can be incorrectly specified, resulting in run-time exceptions in Haskell. Here, Brian shows how to make a type-safe printf function in Idris.

Literate source for this post: [OptionsInHaskell.lhs](https://github.com/carlohamalainen/playground/blob/master/disjoint-haskell-vs-idris/OptionsInHaskell.lhs).

Idris source: [OptionsInIdris.idr](https://github.com/carlohamalainen/playground/blob/master/disjoint-haskell-vs-idris/OptionsInIdris.idr).

