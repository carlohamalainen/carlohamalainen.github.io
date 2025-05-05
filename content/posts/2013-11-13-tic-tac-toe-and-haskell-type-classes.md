---
author: Carlo Hamalainen

date: "2013-11-13T00:00:00Z"
format: image
title: Tic-tac-toe and Haskell type classes
url: /2013/11/13/tic-tac-toe-and-haskell-type-classes/
---
(Literate Haskell source for this post is here: [https://github.com/carlohamalainen/playground/tree/master/haskell/tic_tac_toe](https://github.com/carlohamalainen/playground/tree/master/haskell/tic_tac_toe).)

In a [blog post in 2011](http://blog.tmorris.net/posts/understanding-practical-api-design-static-typing-and-functional-programming/) Tony Morris set an exercise to write an API for the game tic-tac-toe that satisfies these requirements:

  1. If you write a function, I must be able to call it with the same arguments and always get the same results, forever.
  2. If I, as a client of your API, call one of your functions, I should always get a sensible result. Not null or an exception or other backdoors that cause the death of millions of kittens worldwide.
  3. If I call move on a tic-tac-toe board, but the game has finished, I should get a **compile-time type-error**. In other words, calling move on inappropriate game states (i.e. move doesn’t make sense) is disallowed by the types.
  4. If I call takeMoveBack on a tic-tac-toe board, but no moves have yet been made, I get a **compile-time** type-error.
  5. If I call whoWonOrDraw on a tic-tac-toe board, but the game hasn’t yet finished, I get a **compile-time** type-error.
  6. I should be able to call various functions on a game board that is in any state of play e.g. isPositionOccupied works for in-play and completed games.
  7. It is not possible to play out of turn.

I remember when I first saw this list of rules that numbers 3 and 4 stood out to me. How on earth could it be possible to make these _compile-time_ errors?

In Python the standard implementation for a tic-tac-toe game would use a class containing the board state along with methods move, takeMoveBack, and so on. Calling one of these functions with an invalid state would throw an exception:

```haskell
class TicTacToe:
    ...

    def move(self, position):
        if self.game_finished():
            raise ValueError, "Can't move on a finished board."
        else:
            ...

    def takeMoveBack(self, position):
        if self.is_empty_board():
            raise ValueError, "Can't take back a move on an empty board."
        else:
            ...
```

A crazy user of the TicTacToe API might write code like this (intentionally or not):

```python
t = TicTacToe()

t.move('NW') # player 1 marks the North-West square

if random.random() < 1e-10:
    print t.whoWonOrDraw() # raises an exception as the game is not finished
```

There are ways to solve this problem in C#, F#, OCaml, Java, Scala, and Haskell. Of those langauges I am most familiar with Haskell so the following will focus exclusively on a solution using Haskell’s [type classes](http://en.wikipedia.org/wiki/Type_class).

Solving the tic-tac-toe problem requires a bit of code for dealing with the rules of the game itself, but what I want to focus on is how to enforce rules like 3 and 4 in a small system. So here is a reduced problem:

  1. The system has two states: 0 and 1.
  2. In either state, the system stores a single Integer.
  3. The only valid transition is from state 0 to state 1. Attempting to move from state 1 to state 0 should be a **compile-time** error.
  4. In either state, we can call pprint to get a String representation of the state.

First, define data types for the two states:

```haskell
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module TicTacToe where

data State0 = State0 Int deriving Show
data State1 = State1 Int deriving Show
```

Now create a class ``StateLike`` to enforce the definition of a pretty-printing function pprint:

```haskell
class StateLike b where
    pprint :: b -> String

instance StateLike State0 where
    pprint (State0 n) = "Initial state; " ++ show n

instance StateLike State1 where
    pprint (State1 n) = "Final state; " ++ show n
```

In ghci:

```
*TicTacToe> pprint $ State0 42
"Initial state; 42"
*TicTacToe> pprint $ State1 59
"Final state; 59"
```

There’s nothing too fancy so far.

Next we need to enforce rule 3, which says that the only transition is from state 0 to state 1. We would like to write

```haskell
class Transition where
  move :: Int -> State0 -> State1
```

but this does not define a _class_ and ghci complains accordingly:

```
No parameters for class `Transition'
    In the class declaration for `Transition'
```

We can make this a class by replacing ``State0`` and ``State1`` with variables:

```haskell
class Transition a b where
  move :: Int -> a -> b
```

but this still doesn’t make ghci happy. Previously we had no free variable and now we have two, so being a little bit psychic we can can add a [functional dependency](http://www.haskell.org/haskellwiki/Functional_dependencies) to indicate that b is completely determined by a:

```haskell
class Transition a b | a -> b where
    move :: Int -> a -> b
```

This code will now compile. Finally, we provide an instance for ``Transition State0 State1``:

```haskell
instance Transition State0 State1 where
    move i (State0 n) = State1 (n + i)
```

where the new state’s integer component is just the addition of the previous state and the parameter ``i`` supplied to move.

Now we check each of the rules:

**Rule 1:** The system has two states: 0 and 1.

We defined the two data constructors ``State0`` and ``State1``:

```
*TicTacToe> State0 10
State0 10
*TicTacToe> State1 20
State1 20
```

**Rule 2:** In either state, the system stores a single Integer.

We stored 10 and 20 in the previous answer.

**Rule 3:** The only valid transition is from state 0 to state 1. Attempting to move from state 1 to state 0 should be a **compile-time** error.

Attempting to make a move from State0 is acceptable, and returns a State1:

```
*TicTacToe> :t move 3 (State0 42)
move 3 (State0 42) :: State1

*TicTacToe> pprint $ move 3 (State0 42)
"Final state; 45"
```

Attempting to make a transition from State1 results in a type error which can be picked up at compile-time:

```
*TicTacToe> move 4 (move 3 (State0 42))

:25:1:
    No instance for (Transition State1 to0)
      arising from a use of `move'
    Possible fix:
      add an instance declaration for (Transition State1 to0)
    In the expression: move 4 (move 3 (State0 42))
    In an equation for `it': it = move 4 (move 3 (State0 42))
```

**Rule 4:** In either state, we can call pprint to get a String representation of the state.

Yes, for example:

```
*TicTacToe> pprint $ State0 10
"Initial state; 10"
*TicTacToe> pprint $ State1 20
"Final state; 20"
```

If I’m correct, this is the way that we can enforce rules 3 and 4 of the tic-tac-toe problem. This idea may be useful in other situations. For example, a scientific workflow system could enforce, at compile time, the constraint that a node is connected to a data source and a data sink. Or a shopping cart API could make sure that you could not go to the checkout on an empty cart.

Here is the full source code for my two state example:

```haskell
{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}
>
data State0 = State0 Int deriving Show
data State1 = State1 Int deriving Show

class StateLike b where
    pprint :: b -> String

instance StateLike State0 where
    pprint (State0 n) = "Initial state; " ++ show n

instance StateLike State1 where
    pprint (State1 n) = "Final state; " ++ show n

initialState = State0 34

class Transition from to | from -> to where
  move :: Int -> from -> to

instance Transition State0 State1 where
    move i (State0 n) = State1 (n + i)
```

Thinking more generally, we can encode a finite state system using type classes. Here is code for a system with states 0, 1, 2, 3, 4, and admissible transitions

  * 0 → 1
  * 0 → 2
  * 0 → 3
  * 1 → 4
  * 4 → 1

```haskell
data FState0 = FState0 Int deriving Show
data FState1 = FState1 Int deriving Show
data FState2 = FState2 Int deriving Show
data FState3 = FState3 Int deriving Show
data FState4 = FState4 Int deriving Show

class FStateLike b where
    fsPPrint :: b -> String

instance FStateLike FState0 where
    fsPPrint (FState0 n) = "FState0; " ++ show n

instance FStateLike FState1 where
    fsPPrint (FState1 n) = "FState1; " ++ show n

instance FStateLike FState2 where
    fsPPrint (FState2 n) = "FState2; " ++ show n

instance FStateLike FState3 where
    fsPPrint (FState3 n) = "FState3; " ++ show n

instance FStateLike FState4 where
    fsPPrint (FState4 n) = "FState4; " ++ show n

class Transition1 a b | a -> b where
    transition1 :: a -> b

class Transition2 a b | a -> b where
    transition2 :: a -> b

class Transition3 a b | a -> b where
    transition3 :: a -> b

class Transition4 a b | a -> b where
    transition4 :: a -> b

class Transition5 a b | a -> b where
    transition5 :: a -> b

instance Transition1 FState0 FState1 where
    transition1 (FState0 n) = FState1 n

instance Transition2 FState0 FState2 where
    transition2 (FState0 n) = FState2 n

instance Transition3 FState0 FState3 where
    transition3 (FState0 n) = FState3 n

instance Transition4 FState1 FState4 where
    transition4 (FState1 n) = FState4 n

instance Transition5 FState4 FState1 where
    transition5 (FState4 n) = FState1 n

-- OK:
test1 :: FState1
test1 = transition5 $ transition4 $ transition1 $ FState0 42

-- Not ok, compile-time error:
-- test2 = transition4 $ transition2 $ FState0 42
```

You can do a lot with Haskell’s type system. In [Issue 8 of The Monad.Reader](http://www.haskell.org/haskellwiki/User:ConradParker/InstantInsanity) Conrad Parker wrote a complete type-level program for the [Instant Insanity](http://en.wikipedia.org/wiki/Instant_Insanity) game. Wow.

One final comment. Referring to the tic-tac-toe exercise, Tony wrote:

> Recently I set a task, predicted how difficult it would be, then was astonished to find that it appears to be _significantly more difficult than I had originally predicted_. I’m still not sure what is going on here, however, I think there are some lessons to be taken.

Personally, I would have found the tic-tac-toe exercise easy if was prefaced with "Haskell’s type classes can enforce the permissible transitions of a finite state system." But most tutorials on type classes use fairly benign examples like adding an Eq instance for a new Color class. It’s a novel idea to deliberately _not_ provide an instance for a certain class to stop an end-user of an API from making certain transitions in a state diagram. It’s novel to even think of encoding a state diagram using a language’s type system, especially after spending years working with languages with relatively weak type systems.

* * *

Further reading:

  * Tony’s blog post from 2011: <http://blog.tmorris.net/posts/understanding-practical-api-design-static-typing-and-functional-programming/>.
  * Tony’s solution to the tic-tac-toe problem: <http://hackage.haskell.org/package/TicTacToe-0.0.1>.
  * Solving a similar problem in F#: <http://fsharpforfunandprofit.com/posts/designing-for-correctness/>.
  * Typeclass programming: <http://www.haskell.org/haskellwiki/User:ConradParker/InstantInsanity>.

**Archived Comments**

Date: 2017-01-31 08:38:03.281378 UTC

Author: Nick Hamilton

I like your demonstration of encoding state with type classes, but it seems using this mechanism to solve Tony's challenge will result in a tonne of boilerplate? Over a hundred if you find a way to encode mirrored states, otherwise... thousands. Further to that, do you have any thoughts how the 'api' user would know which transition function (of the hundred+) to call?

I had a crack at solving the challenge, but I used the undecidable instances and flexible contexts extensions to create recursively unwrap a nested type containing each of the states.
