---
author: Carlo Hamalainen

date: "2013-03-10T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2013/03/10/note-to-self-semi-unstructured-text-parsing-with-parsec-and-an-alternative/
title: 'note to self: semi-unstructured text parsing with Parsec (and an alternative)'
url: /2013/03/10/note-to-self-semi-unstructured-text-parsing-with-parsec-and-an-alternative/
---
Managed to answer someone's [question](http://www.haskell.org/pipermail/haskell-cafe/2013-March/106776.html) on haskell-cafe about parsing semi-structured text with Parsec:


S. Doaitse Swierstra [pointed out](http://www.haskell.org/pipermail/haskell-cafe/2013-March/106834.html) that the [Data.List.Grouping](http://hackage.haskell.org/packages/archive/list-grouping/0.1.1/doc/html/Data-List-Grouping.html) package may be more appropriate here.

<https://gist.github.com/carlohamalainen/5087207>

```haskell
{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Control.Applicative hiding ((<|>),many)

-- Example input:

{-
top 1:

some text ... bla

top 2:

more text ... bla bla

-}

data Top = Top String deriving (Show)
data Content = Content [String] deriving (Show)
data Section = Section Top Content deriving (Show)

headline = do
    t <- many1 (noneOf ":\n")
    char ':'
    newline

    return $ Top t

contentLine = do
    x <- many (noneOf ":\n")
    newline
    return x

content = do
    line <- optionMaybe (try contentLine)

    case line of Just x -> do xs <- content
                              return (x:xs)
                 _      -> return []

section = do
    h <- headline
    c <- Content <$> content
    return $ Section h c

main = do
    x <- readFile "simple.txt"
    print $ parse (many section) "" x

```
