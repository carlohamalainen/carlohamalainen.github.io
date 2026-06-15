---
author: Carlo Hamalainen
date: "2012-08-15T00:00:00Z"
title: Kogge's The Architecture of Symbolic Computers 1991
url: /2012/08/15/kogges-the-architecture-of-symbolic-computers-1991/
---

I recently discovered Kogge's book _The Architecture of Symbolic Computers_ via two blog posts: [Loper OS: The Book](http://www.loper-os.org/?p=13) and [fogus: Some Lisp books (and then some)](http://blog.fogus.me/2012/07/25/some-lisp-books-and-then-some/). The book seems to be out of print and second hand copies are quite expensive, so I decided to scan the table of contents and index so that people can get some idea of what the book contains before they drop ~{{< dollar >}}US90 on a copy:

[Kogge\_The\_Architecture\_of\_Sybolic\_Computers\_TOC_INDEX.pdf](/stuff/Kogge_The_Architecture_of_Symbolic_Computers_TOC_INDEX.pdf).


Part II (Function Based Computing) is the most interesting to me. Recently I have been experimenting with [Swift](http://www.ci.uchicago.edu/swift/main/) for structuring large scale computations. Swift uses the idea of a "future", and this is covered in Chapter 9 (Demand Driven Evaluation), along with lazy evaluation, streams, and continuations.

**edit 2012-09-14:** I implemented an SECD interpreter in Python, using Chapter 7 of Kogge's book as a guide: <https://github.com/carlohamalainen/pysecd>.

