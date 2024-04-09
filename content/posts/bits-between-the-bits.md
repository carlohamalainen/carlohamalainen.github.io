---
date: 2024-04-02T09:57:06+10:00
title: Bits Between the Bits
author: "Carlo Hamalainen"
url: /2024/04/02/bits-between-the-bits
---

This is a fun talk about what goes on before your program starts executing ``main``:

{{< youtube dOfucXtyEsU >}}

Matt Godbolt mentioned the oddly named "bss" section. The book [Advanced Programming in the UNIX® Environment, Third Edition](https://www.oreilly.com/library/view/advanced-programming-in/9780321638014/) gives the historical reason:

> Uninitialized data segment, often called the "bss" segment, named after an ancient assembler operator that stood for "block started by symbol." Data in this segment is initialized by the kernel to arithmetic 0 or null pointers before the program starts executing.  The C declaration
>
> ```C
> long   sum[1000];
> ```
>
> appearing outside any function causes this variable to be stored in the uninitialized data segment.