---
date: 2025-05-23
title: Performance Surprises
url: /2025/05/23/performance-surprises
---

Performance surprises are everywhere - even in the simplest code 🚀

At my old bank job, I discovered something eye-opening: for some requests, our code spent more time logging than doing the actual mathematical calculations and data loading combined.

The culprit? Our timestamp formatting was using a custom format string, and our internal datetime library was invoking an expensive parser to read that format on every single call. A simple refactor into a separate, compiler-optimized function solved it instantly.

Note to my old colleagues: don't worry, I fixed this before I left! 😄

This reminds me of Matt Godbolt's talk in which he optimized order formatting from 515 nanoseconds down to 13 nanoseconds. His journey from C++ streams (with unexpected dynamic_cast overhead) to custom ASCII conversion routines is a masterclass in performance analysis.

Key takeaway: Never assume operations are "simple" or constant time. Profile first, optimize second. The bottleneck is rarely where you think it is.

{{< youtube fV6qYho-XVs >}}

`#Performance`
`#SoftwareEngineering`
`#Programming`
`#Optimization`
`#FinTech`

Crossposted at [LinkedIn](https://www.linkedin.com/posts/carlo-h%C3%A4m%C3%A4l%C3%A4inen-36857270_performance-tuning-activity-7331560994579603457-_6qq)
