---
author: Carlo Hamalainen

date: "2009-10-29T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2009/10/29/markov-decision-processes-and-the-uct-algorithm/
title: Markov Decision Processes and the UCT algorithm
url: /2009/10/29/markov-decision-processes-and-the-uct-algorithm/
---
At <a>EuroPython 2009</a> I met a guy from Bet Fair who told me about the UCT algorithm. I decided to implement the UCT algorithm for the sailing problem and compare my results to normal Monte Carlo rollout planning. The files [are here](/stuff/mdpnotes/), the main document that I wrote is [mdp.pdf](/stuff/mdpnotes/mdp.pdf).

The original paper on UCT is [Levente Kocsis and Csaba Szepesvári: Bandit based Monte-Carlo Planning](/stuff/mdpnotes/papers/UCT_ecml06.pdf). Improvements to UCT are given by [Gelly, Silver: Combining Online and Offline Knowledge in UCT](/stuff/mdpnotes/papers/387.pdf). Sparse sampling is another approach to large MDPs: [Kearns, Mansour, Ng: A Sparse Sampling Algorithm for Near-Optimal Planning in Large Markov Decision Processes](/stuff/mdpnotes/papers/Kearns,%20Mansour,%20Ng%20-%20A%20sparse%20samling%20algorithm%20for%20near-optimal%20planning%20in%20large%20markov%20decision%20processes.pdf). For a gentle introduction to reinforcement learning see [Greenwald's notes](/stuff/mdpnotes/papers/reinforcement_learning%20-%20Greenwald.pdf).
