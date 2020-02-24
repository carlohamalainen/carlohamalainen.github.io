---
id: 737
title: Markov Decision Processes and the UCT algorithm
date: 2009-10-29T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2009/10/29/markov-decision-processes-and-the-uct-algorithm/
permalink: /2009/10/29/markov-decision-processes-and-the-uct-algorithm/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
At <a>EuroPython 2009</a> I met a guy from Bet Fair who told me about the UCT algorithm. I decided to implement the UCT algorithm for the sailing problem and compare my results to normal Monte Carlo rollout planning. The files [are here](http://carlo-hamalainen.net/stuff/mdpnotes/), the main document that I wrote is [mdp.pdf](http://carlo-hamalainen.net/stuff/mdpnotes/mdp.pdf).

The original paper on UCT is [Levente Kocsis and Csaba Szepesvári: Bandit based Monte-Carlo Planning](http://carlo-hamalainen.net/stuff/mdpnotes/papers/UCT_ecml06.pdf). Improvements to UCT are given by [Gelly, Silver: Combining Online and Offline Knowledge in UCT](http://carlo-hamalainen.net/stuff/mdpnotes/papers/387.pdf). Sparse sampling is another approach to large MDPs: [Kearns, Mansour, Ng: A Sparse Sampling Algorithm for Near-Optimal Planning in Large Markov Decision Processes](http://carlo-hamalainen.net/stuff/mdpnotes/papers/Kearns,%20Mansour,%20Ng%20-%20A%20sparse%20samling%20algorithm%20for%20near-optimal%20planning%20in%20large%20markov%20decision%20processes.pdf). For a gentle introduction to reinforcement learning see [Greenwald's notes](http://carlo-hamalainen.net/stuff/mdpnotes/papers/reinforcement_learning%20-%20Greenwald.pdf).