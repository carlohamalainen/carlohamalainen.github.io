---
id: 713
title: 'Note to self: Parsec and &#8216;nova list&#8217;'
date: 2013-02-19T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/02/19/note-to-self-parsec-and-nova-list/
permalink: /2013/02/19/note-to-self-parsec-and-nova-list/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
<https://gist.github.com/carlohamalainen/4982764.js>

Example input: 

<pre>+--------------------------------------+------------------------+--------+----------------------------+
| ID                                   | Name                   | Status | Networks                   |
+--------------------------------------+------------------------+--------+----------------------------+
| 10000000-0000-0000-0000-000000000001 | vm 1                   | ACTIVE | cell1=192.168.1.1          |
| 10000000-0000-0000-0000-000000000002 | vm 2                   | ACTIVE | cell2=192.168.1.2          |
| 10000000-0000-0000-0000-000000000003 | vm 3                   | ACTIVE | cell2=192.168.1.3          |
| 10000000-0000-0000-0000-000000000004 | vm 4                   | ACTIVE | cell1=192.168.1.4          |
| 10000000-0000-0000-0000-000000000005 | vm 5                   | ACTIVE | cell1=192.168.1.5          |
| 10000000-0000-0000-0000-000000000006 | vm 6                   | BUILD  | cell1=192.168.1.6          |
| 10000000-0000-0000-0000-000000000007 | vm 7                   | BUILD  | cell1=192.168.1.7          |
| 10000000-0000-0000-0000-000000000008 | vm 8                   | ACTIVE | cell2=192.168.1.8          |
| 10000000-0000-0000-0000-000000000009 | vm 9                   | ACTIVE | cell1=192.168.1.9          |
| 10000000-0000-0000-0000-000000000010 | vm 10                  | ACTIVE | cell1=192.168.1.10         |
| 10000000-0000-0000-0000-000000000011 | vm 11                  | ACTIVE | cell2=192.168.1.11         |
| 10000000-0000-0000-0000-000000000012 | vm 12                  | ACTIVE | cell2=192.168.1.12         |
| 10000000-0000-0000-0000-000000000013 | vm 13                  | ACTIVE | cell1=192.168.1.13         |
| 10000000-0000-0000-0000-000000000014 | vm 14                  | ACTIVE | cell1=192.168.1.14         |
| 10000000-0000-0000-0000-000000000015 | vm 15                  | ACTIVE | cell1=192.168.1.15         |
| 10000000-0000-0000-0000-000000000016 | vm 16                  | ACTIVE | cell2=192.168.1.16         |
| 10000000-0000-0000-0000-000000000017 | vm 17                  | ACTIVE | cell1=192.168.1.17         |
| 10000000-0000-0000-0000-000000000018 | vm 18                  | ACTIVE | cell2=192.168.1.18         |
| 10000000-0000-0000-0000-000000000019 | vm 19                  | ACTIVE | cell2=192.168.1.19         |
| 10000000-0000-0000-0000-000000000020 | vm 20                  | ACTIVE | cell1=192.168.1.20         |
| 10000000-0000-0000-0000-000000000021 | vm 21                  | ACTIVE | cell2=192.168.1.21         |
| 10000000-0000-0000-0000-000000000022 | vm 22                  | BUILD  |                            |
| 10000000-0000-0000-0000-000000000023 | vm 23                  | ACTIVE | cell1=192.168.1.22         |
| 10000000-0000-0000-0000-000000000024 | vm 24                  | DERP   | cell1=192.168.1.23         |
+--------------------------------------+------------------------+--------+----------------------------+
</pre>

Example run: 

<pre>$ ghc --make ReadNovaList.hs && cat nova_list_output.txt  | ./ReadNovaList
("10000000-0000-0000-0000-000000000001","vm 1",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.1"}))
("10000000-0000-0000-0000-000000000002","vm 2",Active,Just (Network {networkCell = "cell2", networkIP = "192.168.1.2"}))
("10000000-0000-0000-0000-000000000003","vm 3",Active,Just (Network {networkCell = "cell2", networkIP = "192.168.1.3"}))
("10000000-0000-0000-0000-000000000004","vm 4",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.4"}))
("10000000-0000-0000-0000-000000000005","vm 5",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.5"}))
("10000000-0000-0000-0000-000000000006","vm 6",Building,Just (Network {networkCell = "cell1", networkIP = "192.168.1.6"}))
("10000000-0000-0000-0000-000000000007","vm 7",Building,Just (Network {networkCell = "cell1", networkIP = "192.168.1.7"}))
("10000000-0000-0000-0000-000000000008","vm 8",Active,Just (Network {networkCell = "cell2", networkIP = "192.168.1.8"}))
("10000000-0000-0000-0000-000000000009","vm 9",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.9"}))
("10000000-0000-0000-0000-000000000010","vm 10",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.10"}))
("10000000-0000-0000-0000-000000000011","vm 11",Active,Just (Network {networkCell = "cell2", networkIP = "192.168.1.11"}))
("10000000-0000-0000-0000-000000000012","vm 12",Active,Just (Network {networkCell = "cell2", networkIP = "192.168.1.12"}))
("10000000-0000-0000-0000-000000000013","vm 13",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.13"}))
("10000000-0000-0000-0000-000000000014","vm 14",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.14"}))
("10000000-0000-0000-0000-000000000015","vm 15",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.15"}))
("10000000-0000-0000-0000-000000000016","vm 16",Active,Just (Network {networkCell = "cell2", networkIP = "192.168.1.16"}))
("10000000-0000-0000-0000-000000000017","vm 17",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.17"}))
("10000000-0000-0000-0000-000000000018","vm 18",Active,Just (Network {networkCell = "cell2", networkIP = "192.168.1.18"}))
("10000000-0000-0000-0000-000000000019","vm 19",Active,Just (Network {networkCell = "cell2", networkIP = "192.168.1.19"}))
("10000000-0000-0000-0000-000000000020","vm 20",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.20"}))
("10000000-0000-0000-0000-000000000021","vm 21",Active,Just (Network {networkCell = "cell2", networkIP = "192.168.1.21"}))
("10000000-0000-0000-0000-000000000022","vm 22",Building,Nothing)
("10000000-0000-0000-0000-000000000023","vm 23",Active,Just (Network {networkCell = "cell1", networkIP = "192.168.1.22"}))
("10000000-0000-0000-0000-000000000024","vm 24",StatusError "DERP",Just (Network {networkCell = "cell1", networkIP = "192.168.1.23"}))
</pre>