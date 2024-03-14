---
author: Carlo Hamalainen

date: "2015-06-29T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2015/06/29/update-github-fork/
id: 817
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: Update Github fork
url: /2015/06/29/update-github-fork/
---
Situation:

Repo on Github that I want to contribute to: [https://github.com/whoever/whatever.git](https://github.com/whoever/whatever.git)

My fork of that repo on github: ``git@github.com:myself/whatever.git``

Over time the upstream repo is updated but my fork on github does not automatically get those updates. To push the latest changes from [https://github.com/whoever/whatever.git](https://github.com/whoever/whatever.git) to ``<git@github.com>:myself/whatever.git`` follows these steps.

Clone our repository:

```
git clone git@github.com:myself/whatever.git
cd whatever
```

Add the upstream repo as a remote, and call it upstream:

```
git remote add upstream https://github.com/whoever/whatever.git
```

Get all the branches of the upstream repo into remote-tracking branches. These branches will be named upstream/master, upstream/some-feature, etc.

```
git fetch upstream
```

Now replay all of the upstream's commits from the master branch against ours:

```
git checkout master
git rebase upstream/master
git push
```

Credit: <http://stackoverflow.com/a/7244456>
