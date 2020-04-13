---
id: 708
title: Emacs + SLIME + SBCL setup
date: 2011-09-19T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2011/09/19/emacs-slime-sbcl-setup/
permalink: /2011/09/19/emacs-slime-sbcl-setup/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Recently I tried to set up Emacs, SLIME, and SBCL, all compiled from source. The instructions that I [usually refer to](http://functionalrants.wordpress.com/2008/09/06/how-to-set-up-emacs-slime-sbcl-under-gnulinux/) say that you just need this in your .emacs to get SLIME working:

    ;; Set up the Common Lisp environment
    (add-to-list 'load-path "/usr/share/common-lisp/source/slime/")
    (setq inferior-lisp-program "/usr/bin/sbcl")
    (require 'slime)
    (slime-setup)

This doesn't quite do it for the latest CVS version of SLIME. For example after typing "(defun " nothing sensible showed up in the minibuffer at the bottom of the emacs window. By looking at the latest Ubuntu distribution's slime init file, I found that this is also needed:

    (eval-after-load "slime"
      '(progn
        (slime-setup '(slime-fancy slime-asdf slime-banner))
        (setq slime-complete-symbol*-fancy t)
        (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

My full .emacs file is on GitHub: <https://github.com/carlohamalainen/dotfiles/blob/master/.emacs>.

Note: I only had to do this because I was compiling particular versions of SLIME and SBCL on a shared workstation. If you have a normal Ubuntu/Debian setup you can just apt-get install SLIME and everything will work as expected. I figure this bit of config might be useful for people working on other systems.
