---
author: Carlo Hamalainen

date: "2010-06-30T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2010/06/30/best-screenrc/
title: Best screenrc
url: /2010/06/30/best-screenrc/
---
A mashup of nice options that I've accumulated over time. The status line is rather long but selecting it and copy 'n' paste should work.

    # Ctrl-right square bracket doesn't seem to clash with anything:
    escape ^]]

    # Nice status at the bottom of the screen, permanently shown:
    hardstatus alwayslastline '%{= kG}[ %{G}%H %{g}][%= %{= kw}%?%-Lw%?%{r}(%{W}%n*%f%t%?(%u)%?%{r})%{w}%?%+Lw%?%?%= %{g}][%{B} %d/%m %{W}%c %{g}]'

    # Use Ctrl-left arrow and Ctrl-right arrow to cycle through windows.
    bindkey ^[[1;5D prev
    bindkey ^[[1;5C next
