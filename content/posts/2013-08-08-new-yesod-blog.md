---
author: Carlo Hamalainen

date: "2013-08-08T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2013/08/08/new-yesod-blog/
title: New Yesod blog
url: /2013/08/08/new-yesod-blog/
---
I've ditched my old WordPress blog in favour of a blog system that I wrote myself using [Yesod](http://www.yesodweb.com/). Source code is here: <https://github.com/carlohamalainen/cli-yesod-blog>. 

Overall, the development process was pleasant. Many things like configuration settings, routes, and SQL queries can be checked by the compiler, so a whole class of run-time errors just never happened. I was able to do some fairly involved refactorings of the code, and once it compiled, it Just Worked (TM). This is in stark contrast to my experience of developing Python applications in my day job. 

Reflecting on the qualitative feeling during the development process, I was reminded of a talk by Rich Hickey called [Simple Made Easy](http://www.infoq.com/presentations/Simple-Made-Easy). Some of the main points that Rich made during the talk really resonated with me, even though he had Clojure in mind, not Haskell. (Thanks to [obeautifulcode.com](http://obeautifulcode.com/Craftsmanship/Simple-Software-A-Tutorial/) for making notes). 

On familiarity: 

> "If you want everything to be familiar you will never learn anything new because it cannot be significantly different from what you already know and not drift away from the familiarity." 

Writing my blog system using Yesod definitely pushed me beyond what was familiar. A surprisingly simple problem, adding ReCAPTCHA verification for comments, gave me a concrete reason to dig into the details for [applicative forms in Yesod](/2013/07/24/note-on-applicative-forms-in-yesod). 

On ego: 

> "... this starts trampling on our egos ... due to combination of hubris and insecurity we never talk about something being outside of our capabilities." 

The flipside, if one can get past the ego, is that there are lots of smart people in the Haskell community. The [Haskell Cafe](http://www.haskell.org/mailman/listinfo/haskell-cafe) is a nice place to lurk -- my to-do list from there is ever-increasing. 

Lastly: 

> We are too focused on the experience of programming rather than the artifacts -- the programs that gets produced and run. We should assess quality based on our artifacts, not on the experience of typing programs into an editor! 

For me, and I may be reading something into this that Rich did not intend, Haskell/GHC provides the best experience for maintaining code due to the ability of the compiler to check so many things. On Lisp vs Java, here's a [similar opinion](https://news.ycombinator.com/item?id=6100017):

> > "The difference between Lisp and Java, as Paul Graham has pointed out, is that Lisp is for working with computational ideas and expression, whereas Java is for expressing completed programs. As James [Gosling] says, Java requires you to pin down decisions early on..." 
> 
> 
> 
> My experience with modifying Lisp and Java programs has largely been the opposite. I've been working on a Java application for the past few weeks, and the design of the program has changed several times as I've thought of new features to add and cleaner ways to organize my code. Today, I had a class X that was responsible for keeping track of a certain variable, and I wanted to move that responsibility to class Y. I deleted the relevant field and all references to it in class X, and Eclipse highlighted all of the errors that change introduced. I fixed the highlighted errors and added the new methods I needed to class Y, and when I tested the relevant functionality, I saw that I had introduced only one minor bug, which I fixed with a single line of code. There was nothing difficult about the process. 
> 
> 
> 
> I've found refactoring Lisp programs to be more difficult. When I was writing a prototype of this same application in Lisp (Clojure more specifically), I had a macro to implement a small DSL. The macro was not particularly complex, although it was non-trivial: it did some code walking and called some helper functions. When my DSL grew past a certain size, making changes to its implementation became rather difficult. Sometimes the compiler would report errors at runtime, but more often than not, things would just stop working in mysterious ways whenever I made a change. The macro's output was also quite different than the kind of code I would write by hand, so debugging was difficult. 
> 
> 
> 
> I'm not the best Lisp programmer, so I'm not trying to say that Lisp always makes it hard to change your programs. But there are cases where Java makes it very easy to change your programs. 

Anyway, back to my blog system. The interface is all on the command line, because I like to live in a GNU Screen session and use vim whenever possible. For example, to add a new post, I type 

    $ blog-utils --add 'title of the post'

and vim is opened on a temporary file. When I save and quit, the post is added to the system, but is invisible: 

    "/tmp/blah.html" 40L, 1533C written
    Key {unKey = PersistInt64 104}

    $ blog-utils  --list | grep 'New Yesod'
    104 HIDDEN 2013/8/8/new-yesod-blog  'New Yesod blog' 

To set it to be visible: 

    $ blog-utils --set-post-visible 104
  
Easy 🙂 

One niggly issue was that I had to set up a proxy from Apache to Nginx. This is not the usual direction, but I have a number of sites running on Apache and it seemed the path of least resistance. So for Debian Squeeze, here ar ethe magic lines (nginx runs on port 8080 which is not visible to the outside world):
  
    LoadModule  proxy_module         /usr/lib/apache2/modules/mod_proxy.so
    LoadModule  proxy_http_module    /usr/lib/apache2/modules/mod_proxy_http.so
    LoadModule  headers_module       /usr/lib/apache2/modules/mod_headers.so
    LoadModule  deflate_module       /usr/lib/apache2/modules/mod_deflate.so

    ProxyPass /blog http://carlo-hamalainen.net:8080/blog
    ProxyPassReverse /blog http://carlo-hamalainen.net:8080/blog

    ProxyPass /static http://carlo-hamalainen.net:8080/static
    ProxyPassReverse /static http://carlo-hamalainen.net:8080/static
