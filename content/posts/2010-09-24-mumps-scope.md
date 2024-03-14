---
author: Carlo Hamalainen

date: "2010-09-24T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2010/09/24/mumps-scope/
id: 830
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: Mumps scope
url: /2010/09/24/mumps-scope/
---
Here's a quick post about [scoping](http://en.wikipedia.org/wiki/Scope_(programming)) in Mumps/ObjectScript in [InterSystems Caché](http://en.wikipedia.org/wiki/Cach%C3%A9_(software)).

Rob at <a>M/Gateway Developments</a> pointed out that I was referring to procedures, not functions, in an earlier post (oh dear!). So here we go with the right names:

Here is **PROCEDURE1.mac**:

    PROCEDURE1
        Write "Here we are in PROCEDURE1", !
        Set LocalVariable=0
        Write "We LocalVariable = ", LocalVariable, !

        Write "Calling PROCEDURE2...", !
        Do ^PROCEDURE2
        Write "Now we LocalVariable = ", LocalVariable, !

Here is **PROCEDURE2.mac**:

    PROCEDURE2
        Set LocalVariable=666

Now run it in a fresh terminal:

    USER>Do ^PROCEDURE1
    Here we are in PROCEDURE1
    We LocalVariable = 0
    Calling PROCEDURE2...
    Now we LocalVariable = 666

    USER>

So by default, what look like local variables actually travel along the calling chain because Mumps uses dynamic scope.

Rob replied with this example:

    procedure1
     new LocalVariable,newVar
     Write "Here we are in function1", !
     Set LocalVariable=0
     Write "We LocalVariable = "_LocalVariable, !
     Write "Calling function2...", !
     set newVar=$$function2()
     Write "LocalVariable is still = "_ LocalVariable, !
     Write "newVar="_newVar
     QUIT

    function2()
     new LocalVariable
     set LocalVariable=666
     Write "In function2 with LocalVariable="_LocalVariable
     set LocalVariable=$$function3()
     Write "Now LocalVariable has been updated to "_LocalVariable
     QUIT LocalVariable

    function3()
     QUIT 777

... and wrote:

> It should run correctly in the way you'd expect, with LocalVariable properly lexically scoped. Procedures don't have to have formal parameters, but functions must, even if it's an empty list as above. Procedures just QUIT at the end. Functions quit with a return value and are invoked using the $$ at the front of the name. Procedures are invoked with a Do.

That cleared up some of my (mis)understanding of the scoping rules. Unfortunately the large Mumps system that I have worked on uses procedures almost all of the time (not functions) and no formal parameter lists and I took that as a given.

**Archived Comments**

Date: 2010-09-29 20:21:32 UTC

Author: Kesav Kolla

I guess it's nothing to do with procedure or function. It's something to do with new. In the second procedure if you new the localvariable then the scope is restricted within in the procedure.

Date: 2010-09-30 12:16:46 UTC

Author: Rob

Yes you are correct -- you *could* use a procedure and, if you do, you should always control the scope of the variables it uses with the new command.

However, a lot of legacy Mumps developers don't really appreciate the difference between functions and procedures and there's a tendency for such people to use procedures when functions are more appropriate.

In my reworked example I not only wanted to demonstrate the use of the new command to control variable scope, I also wanted to show how a function was a more appropriate means of returning a value.

Rob

Date: 2010-10-31 17:51:04 UTC

Author: Brian Tyler

So there I was laughing away uncontrollably as I read about MUMPS on The Daily WTF, thinking about the unfortunate people who actually have to write that devil language when lo-and-behold your name popped up on the bottom of the page.

How are you getting on?

England is currently in self-destruct mode. The proletariate decided they had had enough of their betters paying lipservice to welfare and demanded all social policy be scrapped and that they be plunged into Tressellian (as in Robert Tressell) poverty once more so that we can have a proper aristocracy again who can eat lobster on toast for breakfast, like in the good old days.

In protest I've stopped recycling.
