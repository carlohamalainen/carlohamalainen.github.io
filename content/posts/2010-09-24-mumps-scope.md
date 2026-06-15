---
author: Carlo Hamalainen

date: "2010-09-24T00:00:00Z"
format: image
title: Mumps scope
url: /2010/09/24/mumps-scope/
---
Here's a quick post about [scoping](http://en.wikipedia.org/wiki/Scope_(programming)) in Mumps/ObjectScript in [InterSystems Caché](http://en.wikipedia.org/wiki/Cach%C3%A9_(software)).

Rob at M/Gateway Developments pointed out that I was referring to procedures, not functions, in an earlier post (oh dear!). So here we go with the right names:

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

