---
author: Carlo Hamalainen

date: "2014-03-03T00:00:00Z"
format: image
title: Cabal hell workaround
url: /2014/03/03/cabal-hell-workaround/
---
Note: cabal now includes the freeze command, i.e. cabal freeze and there is also [Stack](https://github.com/commercialhaskell/stack), so this post is now deprecated.

Lately I've been encountering cabal hell with a few of my Haskell projects, even ones that I felt were relatively small and straightforward. Building inside cabal sandboxes is a good idea, as it separates various projects from each other, but it does not solve the underlying problem. Michael Snoyman [sums it up](http://www.yesodweb.com/blog/2012/03/cabal-nirvana):

> Simply stated, Hackage is a zoo. Anyone can upload anything at any time. If I write some code that depends on foobar version 0.1, and someone comes along and makes a breaking change in 0.1.1, my code will (generally) break. We also have the issue of different packages using different versions of the same underlying packages (e.g., baz requires foobar 0.1, and bin requires foobar 0.2), and therefore they cannot be installed side-by-side.

Periodically [manually tightening package bounds](https://github.com/carlohamalainen/cli-yesod-blog/commit/574c80daf3814e2b6017103ca122595a9e12da32) is a pain.

Future versions of cabal will hopefully support some kind of "freeze" operation that will fix all of the package versions, but in the meantime we can use [cabal-constraints](https://github.com/benarmston/cabal-constraints) to produce a cabal.config file to fix the versions.

To use cabal-constraints, I followed this procedure:

1. Install cabal-constraints, making sure to use the right version of Cabal:

    ```
    git clone https://github.com/benarmston/cabal-constraints.git
    cd cabal-constraints
    rm -fr .cabal-sandbox cabal.sandbox.config dist

    cabal sandbox init

    cabal install --haddock-hyperlink-source --dependencies-only
        --constraint "Cabal == $( cabal --version | grep 'Cabal library' | cut -f3 -d' ' )"
    cabal install --haddock-hyperlink-source
        --constraint "Cabal == $( cabal --version | grep 'Cabal library' | cut -f3 -d' ' )"

    echo 'export PATH=$PATH:'`pwd`/.cabal-sandbox/bin >> $HOME/.bashrc
    source $HOME/.bashrc
    ```

2. Build the Haskell project. **Use the same version of cabal**. If you have an old build tree and you have updated cabal/Cabal since then, you will have to build a version of cabal-constraints with that particular version of cabal. Go back to step 1.

    ```
    cd my-haskell-project

    rm -fr .cabal-sandbox cabal.sandbox.config dist

    cabal sandbox init

    cabal install --haddock-hyperlink-source --dependencies-only
    cabal install --haddock-hyperlink-source
    ```

3. Run cabal-constraints:

    ```
    cabal-constraints dist/dist-sandbox-*/setup-config >> cabal.config
    ```

 It produces this kind of output:

```
constraints: array == 0.4.0.1
            , base == 4.6.0.1
            , bytestring == 0.10.0.2
            , containers == 0.5.0.0
            , deepseq == 1.3.0.1
            , directory == 1.2.0.1
            , filepath == 1.3.0.1
            , ghc-prim == 0.3.0.0
            , integer-gmp == 0.5.0.0
            , mmorph == 1.0.2
            , mtl == 2.1.2
            , old-locale == 1.0.0.5
            , parsec == 3.1.5
            , pipes == 3.3.0
            , process == 1.1.0.2
            , rts == 1.0
            , text == 1.1.0.0
            , time == 1.4.0.1
            , transformers == 0.3.0.0
            , unix == 2.6.0.1
```

4. For paranoia's sake, rebuild the project to make sure that the constraints are correct:

    ```
    # still in my-haskell-project

    rm -fr .cabal-sandbox cabal.sandbox.config dist

    cabal sandbox init

    cabal install --haddock-hyperlink-source --dependencies-only
    cabal install --haddock-hyperlink-source
    ```

* * *

I find that things are mostly under control now, although sometimes I make a mess of things and have to blow away ~/.cabal and ~/.ghc and replace them with earlier snapshots that I keep in a tarball.
