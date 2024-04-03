---
author: Carlo Hamalainen

date: "2016-10-30T00:00:00Z"
format: image
title: Note on operational
url: /2016/10/30/note-on-operational/
---

I was looking at an old post of mine
on [free monads](https://carlo-hamalainen.net/blog/2014/6/7/notes-on-free-monads)
and wondered what it would look like using
[operational](https://hackage.haskell.org/package/operational"). Here we go!

(Literate Haskell source for this blog post
is [here](https://github.com/carlohamalainen/playground/tree/master/haskell/note-on-operational").)

First, some imports. We use [GADTs](https://wiki.haskell.org/GADTs_for_dummies)
for our instruction type.

```haskell
{-# LANGUAGE GADTs #-}

module Note where

import Control.Monad
import Control.Monad.Operational
import Test.QuickCheck
import qualified Data.Map as M
```

We want to encode a few instructions that operate on a data store. For
the moment we don't care about the implementation, just the underlying
actions that we can perform: create a value, list all values, retrieve
a particular value, and delete a value.

Suppose we have an index of type ``i`` and values
of type ``v``. Then ``DataStoreInstruction`` is:

```haskell
data DataStoreInstruction i v a where
    Create   :: v -> DataStoreInstruction i v i
    List     ::      DataStoreInstruction i v [v]
    Retrieve :: i -> DataStoreInstruction i v (Maybe v)
    Delete   :: i -> DataStoreInstruction i v ()
```

Intuitively, ``Create`` takes a value of type ``v``
and returns an instruction, which on interpretation gives a value
of type ``i`` (the last type parameter).

``List`` doesn't take any argument and produces a list of
type ``v``, i.e. ``[v]``. The only odd one is ``Delete``
which doesn't return anything, so it has a ``()`` in the last type variable.

A sequence of ``DataStoreInstruction``s is a program, which we get
using the [Program](https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#t:Program) constructor:

type DataStoreProgram i v a = Program (DataStoreInstruction i v) a

where the index has type ``i``, the values
have type ``v``, and the overall result of the program has type ``a``.

To more easily construct programs, use [singleton](https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#v:singleton):

```haskell
create :: v -> DataStoreProgram i v i
create = singleton . Create

list :: DataStoreProgram i v [v]
list = singleton List

retrieve :: i -> DataStoreProgram i v (Maybe v)
retrieve = singleton . Retrieve

delete :: i -> DataStoreProgram i v ()
delete = singleton . Delete
```

Now we can write programs in this DSL! All the usual monad things are
at our disposal:

```haskell
-- Insert a few values and return a list
-- of all values:
doSomeThings :: DataStoreProgram Int Int [Int]
doSomeThings = do
    ix3 <- create 3
    ix4 <- create 4
    delete ix3
    ix5 <- create 5
    list

-- Insert all the supplied values and
-- return a list of indexes as well as a
-- list of final values (which should be empty).
insertValues :: [Int] -> DataStoreProgram Int Int ([Int], [Int])
insertValues xs = do
    ixs <- forM xs create
    forM_ ixs delete -- delete everything
    final <- list

    return (ixs, final)
```

The last step is to write an interpreter. We do this
using [view](https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#v:view) and pattern matching on the
constructors of ``DataStoreInstruction``. We
use [:>>=](https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#v::-62--62--61-)
to break apart the program. As the documentation says, ``(someInstruction :>>= k)``
means ``someInstruction`` and the remaining program is given by the function ``k``.

Here we interpret the program using a ``Map`` as the underlying data store:

```haskell
interpretMap :: Ord a => DataStoreProgram a a b -> (M.Map a a -> b)
interpretMap = eval . view
  where
    eval (Return x)          m = x
    eval (Create   v :>>= k) m = interpretMap (k v)              (M.insert v v m)
    eval (List       :>>= k) m = interpretMap (k (M.elems m))    m
    eval (Retrieve i :>>= k) m = interpretMap (k $ M.lookup i m) m
    eval (Delete   i :>>= k) m = interpretMap (k ())             (M.delete i m)
```

If we wanted to we could flatten a program out to a string:

```haskell
interpretString :: (Show a, Show b) => DataStoreProgram a a b -> String
interpretString = eval . view
  where
    eval (Return x)          = "Return "   ++ show x
    eval (Create   v :>>= k) = "Create("   ++ show v ++ "); "  ++ interpretString (k v)
    eval (List       :>>= k) = "List; "                        ++ interpretString (k [])
    eval (Retrieve i :>>= k) = "Retrieve(" ++ show i ++ "); "  ++ interpretString (k $ Just i)
    eval (Delete   i :>>= k) = "Delete("   ++ show i ++ "); "  ++ interpretString (k ())
```

Maybe we have some super-important business need for storing Int/Int key value maps
with a Postgresql backend. We could target this by writing an interpreter that
uses [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple-0.5.2.1/docs/Database-PostgreSQL-Simple.html):

```haskell
data Connection = Connection -- cheating for this example

interprestPostgresql :: DataStoreProgram Int Int b -> (Connection -> IO b)
interprestPostgresql = eval . view
  where
    eval (Return x)          _ = return x
    eval (Create   v :>>= k) c = interprestPostgresql (k v) (insertPsql c v)
    eval (List       :>>= k) c = do allValues <- getAllPsql c
                                    interprestPostgresql (k allValues) c
    eval (Retrieve i :>>= k) c = do v <- lookupPsql c i
                                    interprestPostgresql (k v) c
    eval (Delete   i :>>= k) c = deletePsql c i >> interprestPostgresql (k ()) c

    -- Exercises for the reader.
    insertPsql c v = undefined
    getAllPsql c   = undefined
    lookupPsql c i = undefined
    deletePsql c i = undefined
```

Running the programs:

```
*Note> interpretMap doSomeThings M.empty
[4,5]

*Note> interpretString doSomeThings
"Create(3); Create(4); Delete(3); Create(5); List; Return []"

*Note> interpretMap (insertValues [1..10]) M.empty
([1,2,3,4,5,6,7,8,9,10],[])

*Note> interpretString (insertValues [1..10])
"Create(1); Create(2); Create(3); Create(4); Create(5); Create(6);
 Create(7); Create(8); Create(9); Create(10); Delete(1); Delete(2);
 Delete(3); Delete(4); Delete(5); Delete(6); Delete(7); Delete(8);
 Delete(9); Delete(10); List; Return ([1,2,3,4,5,6,7,8,9,10],[])"
```

### QuickCheck

It's always good to write some tests:

```haskell
prop_insert_then_delete :: [Int] -> Bool
prop_insert_then_delete xs = null $ interpretMap (f xs) M.empty
  where
    f :: [Int] -> DataStoreProgram Int Int [Int]
    f is = do
        ixs <- forM is create
        forM_ ixs delete
        list

prop_create :: Positive Int -> Bool
prop_create (Positive n) = let ns = [1..n] in ns == interpretMap (f ns) M.empty
  where
    f = mapM create
```

```
*Note> quickCheck prop_insert_then_delete
+++ OK, passed 100 tests.

*Note>
*Note>
*Note>
*Note> quickCheck prop_create
+++ OK, passed 100 tests.
```

Over time we find out that the interpreter that uses ``Map`` is too slow,
so we write a new one using a fancy data structure:

```haskell
-- Uses fancy tuned data structure.
interpretMapFast :: Ord a => DataStoreProgram a a b -> (M.Map a a -> b)
interpretMapFast = undefined -- So fancy!
```

Now we can compare implementations using QuickCheck. Nice!

```haskell
prop_fast_inserts :: [Int] -> Bool
prop_fast_inserts xs = (interpretMapFast xs' M.empty) == (interpretMap xs' M.empty)
  where
    xs' = f xs

    f :: [Int] -> DataStoreProgram Int Int [Int]
    f is = do
        ixs <- forM is create
        list
```

## Use of operational in larger projects

Here are a few samples of the operational package in action. For more,
see the [reverse dependencies](http://packdeps.haskellers.com/reverse/operational) on Hackage.

### Hadron

I first heard about operational from this talk at the New York Haskell
meetup: [Conquering Hadoop with Haskell and Ozgun Ataman](https://vimeo.com/90189610).

Here is the ``ConI`` type from [Hadron.Controller](https://github.com/Soostone/hadron/blob/master/src/Hadron/Controller.hs):

```haskell
data ConI a where
    Connect :: forall i o. MapReduce i o
            -> [Tap i] -> Tap o
            -> Maybe String
            -> ConI ()

    MakeTap :: Protocol' a -> ConI (Tap a)

    BinaryDirTap
        :: FilePath
        -> (FilePath -> Bool)
        -> ConI (Tap (FilePath, B.ByteString))

    ConIO :: IO a -> ConI a

    OrchIO :: IO a -> ConI ()
    -- Only the orchestrator performs action

    NodeIO :: IO a -> ConI a
    -- Only the nodes perform action

    SetVal :: String -> B.ByteString -> ConI ()

    GetVal :: String -> ConI (Maybe B.ByteString)

    RunOnce :: Serialize a => IO a -> ConI a
    -- Only run on orchestrator, then make available to all the
    -- nodes via HDFS.
```

There is the distinction between the single orchestrator node, which
runs ``OrchIO`` and can't run ``NodeIO``, and worker nodes that can't run ``OrchIO``
but can run ``NodeIO``. In the ``orchestrate``, trying to evaluate
a ``NodeIO`` results in an error:

```haskell
orchestrate
    :: (MonadMask m, MonadIO m, Applicative m)
    => Controller a
    -> RunContext
    -> RerunStrategy
    -> ContState
    -> m (Either String a)
orchestrate (Controller p) settings rr s = do
    bracket
      (liftIO $ openFile "hadron.log" AppendMode)
      (liftIO . hClose)
      (\_h -> do echoInfo ()  "Initiating orchestration..."
                 evalStateT (runExceptT (go p)) s)
    where
      go = eval . O.view

      eval (Return a) = return a
      eval (i :>>= f) = eval' i >>= go . f

      eval' :: (Functor m, MonadIO m) => ConI a -> ExceptT String (StateT ContState m) a
      eval' (ConIO  f) = liftIO f
      eval' (OrchIO f) = void $ liftIO f
      eval' (NodeIO _) = return (error "NodeIO can't be used in the orchestrator decision path")
```

Meanwhile, worker nodes ignore an ``OrchIO``
and [lift the NodeIO action](https://github.com/Soostone/hadron/blob/8f78faecad49ac0cc35484a70c8b186ae916920b/src/Hadron/Controller.hs#L1022-L1038).

### Chart

The [Graphics.Rendering.Chart.Backend.Impl](http://hackage.haskell.org/package/Chart-1.8/docs/Graphics-Rendering-Chart-Backend-Impl.html) module
defines the the backend instructions:

```haskell
data ChartBackendInstr a where
  StrokePath :: Path -> ChartBackendInstr ()
  FillPath   :: Path -> ChartBackendInstr ()
  GetTextSize :: String -> ChartBackendInstr TextSize
  DrawText    :: Point -> String -> ChartBackendInstr ()
  GetAlignments :: ChartBackendInstr AlignmentFns
  WithTransform  :: Matrix ->  Program ChartBackendInstr a -> ChartBackendInstr a
  WithFontStyle  :: FontStyle -> Program ChartBackendInstr a -> ChartBackendInstr a
  WithFillStyle  :: FillStyle -> Program ChartBackendInstr a -> ChartBackendInstr a
  WithLineStyle  :: LineStyle -> Program ChartBackendInstr a -> ChartBackendInstr a
  WithClipRegion :: Rect -> Program ChartBackendInstr a -> ChartBackendInstr a

type BackendProgram a = Program ChartBackendInstr a
```

Then the [Graphics.Rendering.Chart.Backend.Cairo](https://hackage.haskell.org/package/Chart-cairo-1.8/docs/Graphics-Rendering-Chart-Backend-Cairo.html)
module provides a way to run a ``BackendProgram`` for the cairo graphics engine:

```haskell
runBackend' :: CEnv -> BackendProgram a -> C.Render a
runBackend' env m = eval env (view m)
  where
    eval :: CEnv -> ProgramView ChartBackendInstr a -> C.Render a
    eval env (Return v)= return v
    eval env (StrokePath p :>>= f) = cStrokePath env p >>= step env f
    eval env (FillPath p :>>= f) = cFillPath env p >>= step env f
    eval env (GetTextSize s :>>= f) = cTextSize s >>= step env f
    eval env (DrawText p s :>>= f) = cDrawText env p s >>= step env f
    eval env (GetAlignments :>>= f) = return (ceAlignmentFns env) >>= step env f
    eval env (WithTransform m p :>>= f) = cWithTransform env m p >>= step env f
    eval env (WithFontStyle font p :>>= f) = cWithFontStyle env font p >>= step env f
    eval env (WithFillStyle fs p :>>= f) = cWithFillStyle env fs p >>= step env f
    eval env (WithLineStyle ls p :>>= f) = cWithLineStyle env ls p >>= step env f
    eval env (WithClipRegion r p :>>= f) = cWithClipRegion env r p >>= step env f
```

Meanwhile, [Graphics.Rendering.Chart.Backend.Diagrams](https://hackage.haskell.org/package/Chart-diagrams-1.8/docs/Graphics-Rendering-Chart-Backend-Diagrams.html) does the same but for [diagrams](https://hackage.haskell.org/package/diagrams-lib):

```haskell
runBackend' tr m = eval tr $ view $ m
  where
    eval :: (D.Renderable (D.Path V2 (N b)) b, D.Renderable t b, D.TypeableFloat (N b))
         => TextRender b t -> ProgramView ChartBackendInstr a
         -> DState (N b) (D.QDiagram b V2 (N b) Any, a)
    eval tr (Return v) = return (mempty, v)
    eval tr (StrokePath p   :>>= f) = dStrokePath  p   <># step tr f
    eval tr (FillPath   p   :>>= f) = dFillPath    p   <># step tr f
    eval tr@TextRenderSvg    (DrawText   p s :>>= f) = dDrawTextSvg    p s <># step tr f
    eval tr@TextRenderNative (DrawText   p s :>>= f) = dDrawTextNative p s <># step tr f
    eval tr (GetTextSize  s :>>= f) = dTextSize      s <>= step tr f
    eval tr (GetAlignments  :>>= f) = dAlignmentFns    <>= step tr f
    eval tr (WithTransform m p :>>= f)  = dWithTransform  tr m  p <>= step tr f
    eval tr (WithFontStyle fs p :>>= f) = dWithFontStyle  tr fs p <>= step tr f
    eval tr (WithFillStyle fs p :>>= f) = dWithFillStyle  tr fs p <>= step tr f
    eval tr (WithLineStyle ls p :>>= f) = dWithLineStyle  tr ls p <>= step tr f
    eval tr (WithClipRegion r p :>>= f) = dWithClipRegion tr r  p <>= step tr f
```

### redis-io

The [Data.Redis.Command](https://hackage.haskell.org/package/redis-resp-0.4.0/docs/Data-Redis-Command.html) module
defines heaps of commands:

```haskell
data Command :: * -> * where
    -- Connection
    Ping   :: Resp -> Command ()
    Echo   :: FromByteString a => Resp -> Command a
    Auth   :: Resp -> Command ()
    Quit   :: Resp -> Command ()
    Select :: Resp -> Command ()

    -- Many more here.

type Redis  = ProgramT Command
```

and ``Database.Redis.IO.Client``, an internal module of [redis-io](https://hackage.haskell.org/package/redis-io), defines how
to interpret the redis commands:

```haskell
eval f conn red = run conn [] red
  where
    run :: Connection -> [IO ()] -> Redis IO a -> IO (a, [IO ()])
    run h ii c = do
        r <- viewT c
        case r of
            Return a -> return (a, ii)

            -- Connection
            Ping   x :>>= k -> f h x (matchStr "PING" "PONG") >>= \(a, i) -> run h (i:ii) $ k a
            Echo   x :>>= k -> f h x (readBulk "ECHO")        >>= \(a, i) -> run h (i:ii) $ k a
            Auth   x :>>= k -> f h x (matchStr "AUTH" "OK")   >>= \(a, i) -> run h (i:ii) $ k a
            Quit   x :>>= k -> f h x (matchStr "QUIT" "OK")   >>= \(a, i) -> run h (i:ii) $ k a
            Select x :>>= k -> f h x (matchStr "SELECT" "OK") >>= \(a, i) -> run h (i:ii) $ k a

            -- Many more here, snipped
```

### language-puppet

The internal module ``Puppet.Interpreter.Types`` of [language-puppet](https://hackage.haskell.org/package/language-puppet)
defines an interpreter instruction type:

```haskell
data InterpreterInstr a where
    GetNativeTypes      :: InterpreterInstr (Container NativeTypeMethods)
    GetStatement        :: TopLevelType -> Text -> InterpreterInstr Statement
    ComputeTemplate     :: Either Text T.Text -> InterpreterState -> InterpreterInstr T.Text
    ExternalFunction    :: Text -> [PValue] -> InterpreterInstr PValue
    GetNodeName         :: InterpreterInstr Text
    HieraQuery          :: Container Text -> T.Text -> HieraQueryType -> InterpreterInstr (Maybe PValue)
    GetCurrentCallStack :: InterpreterInstr [String]
    IsIgnoredModule     :: Text -> InterpreterInstr Bool
    IsExternalModule    :: Text -> InterpreterInstr Bool
    IsStrict            :: InterpreterInstr Bool
    PuppetPaths         :: InterpreterInstr PuppetDirPaths
    -- Many more here, snipped.
```

Then ``Puppet.Interpreter.IO`` provides:

```haskell
-- The internal (not exposed) eval function
eval :: Monad m
     => InterpreterReader m
     -> InterpreterState
     -> ProgramViewT InterpreterInstr (State InterpreterState) a
     -> m (Either PrettyError a, InterpreterState, InterpreterWriter)
eval _ s (Return x) = return (Right x, s, mempty)
eval r s (a :>>= k) =
    let runInstr = interpretMonad r s . k -- run one instruction
        thpe = interpretMonad r s . throwPosError . getError
        pdb = r^.readerPdbApi
        strFail iof errf = iof >>= \case
            Left rr -> thpe (errf (string rr))
            Right x -> runInstr x
        canFail iof = iof >>= \case
            S.Left err -> thpe err
            S.Right x -> runInstr x
        canFailX iof = runExceptT iof >>= \case
            Left err -> thpe err
            Right x -> runInstr x
        logStuff x c = (_3 %~ (x <>)) <$> c
    in  case a of
            IsStrict                     -> runInstr (r ^. readerIsStrict)
            ExternalFunction fname args  -> case r ^. readerExternalFunc . at fname of
                                                Just fn -> interpretMonad r s ( fn args >>= k)
                                                Nothing -> thpe (PrettyError ("Unknown function: " <> ttext fname))
            GetStatement topleveltype toplevelname
                                         -> canFail ((r ^. readerGetStatement) topleveltype toplevelname)
            ComputeTemplate fn stt       -> canFail ((r ^. readerGetTemplate) fn stt r)
            WriterTell t                 -> logStuff t (runInstr ())
            WriterPass _                 -> thpe "WriterPass"
            WriterListen _               -> thpe "WriterListen"
            PuppetPaths                  -> runInstr (r ^. readerPuppetPaths)
            GetNativeTypes               -> runInstr (r ^. readerNativeTypes)
            ErrorThrow d                 -> return (Left d, s, mempty)
            ErrorCatch _ _               -> thpe "ErrorCatch"
            GetNodeName                  -> runInstr (r ^. readerNodename)
            -- More cases here, snipped.
```

Since ``InterpreterInstr`` is a normal data type, it's possible to
write an instance of ``Pretty`` so that warnings or error messages
look nicer:

```haskell
-- Puppet.Interpreter.PrettyPrinter

instance Pretty (InterpreterInstr a) where

    ...

    pretty (ExternalFunction fn args)  = pf (ttext fn) (map pretty args)
    pretty GetNodeName                 = pf "GetNodeName" []
    pretty (HieraQuery _ q _)          = pf "HieraQuery" [ttext q]
    pretty GetCurrentCallStack         = pf "GetCurrentCallStack" []
    pretty (ErrorThrow rr)             = pf "ErrorThrow" [getError rr]
    pretty (ErrorCatch _ _)            = pf "ErrorCatch" []
    pretty (WriterTell t)              = pf "WriterTell" (map (pretty . view _2) t)
    pretty (WriterPass _)              = pf "WriterPass" []
```

## References

* [The Operational Monad Tutorial](http://apfelmus.nfshost.com/articles/operational-monad.html)
* [Control.Monad.Operational](https://hackage.haskell.org/package/operational)
* [operational's reverse dependencies](http://packdeps.haskellers.com/reverse/operational)
* [Conquering Hadoop with Haskell and Ozgun Ataman](https://vimeo.com/90189610)
