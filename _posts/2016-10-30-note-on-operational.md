---
id: 834
title: Note on operational
date: 2016-10-30T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2016/10/30/note-on-operational/
permalink: /2016/10/30/note-on-operational/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
I was looking at an old post of mine on [free monads](https://carlo-hamalainen.net/blog/2014/6/7/notes-on-free-monads) and wondered what it would look like using [operational](https://hackage.haskell.org/package/operational). Here we go! 

(Literate Haskell source for this blog post is [here](https://github.com/carlohamalainen/playground/tree/master/haskell/note-on-operational).) 

First, some imports. We use [GADTs](https://wiki.haskell.org/GADTs_for_dummies) for our instruction type. 

<pre>&gt; {-# LANGUAGE GADTs #-}
&gt;
&gt; module Note where
&gt;
&gt; import Control.Monad
&gt; import Control.Monad.Operational
&gt; import Test.QuickCheck
&gt; import qualified Data.Map as M
</pre>

We want to encode a few instructions that operate on a data store. For the moment we don’t care about the implementation, just the underlying actions that we can perform: create a value, list all values, retrieve a particular value, and delete a value. 

Suppose we have an index of type i and values of type v. Then DataStoreInstruction is: 

<pre>&gt; data DataStoreInstruction i v a where
&gt;     Create   :: v -&gt; DataStoreInstruction i v i
&gt;     List     ::      DataStoreInstruction i v [v]
&gt;     Retrieve :: i -&gt; DataStoreInstruction i v (Maybe v)
&gt;     Delete   :: i -&gt; DataStoreInstruction i v ()
</pre>

Intuitively, Create takes a value of type v and returns an instruction, which on interpretation gives a value of type i (the last type parameter).

List doesn’t take any argument and produces a list of type v, i.e. [v]. The only odd one is Delete which doesn’t return anything, so it has a () in the last type variable.

A sequence of DataStoreInstructions is a program, which we get using the [Program](https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#t:Program) constructor: 

<pre>&gt; type DataStoreProgram i v a = Program (DataStoreInstruction i v) a
</pre>

where the index has type i, the values have type v, and the overall result of the program has type a.

To more easily construct programs, use [singleton](https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#v:singleton): 

<pre>&gt; create :: v -&gt; DataStoreProgram i v i
&gt; create = singleton . Create
&gt;
&gt; list :: DataStoreProgram i v [v]
&gt; list = singleton List
&gt;
&gt; retrieve :: i -&gt; DataStoreProgram i v (Maybe v)
&gt; retrieve = singleton . Retrieve
&gt;
&gt; delete :: i -&gt; DataStoreProgram i v ()
&gt; delete = singleton . Delete
</pre>

Now we can write programs in this DSL! All the usual monad things are at our disposal:

<pre>&gt; -- Insert a few values and return a list
&gt; -- of all values:
&gt; doSomeThings :: DataStoreProgram Int Int [Int]
&gt; doSomeThings = do
&gt;     ix3      ix4      delete ix3
&gt;     ix5      list
</pre>

<pre>&gt; -- Insert all the supplied values and
&gt; -- return a list of indexes as well as a
&gt; -- list of final values (which should be empty).
&gt; insertValues :: [Int] -&gt; DataStoreProgram Int Int ([Int], [Int])
&gt; insertValues xs = do
&gt;     ixs      forM_ ixs delete -- delete everything
&gt;     final 
&gt;     return (ixs, final)
</pre>

The last step is to write an interpreter. We do this using [view](https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#v:view) and pattern matching on the constructors of DataStoreInstruction. We use [:>>=](https://hackage.haskell.org/package/operational-0.2.3.4/docs/Control-Monad-Operational.html#v::-62--62--61-) to break apart the program. As the documentation says, (someInstruction :>>= k) means someInstruction and the remaining program is given by the function k.

Here we interpret the program using a Map as the underlying data store:

<pre>&gt; interpretMap :: Ord a =&gt; DataStoreProgram a a b -&gt; (M.Map a a -&gt; b)
&gt; interpretMap = eval . view
&gt;   where
&gt;     eval (Return x)          m = x
&gt;     eval (Create   v :&gt;&gt;= k) m = interpretMap (k v)              (M.insert v v m)
&gt;     eval (List       :&gt;&gt;= k) m = interpretMap (k (M.elems m))    m
&gt;     eval (Retrieve i :&gt;&gt;= k) m = interpretMap (k $ M.lookup i m) m
&gt;     eval (Delete   i :&gt;&gt;= k) m = interpretMap (k ())             (M.delete i m)
</pre>

If we wanted to we could flatten a program out to a string: 

<pre>&gt; interpretString :: (Show a, Show b) =&gt; DataStoreProgram a a b -&gt; String
&gt; interpretString = eval . view
&gt;   where
&gt;     eval (Return x)          = "Return "   ++ show x
&gt;     eval (Create   v :&gt;&gt;= k) = "Create("   ++ show v ++ "); "  ++ interpretString (k v)
&gt;     eval (List       :&gt;&gt;= k) = "List; "                        ++ interpretString (k [])
&gt;     eval (Retrieve i :&gt;&gt;= k) = "Retrieve(" ++ show i ++ "); "  ++ interpretString (k $ Just i)
&gt;     eval (Delete   i :&gt;&gt;= k) = "Delete("   ++ show i ++ "); "  ++ interpretString (k ())
</pre>

Maybe we have some super-important business need for storing Int/Int key value maps with a Postgresql backend. We could target this by writing an interpreter that uses [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple-0.5.2.1/docs/Database-PostgreSQL-Simple.html):

<pre>&gt; data Connection = Connection -- cheating for this example
&gt;
&gt; interprestPostgresql :: DataStoreProgram Int Int b -&gt; (Connection -&gt; IO b)
&gt; interprestPostgresql = eval . view
&gt;   where
&gt;     eval (Return x)          _ = return x
&gt;     eval (Create   v :&gt;&gt;= k) c = interprestPostgresql (k v) (insertPsql c v)
&gt;     eval (List       :&gt;&gt;= k) c = do allValues                                      interprestPostgresql (k allValues) c
&gt;     eval (Retrieve i :&gt;&gt;= k) c = do v                                      interprestPostgresql (k v) c
&gt;     eval (Delete   i :&gt;&gt;= k) c = deletePsql c i &gt;&gt; interprestPostgresql (k ()) c
&gt;
&gt;     -- Exercises for the reader.
&gt;     insertPsql c v = undefined
&gt;     getAllPsql c   = undefined
&gt;     lookupPsql c i = undefined
&gt;     deletePsql c i = undefined
</pre>

Running the programs: 

<pre>*Note&gt; interpretMap doSomeThings M.empty
[4,5]

*Note&gt; interpretString doSomeThings
"Create(3); Create(4); Delete(3); Create(5); List; Return []"

*Note&gt; interpretMap (insertValues [1..10]) M.empty
([1,2,3,4,5,6,7,8,9,10],[])

*Note&gt; interpretString (insertValues [1..10])
"Create(1); Create(2); Create(3); Create(4); Create(5); Create(6);
 Create(7); Create(8); Create(9); Create(10); Delete(1); Delete(2);
  Delete(3); Delete(4); Delete(5); Delete(6); Delete(7); Delete(8);
   Delete(9); Delete(10); List; Return ([1,2,3,4,5,6,7,8,9,10],[])"
</pre>

### QuickCheck 

It’s always good to write some tests: 

<pre>&gt; prop_insert_then_delete :: [Int] -&gt; Bool
&gt; prop_insert_then_delete xs = null $ interpretMap (f xs) M.empty
&gt;   where
&gt;     f :: [Int] -&gt; DataStoreProgram Int Int [Int]
&gt;     f is = do
&gt;         ixs          forM_ ixs delete
&gt;         list
</pre>

<pre>&gt; prop_create :: Positive Int -&gt; Bool
&gt; prop_create (Positive n) = let ns = [1..n] in ns == interpretMap (f ns) M.empty
&gt;   where
&gt;     f = mapM create
</pre>

<pre>*Note&gt; quickCheck prop_insert_then_delete
+++ OK, passed 100 tests.
 *Note&gt;
*Note&gt;
*Note&gt;
*Note&gt; quickCheck prop_create
+++ OK, passed 100 tests.
</pre>

Over time we find out that the interpreter that uses Map is too slow, so we write a new one using a fancy data structure: 

<pre>&gt; -- Uses fancy tuned data structure.
&gt; interpretMapFast :: Ord a =&gt; DataStoreProgram a a b -&gt; (M.Map a a -&gt; b)
&gt; interpretMapFast = undefined -- So fancy!
</pre>

Now we can compare implementations using QuickCheck. Nice! 

<pre>&gt; prop_fast_inserts :: [Int] -&gt; Bool
&gt; prop_fast_inserts xs = (interpretMapFast xs' M.empty) == (interpretMap xs' M.empty)
&gt;   where
&gt;     xs' = f xs
&gt;
&gt;     f :: [Int] -&gt; DataStoreProgram Int Int [Int]
&gt;     f is = do
&gt;         ixs          list
</pre>

## Use of operational in larger projects 

Here are a few samples of the operational package in action. For more, see the [reverse dependencies](http://packdeps.haskellers.com/reverse/operational) on Hackage. 

### Hadron 

I first heard about operational from this talk at the New York Haskell meetup: [Conquering Hadoop with Haskell and Ozgun Ataman](https://vimeo.com/90189610).

Here is the ConI type from [Hadron.Controller](https://github.com/Soostone/hadron/blob/master/src/Hadron/Controller.hs): 

<pre>&gt; data ConI a where
&gt;     Connect :: forall i o. MapReduce i o
&gt;             -&gt; [Tap i] -&gt; Tap o
&gt;             -&gt; Maybe String
&gt;             -&gt; ConI ()
&gt;
&gt;     MakeTap :: Protocol' a -&gt; ConI (Tap a)
&gt;
&gt;     BinaryDirTap
&gt;         :: FilePath
&gt;         -&gt; (FilePath -&gt; Bool)
&gt;         -&gt; ConI (Tap (FilePath, B.ByteString))
&gt;
&gt;     ConIO :: IO a -&gt; ConI a
&gt;
&gt;     OrchIO :: IO a -&gt; ConI ()
&gt;     -- Only the orchestrator performs action
&gt;
&gt;     NodeIO :: IO a -&gt; ConI a
&gt;     -- Only the nodes perform action
&gt;
&gt;     SetVal :: String -&gt; B.ByteString -&gt; ConI ()
&gt;
&gt;     GetVal :: String -&gt; ConI (Maybe B.ByteString)
&gt;
&gt;     RunOnce :: Serialize a =&gt; IO a -&gt; ConI a
&gt;     -- Only run on orchestrator, then make available to all the
&gt;     -- nodes via HDFS.
</pre>

There is the distinction between the single orchestrator node, which runs OrchIO and can’t run NodeIO, and worker nodes that can’t run OrchIO but can run NodeIO. In the orchestrate, trying to evaluate a NodeIO results in an error:

<pre>&gt; orchestrate
&gt;     :: (MonadMask m, MonadIO m, Applicative m)
&gt;     =&gt; Controller a
&gt;     -&gt; RunContext
&gt;     -&gt; RerunStrategy
&gt;     -&gt; ContState
&gt;     -&gt; m (Either String a)
&gt; orchestrate (Controller p) settings rr s = do
&gt;     bracket
&gt;       (liftIO $ openFile "hadron.log" AppendMode)
&gt;       (liftIO . hClose)
&gt;       (_h -&gt; do echoInfo ()  "Initiating orchestration..."
&gt;                  evalStateT (runExceptT (go p)) s)
&gt;     where
&gt;       go = eval . O.view
&gt;
&gt;       eval (Return a) = return a
&gt;       eval (i :&gt;&gt;= f) = eval' i &gt;&gt;= go . f
&gt;
&gt;       eval' :: (Functor m, MonadIO m) =&gt; ConI a -&gt; ExceptT String (StateT ContState m) a
&gt;       eval' (ConIO  f) = liftIO f
&gt;       eval' (OrchIO f) = void $ liftIO f
&gt;       eval' (NodeIO _) = return (error "NodeIO can't be used in the orchestrator decision path")
</pre>

Meanwhile, worker nodes ignore an OrchIO and [lift the NodeIO action](https://github.com/Soostone/hadron/blob/8f78faecad49ac0cc35484a70c8b186ae916920b/src/Hadron/Controller.hs#L1022-L1038). 

### Chart 

The [Graphics.Rendering.Chart.Backend.Impl](http://hackage.haskell.org/package/Chart-1.8/docs/Graphics-Rendering-Chart-Backend-Impl.html) module defines the the backend instructions: 

<pre>&gt; data ChartBackendInstr a where
&gt;   StrokePath :: Path -&gt; ChartBackendInstr ()
&gt;   FillPath   :: Path -&gt; ChartBackendInstr ()
&gt;   GetTextSize :: String -&gt; ChartBackendInstr TextSize
&gt;   DrawText    :: Point -&gt; String -&gt; ChartBackendInstr ()
&gt;   GetAlignments :: ChartBackendInstr AlignmentFns
&gt;   WithTransform  :: Matrix -&gt;  Program ChartBackendInstr a -&gt; ChartBackendInstr a
&gt;   WithFontStyle  :: FontStyle -&gt; Program ChartBackendInstr a -&gt; ChartBackendInstr a
&gt;   WithFillStyle  :: FillStyle -&gt; Program ChartBackendInstr a -&gt; ChartBackendInstr a
&gt;   WithLineStyle  :: LineStyle -&gt; Program ChartBackendInstr a -&gt; ChartBackendInstr a
&gt;   WithClipRegion :: Rect -&gt; Program ChartBackendInstr a -&gt; ChartBackendInstr a
&gt;
&gt; type BackendProgram a = Program ChartBackendInstr a
</pre>

Then the [Graphics.Rendering.Chart.Backend.Cairo](https://hackage.haskell.org/package/Chart-cairo-1.8/docs/Graphics-Rendering-Chart-Backend-Cairo.html) module provides a way to run a BackendProgram for the cairo graphics engine: 

<pre>&gt; runBackend' :: CEnv -&gt; BackendProgram a -&gt; C.Render a
&gt; runBackend' env m = eval env (view m)
&gt;   where
&gt;     eval :: CEnv -&gt; ProgramView ChartBackendInstr a -&gt; C.Render a
&gt;     eval env (Return v)= return v
&gt;     eval env (StrokePath p :&gt;&gt;= f) = cStrokePath env p &gt;&gt;= step env f
&gt;     eval env (FillPath p :&gt;&gt;= f) = cFillPath env p &gt;&gt;= step env f
&gt;     eval env (GetTextSize s :&gt;&gt;= f) = cTextSize s &gt;&gt;= step env f
&gt;     eval env (DrawText p s :&gt;&gt;= f) = cDrawText env p s &gt;&gt;= step env f
&gt;     eval env (GetAlignments :&gt;&gt;= f) = return (ceAlignmentFns env) &gt;&gt;= step env f
&gt;     eval env (WithTransform m p :&gt;&gt;= f) = cWithTransform env m p &gt;&gt;= step env f
&gt;     eval env (WithFontStyle font p :&gt;&gt;= f) = cWithFontStyle env font p &gt;&gt;= step env f
&gt;     eval env (WithFillStyle fs p :&gt;&gt;= f) = cWithFillStyle env fs p &gt;&gt;= step env f
&gt;     eval env (WithLineStyle ls p :&gt;&gt;= f) = cWithLineStyle env ls p &gt;&gt;= step env f
&gt;     eval env (WithClipRegion r p :&gt;&gt;= f) = cWithClipRegion env r p &gt;&gt;= step env f
</pre>

Meanwhile, [Graphics.Rendering.Chart.Backend.Diagrams](https://hackage.haskell.org/package/Chart-diagrams-1.8/docs/Graphics-Rendering-Chart-Backend-Diagrams.html) does the same but for [diagrams](https://hackage.haskell.org/package/diagrams-lib): 

<pre>&gt; runBackend' tr m = eval tr $ view $ m
&gt;   where
&gt;     eval :: (D.Renderable (D.Path V2 (N b)) b, D.Renderable t b, D.TypeableFloat (N b))
&gt;          =&gt; TextRender b t -&gt; ProgramView ChartBackendInstr a
&gt;          -&gt; DState (N b) (D.QDiagram b V2 (N b) Any, a)
&gt;     eval tr (Return v) = return (mempty, v)
&gt;     eval tr (StrokePath p   :&gt;&gt;= f) = dStrokePath  p   # step tr f
&gt;     eval tr (FillPath   p   :&gt;&gt;= f) = dFillPath    p   # step tr f
&gt;     eval tr@TextRenderSvg    (DrawText   p s :&gt;&gt;= f) = dDrawTextSvg    p s # step tr f
&gt;     eval tr@TextRenderNative (DrawText   p s :&gt;&gt;= f) = dDrawTextNative p s # step tr f
&gt;     eval tr (GetTextSize  s :&gt;&gt;= f) = dTextSize      s = step tr f
&gt;     eval tr (GetAlignments  :&gt;&gt;= f) = dAlignmentFns    = step tr f
&gt;     eval tr (WithTransform m p :&gt;&gt;= f)  = dWithTransform  tr m  p = step tr f
&gt;     eval tr (WithFontStyle fs p :&gt;&gt;= f) = dWithFontStyle  tr fs p = step tr f
&gt;     eval tr (WithFillStyle fs p :&gt;&gt;= f) = dWithFillStyle  tr fs p = step tr f
&gt;     eval tr (WithLineStyle ls p :&gt;&gt;= f) = dWithLineStyle  tr ls p = step tr f
&gt;     eval tr (WithClipRegion r p :&gt;&gt;= f) = dWithClipRegion tr r  p = step tr f
</pre>

### redis-io 

The [Data.Redis.Command](https://hackage.haskell.org/package/redis-resp-0.4.0/docs/Data-Redis-Command.html) module defines heaps of commands: 

<pre>&gt; data Command :: * -&gt; * where
&gt;     -- Connection
&gt;     Ping   :: Resp -&gt; Command ()
&gt;     Echo   :: FromByteString a =&gt; Resp -&gt; Command a
&gt;     Auth   :: Resp -&gt; Command ()
&gt;     Quit   :: Resp -&gt; Command ()
&gt;     Select :: Resp -&gt; Command ()
&gt;
&gt;     -- Many more here.
&gt;
&gt; type Redis  = ProgramT Command
</pre>

and Database.Redis.IO.Client, an internal module of [redis-io](https://hackage.haskell.org/package/redis-io), defines how to interpret the redis commands: 

<pre>&gt; eval f conn red = run conn [] red
&gt;   where
&gt;     run :: Connection -&gt; [IO ()] -&gt; Redis IO a -&gt; IO (a, [IO ()])
&gt;     run h ii c = do
&gt;         r          case r of
&gt;             Return a -&gt; return (a, ii)
&gt;
&gt;             -- Connection
&gt;             Ping   x :&gt;&gt;= k -&gt; f h x (matchStr "PING" "PONG") &gt;&gt;= (a, i) -&gt; run h (i:ii) $ k a
&gt;             Echo   x :&gt;&gt;= k -&gt; f h x (readBulk "ECHO")        &gt;&gt;= (a, i) -&gt; run h (i:ii) $ k a
&gt;             Auth   x :&gt;&gt;= k -&gt; f h x (matchStr "AUTH" "OK")   &gt;&gt;= (a, i) -&gt; run h (i:ii) $ k a
&gt;             Quit   x :&gt;&gt;= k -&gt; f h x (matchStr "QUIT" "OK")   &gt;&gt;= (a, i) -&gt; run h (i:ii) $ k a
&gt;             Select x :&gt;&gt;= k -&gt; f h x (matchStr "SELECT" "OK") &gt;&gt;= (a, i) -&gt; run h (i:ii) $ k a
&gt;
&gt;             -- Many more here, snipped
</pre>

### language-puppet 

The internal module Puppet.Interpreter.Types of [language-puppet](https://hackage.haskell.org/package/language-puppet) defines an interpreter instruction type: 

<pre>&gt; data InterpreterInstr a where
&gt;     GetNativeTypes      :: InterpreterInstr (Container NativeTypeMethods)
&gt;     GetStatement        :: TopLevelType -&gt; Text -&gt; InterpreterInstr Statement
&gt;     ComputeTemplate     :: Either Text T.Text -&gt; InterpreterState -&gt; InterpreterInstr T.Text
&gt;     ExternalFunction    :: Text -&gt; [PValue] -&gt; InterpreterInstr PValue
&gt;     GetNodeName         :: InterpreterInstr Text
&gt;     HieraQuery          :: Container Text -&gt; T.Text -&gt; HieraQueryType -&gt; InterpreterInstr (Maybe PValue)
&gt;     GetCurrentCallStack :: InterpreterInstr [String]
&gt;     IsIgnoredModule     :: Text -&gt; InterpreterInstr Bool
&gt;     IsExternalModule    :: Text -&gt; InterpreterInstr Bool
&gt;     IsStrict            :: InterpreterInstr Bool
&gt;     PuppetPaths         :: InterpreterInstr PuppetDirPaths
&gt;     -- Many more here, snipped.
</pre>

Then Puppet.Interpreter.IO provides: 

<pre>&gt; -- The internal (not exposed) eval function
&gt; eval :: Monad m
&gt;      =&gt; InterpreterReader m
&gt;      -&gt; InterpreterState
&gt;      -&gt; ProgramViewT InterpreterInstr (State InterpreterState) a
&gt;      -&gt; m (Either PrettyError a, InterpreterState, InterpreterWriter)
&gt; eval _ s (Return x) = return (Right x, s, mempty)
&gt; eval r s (a :&gt;&gt;= k) =
&gt;     let runInstr = interpretMonad r s . k -- run one instruction
&gt;         thpe = interpretMonad r s . throwPosError . getError
&gt;         pdb = r^.readerPdbApi
&gt;         strFail iof errf = iof &gt;&gt;= case
&gt;             Left rr -&gt; thpe (errf (string rr))
&gt;             Right x -&gt; runInstr x
&gt;         canFail iof = iof &gt;&gt;= case
&gt;             S.Left err -&gt; thpe err
&gt;             S.Right x -&gt; runInstr x
&gt;         canFailX iof = runExceptT iof &gt;&gt;= case
&gt;             Left err -&gt; thpe err
&gt;             Right x -&gt; runInstr x
&gt;         logStuff x c = (_3 %~ (x ))  c
&gt;     in  case a of
&gt;             IsStrict                     -&gt; runInstr (r ^. readerIsStrict)
&gt;             ExternalFunction fname args  -&gt; case r ^. readerExternalFunc . at fname of
&gt;                                                 Just fn -&gt; interpretMonad r s ( fn args &gt;&gt;= k)
&gt;                                                 Nothing -&gt; thpe (PrettyError ("Unknown function: "  ttext fname))
&gt;             GetStatement topleveltype toplevelname
&gt;                                          -&gt; canFail ((r ^. readerGetStatement) topleveltype toplevelname)
&gt;             ComputeTemplate fn stt       -&gt; canFail ((r ^. readerGetTemplate) fn stt r)
&gt;             WriterTell t                 -&gt; logStuff t (runInstr ())
&gt;             WriterPass _                 -&gt; thpe "WriterPass"
&gt;             WriterListen _               -&gt; thpe "WriterListen"
&gt;             PuppetPaths                  -&gt; runInstr (r ^. readerPuppetPaths)
&gt;             GetNativeTypes               -&gt; runInstr (r ^. readerNativeTypes)
&gt;             ErrorThrow d                 -&gt; return (Left d, s, mempty)
&gt;             ErrorCatch _ _               -&gt; thpe "ErrorCatch"
&gt;             GetNodeName                  -&gt; runInstr (r ^. readerNodename)
&gt;             -- More cases here, snipped.
</pre>

Since InterpreterInstr is a normal data type, it’s possible to write an instance of Pretty so that warnings or error messages look nicer: 

<pre>&gt; -- Puppet.Interpreter.PrettyPrinter
&gt;
&gt; instance Pretty (InterpreterInstr a) where
&gt;
&gt;     ...
&gt;
&gt;     pretty (ExternalFunction fn args)  = pf (ttext fn) (map pretty args)
&gt;     pretty GetNodeName                 = pf "GetNodeName" []
&gt;     pretty (HieraQuery _ q _)          = pf "HieraQuery" [ttext q]
&gt;     pretty GetCurrentCallStack         = pf "GetCurrentCallStack" []
&gt;     pretty (ErrorThrow rr)             = pf "ErrorThrow" [getError rr]
&gt;     pretty (ErrorCatch _ _)            = pf "ErrorCatch" []
&gt;     pretty (WriterTell t)              = pf "WriterTell" (map (pretty . view _2) t)
&gt;     pretty (WriterPass _)              = pf "WriterPass" []
</pre>

## References 

  * [The Operational Monad Tutorial](http://apfelmus.nfshost.com/articles/operational-monad.html) 
  * [Control.Monad.Operational](https://hackage.haskell.org/package/operational) 
  * [operational’s reverse dependencies](http://packdeps.haskellers.com/reverse/operational) 
  * [Conquering Hadoop with Haskell and Ozgun Ataman](https://vimeo.com/90189610)