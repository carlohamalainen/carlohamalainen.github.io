---
id: 696
title: Classy mtl
date: 2015-07-20T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2015/07/20/classy-mtl/
permalink: /2015/07/20/classy-mtl/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
This post has a minimal stand-alone example of the classy lenses and prisms from [George Wilson’s](http://twitter.com/GeorgeTalksCode) [talk](http://talks.bfpg.org/talks/2015-06-09.next_level_mtl.html) about mtl. The source code for George’s talk is here: <https://github.com/gwils/next-level-mtl-with-classy-optics>.

Literate Haskell source for this post is here: <https://github.com/carlohamalainen/playground/tree/master/haskell/classy-mtl>.

First, some imports: 

<pre>&gt; {-# LANGUAGE OverloadedStrings    #-}
&gt; {-# LANGUAGE TemplateHaskell      #-}
&gt;
&gt; module Classy where
&gt;
&gt; import Control.Lens
&gt; import Control.Monad.Except
&gt; import Control.Monad.Reader
&gt; import Data.Text
</pre>

## Toy program &#8211; uses the network and a database 

The case study in George’s talk was a program that has to interact with a database and the network. We have a type for the database connection info: 

<pre>&gt; type DbConnection = Text
&gt; type DbSchema     = Text
&gt;
&gt; data DbConfig = DbConfig
&gt;     { _dbConn :: DbConnection
&gt;     , _schema :: DbSchema
&gt;     }
</pre>

For the network we have a port and some kind of SSL setting: 

<pre>&gt; type Port = Integer
&gt; type Ssl  = Text
&gt;
&gt; data NetworkConfig = NetworkConfig
&gt;     { _port     :: Port
&gt;     , _ssl      :: Ssl
&gt;     }
</pre>

At the top level, our application has a database and a network configuration: 

<pre>&gt; data AppConfig = AppConfig
&gt;     { _appDbConfig   :: DbConfig
&gt;     , _appNetConfig  :: NetworkConfig
&gt;     }
</pre>

Types for errors that we see when dealing with the database and the network: 

<pre>&gt; data DbError = QueryError Text | InvalidConnection
&gt;
&gt; data NetworkError = Timeout Int | ServerOnFire
&gt;
&gt; data AppError = AppDbError  { dbError  :: DbError      }
&gt;               | AppNetError { netError :: NetworkError }
</pre>

## Classy lenses and prisms 

Use Template Haskell to make all of the classy lenses and prisms. Documentation for makeClassy and makeClassyPrisms is in [Control.Lens.TH](https://hackage.haskell.org/package/lens-4.11/docs/Control-Lens-TH.html).

<pre>&gt; makeClassy ''DbConfig
&gt; makeClassy ''NetworkConfig
&gt; makeClassy ''AppConfig
&gt;
&gt; makeClassyPrisms ''DbError
&gt; makeClassyPrisms ''NetworkError
&gt; makeClassyPrisms ''AppError
</pre>

We get the following typeclasses: 

  * HasDbConfig 
  * HasNetworkConfig 
  * HasAppConfig 
  * AsNetworkError 
  * AsDbError 
  * AsAppError 

For example, here is the generated class HasDbConfig: 

<pre>*Classy&gt; :i HasDbConfig
class HasDbConfig c_a6IY where
  dbConfig :: Functor f =&gt; (DbConfig -&gt; f DbConfig) -&gt; c0 -&gt; f c0
  dbConn   :: Functor f =&gt; (DbConnection -&gt; f DbConnection) -&gt; c0 -&gt; f c0
  schema   :: Functor f =&gt; (DbSchema -&gt; f DbSchema) -&gt; c0 -&gt; f c0
instance HasDbConfig DbConfig -- Defined at Classy.lhs:58:3
</pre>

If we write HasDbConfig r in the class constraints of a type signature then we can use the lenses dbConfig, dbConn, and schema to get the entire config, connection string, and schema, from something of type r.

In contrast, the constraint AsNetworkError r means that we can use the prisms \_NetworkError, \_Timeout, and _ServerOnFire on a value of type r to get at the network error details.

<pre>*Classy&gt; :i AsNetworkError
class AsNetworkError r_a759 where
  _NetworkError ::
    (Choice p, Control.Applicative.Applicative f) =&gt;
    p NetworkError (f NetworkError) -&gt; p r0 (f r0)

  _Timeout ::
    (Choice p, Control.Applicative.Applicative f) =&gt;
    p Int (f Int) -&gt; p r0 (f r0)

  _ServerOnFire ::
    (Choice p, Control.Applicative.Applicative f) =&gt;
    p () (f ()) -&gt; p r0 (f r0)
    -- Defined at Classy.lhs:63:3

instance AsNetworkError NetworkError -- Defined at Classy.lhs:63:3
</pre>

## Using the class constraints 

The first function is loadFromDb which uses a reader environment for database configuration, can throw a database error, and do IO actions. 

<pre>&gt; loadFromDb :: ( MonadError e m,
&gt;                 MonadReader r m,
&gt;                 AsDbError e,
&gt;                 HasDbConfig r,
&gt;                 MonadIO m) =&gt; m Text
&gt; loadFromDb = do
&gt;
&gt;   -- Due to "MonadReader r m" and "HasDbConfig r"
&gt;   -- we can ask for the database config:
&gt;   rdr    let dbconf  = rdr ^. dbConfig :: DbConfig
&gt;
&gt;   -- We can ask for the connection string directly:
&gt;   let connstr  = rdr ^. dbConn :: DbConnection
&gt;
&gt;   -- We have "AsDbError e", so we can throw a DB error:
&gt;   throwError $ (_InvalidConnection #) ()
&gt;   throwError $ (_QueryError #) "Bad SQL!"
&gt;
&gt;   return "foo"
</pre>

Another function, sendOverNet uses a reader environment with a network config, throws network errors, and does IO actions. 

<pre>&gt; sendOverNet :: ( MonadError e m,
&gt;                  MonadReader r m,
&gt;                  AsNetworkError e,
&gt;                  AsAppError e,
&gt;                  HasNetworkConfig r,
&gt;                  MonadIO m) =&gt; Text -&gt; m ()
&gt; sendOverNet mydata = do
&gt;
&gt;   -- We have "MonadReader r m" and "HasNetworkConfig r"
&gt;   -- so we can ask about the network config:
&gt;   rdr    let netconf = rdr ^. networkConfig  :: NetworkConfig
&gt;       p       = rdr ^. port           :: Port
&gt;       s       = rdr ^. ssl            :: Ssl
&gt;
&gt;   liftIO $ putStrLn $ "Pretending to connect to the network..."
&gt;
&gt;   -- We have "AsNetworkError e" so we can throw a network error:
&gt;   throwError $ (_NetworkError #) (Timeout 100)
&gt;
&gt;   -- We have "AsAppError e" so we can throw an application-level error:
&gt;   throwError $ (_AppNetError #) (Timeout 100)
&gt;
&gt;   return ()
</pre>

If we load from the database and also send over the network then we get extra class constraints: 

<pre>&gt; loadAndSend :: ( AsAppError e,
&gt;                  AsNetworkError e,
&gt;                  AsDbError e,
&gt;                  HasNetworkConfig r,
&gt;                  HasDbConfig r,
&gt;                  MonadReader r m,
&gt;                  MonadError e m,
&gt;                  MonadIO m) =&gt; m ()
&gt; loadAndSend = do
&gt;   liftIO $ putStrLn "Loading from the database..."
&gt;   t 
&gt;   liftIO $ putStrLn "Sending to the network..."
&gt;   sendOverNet t
</pre>

## Things that won’t compile 

We can’t throw the database error InvalidConnection without the right class constraint: 

<pre>&gt; nope1 :: (MonadError e m, AsNetworkError e) =&gt; m ()
&gt; nope1 = throwError $ (_InvalidConnection #) ()
</pre>

<font color="red"></p> 

<pre>
Could not deduce (AsDbError e)
arising from a use of ‘_InvalidConnection’
</pre>

<p>
  </font>
</p>

<p>
  We can’t throw an application error if we are only allowed to throw network errors, even though this specific application error is a network error:
</p>

<pre>&gt; nope2 :: (MonadError e m, AsNetworkError e) =&gt; m ()
&gt; nope2 = throwError $ (_AppNetError #) (Timeout 100)
</pre>

<p>
  <font color="red"></p> 
  
  <pre>
Could not deduce (AsAppError e)
arising from a use of ‘_AppNetError’
</pre>
  
  <p>
    </font>
  </p>
  
  <p>
    We can’t get the network config from a value of type r if we only have the constraint about having the database config:
  </p>
  
  <pre>&gt; nope3 :: (MonadReader r m, HasDbConfig r) =&gt; m ()
&gt; nope3 = do
&gt;   rdr    let netconf = rdr ^. networkConfig
&gt;
&gt;   return ()
</pre>
  
  <p>
    <font color="red"></p> 
    
    <pre>
Could not deduce (HasNetworkConfig r)
arising from a use of ‘networkConfig’
</pre>
    
    <p>
      </font>
    </p>
    
    <h2>
      What is the #?
    </h2>
    
    <p>
      The # is an infix alias for <a href="https://hackage.haskell.org/package/lens-4.11/docs/Control-Lens-Review.html#v:review">review</a>. More details are in <a href="https://hackage.haskell.org/package/lens-4.11/docs/Control-Lens-Review.html">Control.Lens.Review</a>.
    </p>
    
    <pre>
*Classy&gt; :t review _InvalidConnection ()
review _InvalidConnection () :: AsDbError e =&gt; e

*Classy&gt; :t throwError $ review _InvalidConnection ()
throwError $ review _InvalidConnection () :: (AsDbError e, MonadError e m) =&gt; m a
</pre>
    
    <h2>
      What is the monad transformer stack?
    </h2>
    
    <p>
      We didn’t specify it! The functions loadFromDb and sendOverNet have the general monad m in their type signatures, not a specific transformer stack like ReaderT AppConfig (ExceptT AppError IO) a.
    </p>
    
    <h2>
      What else?
    </h2>
    
    <p>
      <a href="http://twitter.com/benkolera">Ben Kolera</a> did a talk at <a href="http://bfpg.org">BFPG</a> about <a href="http://talks.bfpg.org/talks/2015-02-24.monad_transformers.html">stacking monad transformers</a>. He later modified the code from his talk to use the classy lens/prism approach. You can see the code <a href="https://github.com/benkolera/talk-stacking-your-monads/tree/master/code">before</a> and <a href="https://github.com/benkolera/talk-stacking-your-monads/tree/master/code-classy">after</a>, and also see a <a href="https://github.com/benkolera/talk-stacking-your-monads/blob/master/classy.diff">diff</a>. As far as I could see there is <a href="https://github.com/benkolera/talk-stacking-your-monads/blob/master/code-classy/src/Csv.hs#L60">one spot</a> in the code where an error is thrown, which motivated me to create the stand-alone example in this post with the body for loadFromDb and sendOverNet sketched out.
    </p>