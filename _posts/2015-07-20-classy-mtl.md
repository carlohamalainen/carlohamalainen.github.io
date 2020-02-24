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

<pre>> {-# LANGUAGE OverloadedStrings    #-}
> {-# LANGUAGE TemplateHaskell      #-}
>
> module Classy where
>
> import Control.Lens
> import Control.Monad.Except
> import Control.Monad.Reader
> import Data.Text
</pre>

## Toy program -- uses the network and a database 

The case study in George’s talk was a program that has to interact with a database and the network. We have a type for the database connection info: 

<pre>> type DbConnection = Text
> type DbSchema     = Text
>
> data DbConfig = DbConfig
>     { _dbConn :: DbConnection
>     , _schema :: DbSchema
>     }
</pre>

For the network we have a port and some kind of SSL setting: 

<pre>> type Port = Integer
> type Ssl  = Text
>
> data NetworkConfig = NetworkConfig
>     { _port     :: Port
>     , _ssl      :: Ssl
>     }
</pre>

At the top level, our application has a database and a network configuration: 

<pre>> data AppConfig = AppConfig
>     { _appDbConfig   :: DbConfig
>     , _appNetConfig  :: NetworkConfig
>     }
</pre>

Types for errors that we see when dealing with the database and the network: 

<pre>> data DbError = QueryError Text | InvalidConnection
>
> data NetworkError = Timeout Int | ServerOnFire
>
> data AppError = AppDbError  { dbError  :: DbError      }
>               | AppNetError { netError :: NetworkError }
</pre>

## Classy lenses and prisms 

Use Template Haskell to make all of the classy lenses and prisms. Documentation for makeClassy and makeClassyPrisms is in [Control.Lens.TH](https://hackage.haskell.org/package/lens-4.11/docs/Control-Lens-TH.html).

<pre>> makeClassy ''DbConfig
> makeClassy ''NetworkConfig
> makeClassy ''AppConfig
>
> makeClassyPrisms ''DbError
> makeClassyPrisms ''NetworkError
> makeClassyPrisms ''AppError
</pre>

We get the following typeclasses: 

  * HasDbConfig 
  * HasNetworkConfig 
  * HasAppConfig 
  * AsNetworkError 
  * AsDbError 
  * AsAppError 

For example, here is the generated class HasDbConfig: 

<pre>*Classy> :i HasDbConfig
class HasDbConfig c_a6IY where
  dbConfig :: Functor f => (DbConfig -> f DbConfig) -> c0 -> f c0
  dbConn   :: Functor f => (DbConnection -> f DbConnection) -> c0 -> f c0
  schema   :: Functor f => (DbSchema -> f DbSchema) -> c0 -> f c0
instance HasDbConfig DbConfig -- Defined at Classy.lhs:58:3
</pre>

If we write HasDbConfig r in the class constraints of a type signature then we can use the lenses dbConfig, dbConn, and schema to get the entire config, connection string, and schema, from something of type r.

In contrast, the constraint AsNetworkError r means that we can use the prisms \_NetworkError, \_Timeout, and _ServerOnFire on a value of type r to get at the network error details.

<pre>*Classy> :i AsNetworkError
class AsNetworkError r_a759 where
  _NetworkError ::
    (Choice p, Control.Applicative.Applicative f) =>
    p NetworkError (f NetworkError) -> p r0 (f r0)

  _Timeout ::
    (Choice p, Control.Applicative.Applicative f) =>
    p Int (f Int) -> p r0 (f r0)

  _ServerOnFire ::
    (Choice p, Control.Applicative.Applicative f) =>
    p () (f ()) -> p r0 (f r0)
    -- Defined at Classy.lhs:63:3

instance AsNetworkError NetworkError -- Defined at Classy.lhs:63:3
</pre>

## Using the class constraints 

The first function is loadFromDb which uses a reader environment for database configuration, can throw a database error, and do IO actions. 

<pre>> loadFromDb :: ( MonadError e m,
>                 MonadReader r m,
>                 AsDbError e,
>                 HasDbConfig r,
>                 MonadIO m) => m Text
> loadFromDb = do
>
>   -- Due to "MonadReader r m" and "HasDbConfig r"
>   -- we can ask for the database config:
>   rdr    let dbconf  = rdr ^. dbConfig :: DbConfig
>
>   -- We can ask for the connection string directly:
>   let connstr  = rdr ^. dbConn :: DbConnection
>
>   -- We have "AsDbError e", so we can throw a DB error:
>   throwError $ (_InvalidConnection #) ()
>   throwError $ (_QueryError #) "Bad SQL!"
>
>   return "foo"
</pre>

Another function, sendOverNet uses a reader environment with a network config, throws network errors, and does IO actions. 

<pre>> sendOverNet :: ( MonadError e m,
>                  MonadReader r m,
>                  AsNetworkError e,
>                  AsAppError e,
>                  HasNetworkConfig r,
>                  MonadIO m) => Text -> m ()
> sendOverNet mydata = do
>
>   -- We have "MonadReader r m" and "HasNetworkConfig r"
>   -- so we can ask about the network config:
>   rdr    let netconf = rdr ^. networkConfig  :: NetworkConfig
>       p       = rdr ^. port           :: Port
>       s       = rdr ^. ssl            :: Ssl
>
>   liftIO $ putStrLn $ "Pretending to connect to the network..."
>
>   -- We have "AsNetworkError e" so we can throw a network error:
>   throwError $ (_NetworkError #) (Timeout 100)
>
>   -- We have "AsAppError e" so we can throw an application-level error:
>   throwError $ (_AppNetError #) (Timeout 100)
>
>   return ()
</pre>

If we load from the database and also send over the network then we get extra class constraints: 

<pre>> loadAndSend :: ( AsAppError e,
>                  AsNetworkError e,
>                  AsDbError e,
>                  HasNetworkConfig r,
>                  HasDbConfig r,
>                  MonadReader r m,
>                  MonadError e m,
>                  MonadIO m) => m ()
> loadAndSend = do
>   liftIO $ putStrLn "Loading from the database..."
>   t 
>   liftIO $ putStrLn "Sending to the network..."
>   sendOverNet t
</pre>

## Things that won’t compile 

We can’t throw the database error InvalidConnection without the right class constraint: 

<pre>> nope1 :: (MonadError e m, AsNetworkError e) => m ()
> nope1 = throwError $ (_InvalidConnection #) ()
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

<pre>> nope2 :: (MonadError e m, AsNetworkError e) => m ()
> nope2 = throwError $ (_AppNetError #) (Timeout 100)
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
  
  <pre>> nope3 :: (MonadReader r m, HasDbConfig r) => m ()
> nope3 = do
>   rdr    let netconf = rdr ^. networkConfig
>
>   return ()
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
*Classy> :t review _InvalidConnection ()
review _InvalidConnection () :: AsDbError e => e

*Classy> :t throwError $ review _InvalidConnection ()
throwError $ review _InvalidConnection () :: (AsDbError e, MonadError e m) => m a
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