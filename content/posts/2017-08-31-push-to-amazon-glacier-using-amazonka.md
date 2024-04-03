---
author: Carlo Hamalainen

date: "2017-08-31T02:10:51Z"
guid: http://carlo-hamalainen.net/?p=994
title: Push to Amazon Glacier using amazonka
url: /2017/08/31/push-to-amazon-glacier-using-amazonka/
---
Here is a small Haskell package for pushing files to Amazon Glacier: <https://github.com/carlohamalainen/glacier-push>. It uses [Brendan Hay's amazonka API](https://github.com/brendanhay/amazonka), in particular [amazonka-glacier](https://hackage.haskell.org/package/amazonka-glacier-1.4.5).

One thing that I couldn't find in amazonka was a way to calculate the tree hash of a file. The Glacier API needs this for each part that is uploaded as well as the whole file. Amazon explains how to calculate the tree hash in their [Glacier docs](http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html) and provides sample code in Java and C++. Since the algorithm is recursive, it is quite short in Haskell:

```haskell
oneMb :: Int64
oneMb = 1024*1024

treeHash :: BS.ByteString -> Hash
treeHash s = treeHash' $ map sha256 $ oneMbChunks s
  where
    treeHash' []  = error "Internal error in treeHash'."
    treeHash' [x] = B16.encode x
    treeHash' xs  = treeHash' $ next xs

    next :: [BS.ByteString] -> [BS.ByteString]
    next []       = []
    next [a]      = [a]
    next (a:b:xs) = sha256 (BS.append a b) : next xs

    oneMbChunks :: BS.ByteString -> [BS.ByteString]
    oneMbChunks x
      | BS.length x <= oneMb = [x]
      | otherwise            = BS.take oneMb x : oneMbChunks (BS.drop oneMb x)

    sha256 :: BS.ByteString -> BS.ByteString
    sha256 = cs . SHA256.hashlazy
```

To push a large file to Glacier we do three things: initiate the multipart upload, upload each part (say, 100Mb chunks), and then finalize the upload.

## Initiate the multipart upload

We do this to get an ``uploadId`` which is then used for each of the multipart uploads. We use [initiateMultipartUpload](https://hackage.haskell.org/package/amazonka-glacier-1.4.5/docs/Network-AWS-Glacier-InitiateMultipartUpload.html#v:initiateMultipartUpload), and need to set the part size.

```haskell
    initiateMulti env vault _partSize = send' env mpu
      where
        mpu = initiateMultipartUpload accountId vault
                & imuPartSize .~ (Just $ cs $ show _partSize)
```

## Upload the parts

With the response from initiating the multipart upload (the ``mu`` parameter in ``uploadOnePart``) we can push a single part using [uploadMultipartPart](https://hackage.haskell.org/package/amazonka-glacier-1.4.5/docs/Network-AWS-Glacier-UploadMultipartPart.html#v:uploadMultipartPart). Here we have to also set the checksum and content range:

```haskell
    uploadOnePart env vault mu p = do
        let Part{..} = p

        body <- toHashed <$> getPart _path (_partStart, _partEnd)

        uploadId <- case mu ^. imursUploadId of
                        Nothing     -> throw MissingUploadID
                        Just uid    -> return uid

        let ump = uploadMultipartPart accountId vault uploadId body
                    & umpChecksum .~ (Just $ cs $ p ^. partHash)
                    & umpRange    .~ Just cr

        send' env ump

      where

        contentRange :: Int64 -> Int64 -> Text
        contentRange x y = "bytes " `append` cs (show x) `append` accountId `append` cs (show y) `append` "/*"
```

## Complete the multipart upload

Completing the multipart upload lets Glacier know that it should start its job of assembling all the parts into a full archive. We have to set the archive size and the tree hash of the entire file.

```haskell
    completeMulti env vault mp mu = do
        uploadId <- case mu ^. imursUploadId of
                        Nothing     -> throw MissingUploadID
                        Just uid    -> return uid

        let cmu = completeMultipartUpload accountId vault uploadId
                    & cmuArchiveSize .~ (Just $ cs $ show $ mp ^. multipartArchiveSize)
                    & cmuChecksum    .~ (Just $ cs $ mp ^. multipartFullHash)

        send' env cmu
```

## Notes

In each of these functions I used a helper for sending the request:

```haskell
    send' env x = liftIO $ runResourceT . runAWST env $ send x
```

I run the main block of work in a ``KatipContextT`` since I am using [katip](https://hackage.haskell.org/package/katip) for structured logging. Adding new key-value info to the log context is accomplished using [katipAddContext](https://hackage.haskell.org/package/katip-0.5.0.0/docs/Katip.html#v:katipAddContext).

```haskell
    go vault' path = do
        $(logTM) InfoS "Startup."

        let vault = cs vault'

        let myPartSize = 128*oneMb

        mp  <- liftIO $ mkMultiPart path myPartSize

        env <- liftIO $ newEnv'
        mu  <- liftIO $ initiateMulti env vault myPartSize

        let uploadId = fromMaybe (error "No UploadId in response, can't continue multipart upload.")
                     $ mu ^. imursUploadId

        partResponses <- forM (mp ^. multiParts) $ \p ->
            katipAddContext (sl "uploadId" uploadId) $
            katipAddContext (sl "location" $ fromMaybe "(nothing)" $ mu ^. imursLocation) $ do
                doWithRetries 10 (uploadOnePart env vault mu p)

        case lefts partResponses of
            []   -> do $(logTM) InfoS "All parts uploaded successfully, now completing the multipart upload."
                       catch (do completeResponse <- completeMulti env vault mp mu
                                 katipAddContext (sl "uploadId" uploadId)                             $
                                  katipAddContext (sl "archiveId" $ completeResponse ^. acoArchiveId) $
                                   katipAddContext (sl "checksum" $ completeResponse ^. acoChecksum ) $
                                    katipAddContext (sl "location" $ completeResponse ^. acoLocation) $ do
                                      $(logTM) InfoS "Done"
                                      liftIO exitSuccess)
                             (\e -> do logServiceError "Failed to complete multipart upload." e
                                       $(logTM) ErrorS "Failed."
                                       liftIO exitFailure)

            errs -> do forM_ errs (logServiceError "Failed part upload.")
                       $(logTM) ErrorS "Too many part errors."
                       liftIO exitFailure

    logServiceError msg (ServiceError e)
        = let smsg :: Text
              smsg = toText $ fromMaybe "" $ e ^. serviceMessage

              scode :: Text
              scode = toText $ e ^. serviceCode

            in katipAddContext (sl "serviceMessage" smsg) $
                katipAddContext (sl "serviceCode" scode)  $
                 (headersAsContext $ e ^. serviceHeaders) $
                   $(logTM) ErrorS msg

    logServiceError msg (TransportError e)
        = let txt :: Text
              txt = toText $ show e
            in katipAddContext (sl "TransportError" txt) $
                $(logTM) ErrorS msg

    logServiceError msg (SerializeError e)
        = let txt :: Text
              txt = toText $ show e
            in katipAddContext (sl "SerializeError" txt) $
                $(logTM) ErrorS msg
```

I found it handy to write this little helper function to turn each header from an amazonka ``ServiceError`` into a Katip context key/value pair:

```haskell
    headersAsContext :: KatipContext m => [Header] -> m a -> m a
    headersAsContext hs = foldl (.) id $ map headerToContext hs
      where
        headerToContext :: KatipContext m => Header -> m a -> m a
        headerToContext (h, x) = let h' = cs $ CI.original h :: Text
                                     x' = cs x               :: Text
                                   in katipAddContext (sl h' x')
```

Katip can write to ElasticSearch using [katip-elasticsearch](https://hackage.haskell.org/package/katip-elasticsearch). Then you'd be able to search for errors on specific header fields, etc.

## Sample run

```shell-session
$ glacier-push-exe basement myfile
[2017-08-30 12:44:47][glacier-push.main][Info][x4][1724][ThreadId 7][main:Main app/Main.hs:300:7] Startup.
[2017-08-30 12:44:50][glacier-push.main][Info][x4][1724][ThreadId 7][location:/998720554704/vaults/basement/multipart-uploads/vZMCGNsLGhfTJ_hJ-CJ_OF_juCAY1IaZDl_A3YqOZXnuQRH_AtPiMaUE-K1JDew-ZwiIuDZgR3QbjsJIEfWtGeMNeKDs][partEnd:134217727][partStart:0][uploadId:vZMCGNsLGhfTJ_hJ-CJ_OF_juCAY1IaZDl_A3YqOZXnuQRH_AtPiMaUE-K1JDew-ZwiIuDZgR3QbjsJIEfWtGeMNeKDs][main:Main app/Main.hs:213:15] Uploading part.
[2017-08-30 12:45:45][glacier-push.main][Info][x4][1724][ThreadId 7][location:/998720554704/vaults/basement/multipart-uploads/vZMCGNsLGhfTJ_hJ-CJ_OF_juCAY1IaZDl_A3YqOZXnuQRH_AtPiMaUE-K1JDew-ZwiIuDZgR3QbjsJIEfWtGeMNeKDs][partEnd:268435455][partStart:134217728][uploadId:vZMCGNsLGhfTJ_hJ-CJ_OF_juCAY1IaZDl_A3YqOZXnuQRH_AtPiMaUE-K1JDew-ZwiIuDZgR3QbjsJIEfWtGeMNeKDs][main:Main app/Main.hs:213:15] Uploading part.
[2017-08-30 12:46:37][glacier-push.main][Info][x4][1724][ThreadId 7][location:/998720554704/vaults/basement/multipart-uploads/vZMCGNsLGhfTJ_hJ-CJ_OF_juCAY1IaZDl_A3YqOZXnuQRH_AtPiMaUE-K1JDew-ZwiIuDZgR3QbjsJIEfWtGeMNeKDs][partEnd:293601279][partStart:268435456][uploadId:vZMCGNsLGhfTJ_hJ-CJ_OF_juCAY1IaZDl_A3YqOZXnuQRH_AtPiMaUE-K1JDew-ZwiIuDZgR3QbjsJIEfWtGeMNeKDs][main:Main app/Main.hs:213:15] Uploading part.
[2017-08-30 12:46:55][glacier-push.main][Info][x4][1724][ThreadId 7][main:Main app/Main.hs:320:22] All parts uploaded successfully, now completing the multipart upload.
[2017-08-30 12:46:57][glacier-push.main][Info][x4][1724][ThreadId 7][archiveId:bImG6jM0eQGNC7kIJTsC_wtcAXdPDUtJ-NyfstrkxeyTtXC_iEgkvenH-h_eQH-LYbhVKWJM7WuZlb7OHKtgKJNEpOtVaqxEhlNRTHphUtLCurcHAQDHKkiTnIXTpFxgPgvP9Q0axA][checksum:4f08645d8f3705dc222eef7547591c400362806abb7a6298b9267ebf2be7d901][location:/998720554704/vaults/basement/archives/bImG6jM0eQGNC7kIJTsC_wtcAXdPDUtJ-NyfstrkxeyTtXC_iEgkvenH-h_eQH-LYbhVKWJM7WuZlb7OHKtgKJNEpOtVaqxEhlNRTHphUtLCurcHAQDHKkiTnIXTpFxgPgvP9Q0axA][uploadId:vZMCGNsLGhfTJ_hJ-CJ_OF_juCAY1IaZDl_A3YqOZXnuQRH_AtPiMaUE-K1JDew-ZwiIuDZgR3QbjsJIEfWtGeMNeKDs][main:Main app/Main.hs:326:37] Done
```

The lines are pretty long (as they are intended for consumption into ElasticSearch, not human parsing) so here is one with line breaks:

```
[2017-08-30 12:45:45]
 [glacier-push.main]
 [Info]
 [x4]
 [1724]
 [ThreadId 7]
 [location:/998720554704/vaults/basement/multipart-uploads/vZMCGNsLGhfTJ_hJ-CJ_OF_juCAY1IaZDl_A3YqOZXnuQRH_AtPiMaUE-K1JDew-ZwiIuDZgR3QbjsJIEfWtGeMNeKDs]
 [partEnd:268435455]
 [partStart:134217728]
 [uploadId:vZMCGNsLGhfTJ_hJ-CJ_OF_juCAY1IaZDl_A3YqOZXnuQRH_AtPiMaUE-K1JDew-ZwiIuDZgR3QbjsJIEfWtGeMNeKDs]
 [main:Main app/Main.hs:213:15] Uploading part.
```


## Checking out and compiling

Use [Stack](https://docs.haskellstack.org/en/stable/README). Then:

```
$ git clone https://github.com/carlohamalainen/glacier-push.git
$ cd glacier-push
$ stack build
```


To browse the source on github, have a look at:

  * [glacier-push/blob/master/app/Main.hs](https://github.com/carlohamalainen/glacier-push/blob/master/app/Main.hs)
  * [glacier-push/blob/master/src/Push.hs](https://github.com/carlohamalainen/glacier-push/blob/master/src/Push.hs)
