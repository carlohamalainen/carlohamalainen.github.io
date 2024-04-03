---
author: Carlo Hamalainen

date: "2014-08-28T00:00:00Z"
format: image
title: Reading stdout and stderr of createProcess
url: /2014/08/28/reading-stdout-and-stderr-of-createprocess/
---

**2024-03-13**: the github repository for this no longer exists and the code sample below is missing some lines. 😞

---

For some time I've used this little utility to run a command with parameters and return stdout if the command exited successfully, or stdout and stderr if there was an error:

```haskell
readRestOfHandle :: Handle -> IO String
readRestOfHandle h = do
    ineof  [String] -> IO (Either String String)
runShellCommand cmd args = do
    (Just _, Just hout, Just herr, h) <- createProcess (proc cmd args){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdOut <- readRestOfHandle hout
    stdErr <- readRestOfHandle herr

    exitCode  return $ Right stdOut
                     _           -> return $ Left $ stdOut ++ "nnn" ++ stdErr
```

Unfortunately this code is prone to deadlocking -- I had this happen on a call to [dcm2mnc](http://www.bic.mni.mcgill.ca/~mferre/fmri/dcm2mnc_help.html). It ran dcm2mnc and then hung.

The fix was to use [process-streaming](http://hackage.haskell.org/package/process-streaming) which provides a wrapper for createProcess:

```
simpleSafeExecute :: PipingPolicy String a -> CreateProcess -> IO (Either String a)
```

And it's straightforward to use it to get the stdout and stderr separately:

```
x <- simpleSafeExecute (pipeoe $ separated (surely B.toLazyM) (surely B.toLazyM)) (proc cmd args)
```

Here's my commit where I switched to process-streaming: <https://github.com/carlohamalainen/mytardis-rest/commit/7a97d2482abc7e5726ed003a480fc9a27ead7403>.

A discussion on haskell-cafe where I found out about process-streaming: <https://groups.google.com/d/msg/haskell-cafe/I1ROxgw7DIs/taKHeJJHiVkJ>.
