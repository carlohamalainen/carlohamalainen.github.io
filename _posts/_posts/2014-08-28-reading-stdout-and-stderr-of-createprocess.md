---
id: 715
title: Reading stdout and stderr of createProcess
date: 2014-08-28T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/08/28/reading-stdout-and-stderr-of-createprocess/
permalink: /2014/08/28/reading-stdout-and-stderr-of-createprocess/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
For some time I&#8217;ve used this little utility to run a command with parameters and return stdout if the command exited successfully, or stdout and stderr if there was an error: 

<pre>readRestOfHandle :: Handle -&gt; IO String
readRestOfHandle h = do
    ineof  [String] -&gt; IO (Either String String)
runShellCommand cmd args = do
    (Just _, Just hout, Just herr, h) &lt;- createProcess (proc cmd args){ std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }

    stdOut &lt;- readRestOfHandle hout
    stdErr &lt;- readRestOfHandle herr

    exitCode  return $ Right stdOut
                     _           -&gt; return $ Left $ stdOut ++ "nnn" ++ stdErr
</pre>

Unfortunately this code is prone to deadlocking &#8211; I had this happen on a call to [dcm2mnc](http://www.bic.mni.mcgill.ca/~mferre/fmri/dcm2mnc_help.html). It ran dcm2mnc and then hung. 

The fix was to use [process-streaming](http://hackage.haskell.org/package/process-streaming) which provides a wrapper for createProcess:

<pre>simpleSafeExecute :: PipingPolicy String a -&gt; CreateProcess -&gt; IO (Either String a)
</pre>

And it&#8217;s straightforward to use it to get the stdout and stderr separately: 

<pre>x &lt;- simpleSafeExecute (pipeoe $ separated (surely B.toLazyM) (surely B.toLazyM)) (proc cmd args)
</pre>

Here&#8217;s my commit where I switched to process-streaming: <https://github.com/carlohamalainen/mytardis-rest/commit/7a97d2482abc7e5726ed003a480fc9a27ead7403>. 

A discussion on haskell-cafe where I found out about process-streaming: <https://groups.google.com/d/msg/haskell-cafe/I1ROxgw7DIs/taKHeJJHiVkJ>.