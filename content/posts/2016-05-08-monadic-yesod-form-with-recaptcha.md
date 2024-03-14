---
author: Carlo Hamalainen

date: "2016-05-08T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2016/05/08/monadic-yesod-form-with-recaptcha/
id: 784
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: Monadic Yesod form with ReCAPTCHA
url: /2016/05/08/monadic-yesod-form-with-recaptcha/
---
Applicative forms in [Yesod](http://www.yesodweb.com/book/forms) are nifty but they don't let you customise layout, CSS, and so on. I had this form for comments on [my blog](https://github.com/carlohamalainen/cli-yesod-blog):

```haskell
commentFormOLD :: EntryId -> Form Comment
commentFormOLD entryId = renderDivs $ Comment
     pure entryId
     lift (liftIO getCurrentTime)
     areq textField (fieldSettingsLabel MsgCommentName) Nothing
     aopt emailField (fieldSettingsLabel MsgCommentEmail) Nothing
     aopt urlField (fieldSettingsLabel MsgCommentUrl) Nothing
     areq htmlField (fieldSettingsLabel MsgCommentText) Nothing
     pure False <* recaptchaAForm
```

I wanted to tweak the layout so I had to convert it to a monadic form. The only quirk was that the second part of the return value of ``recaptchaMForm`` is of type ``[FieldView site]``, not ``FieldView site``. It looks like the first element of the list does the job for rendering the [Yesod-ReCAPTCHA](http://hackage.haskell.org/package/yesod-recaptcha-1.4/docs/Yesod-ReCAPTCHA.html) widget. So we set

```haskell
let recapView0 = recapView DL.!! 0
```

and then write

```haskell
^{fvInput recapView0}
```

in the whamlet.

Here's the full function. Note the ``fvId`` bits where we can specify the width and height. Also, to the form as a parameter to ``generateFormPost``, we must have a parameter of type ``Html``. So we put the ``EntryId`` at the front so that we can use [Currying](https://en.wikipedia.org/wiki/Currying).

```haskell
commentForm :: EntryId -> Html -> MForm Handler (FormResult Comment, Widget)
commentForm entryId extra = do
    (nameRes, nameView)     <- mreq textField  (fieldSettingsLabel MsgCommentName)  Nothing
    (emailRes, emailView)   <- mopt emailField (fieldSettingsLabel MsgCommentEmail) Nothing
    (urlRes, urlView)       <- mopt urlField   (fieldSettingsLabel MsgCommentUrl)   Nothing
    (textRes, textView)     <- mreq htmlField  (fieldSettingsLabel MsgCommentText)  Nothing

    (recapRes, recapView)   <- recaptchaMForm

    let recapView0 = recapView DL.!! 0

    now <- liftIO getCurrentTime

    let c = Comment
              <$> pure entryId
              <*> pure now
              <*> nameRes
              <*> emailRes
              <*> urlRes
              <*> textRes
              <*> pure False <* recapRes

    let widget = do
            toWidget
                [lucius|
                    ##{fvId nameView} {
                        width: 70ch;
                    }
                    ##{fvId emailView} {
                        width: 70ch;
                    }
                    ##{fvId urlView} {
                        width: 70ch;
                    }
                    ##{fvId textView} {
                        width: 120ch;
                        height: 120ch;
                    }
                |]
            [whamlet|
                #{extra}
                <p>
                    Name                 #
                <p>
                    ^{fvInput nameView}
                <p>
                    \ Email (not shown)  #
                <p>
                    ^{fvInput emailView}
                <p>
                    \ URL (optional)     #
                <p>
                    ^{fvInput urlView}
                <p>
                    \ Comment            #
                <p>
                    ^{fvInput textView}
                <p>
                    ^{fvInput recapView0}
            |]

    return (c, widget)
```

The diff starting from line 71 shows the change in Handler/Home.hs:

[ce76215f72cbcf748bef89bfa3b09a077ceb9ab9#diff-1d7a37a0e3408faaa1abf31093eb8a50L71](https://github.com/carlohamalainen/cli-yesod-blog/commit/ce76215f72cbcf748bef89bfa3b09a077ceb9ab9#diff-1d7a37a0e3408faaa1abf31093eb8a50L71).
