---
id: 685
title: Note on applicative forms in Yesod
date: 2013-07-24T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/07/24/note-on-applicative-forms-in-yesod/
permalink: /2013/07/24/note-on-applicative-forms-in-yesod/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
I'm writing a [blog system](https://github.com/carlohamalainen/cli-yesod-blog) using [Yesod](http://www.yesodweb.com/). I want to allow comments from users that are not logged in, but I don't want millions of spam messages, so I thought I'd add ReCAPTCHA verification to the comment form. Here is the basic form: 

<pre>commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
     pure entryId
     aformM (liftIO getCurrentTime)
     aformM requireAuthId
     areq textField (fieldSettingsLabel MsgCommentName) Nothing
     areq textareaField (fieldSettingsLabel MsgCommentText) Nothing
</pre>

To get a ReCAPTCHA field, we just append <* recaptchaAForm right at the end: 

<pre>commentForm :: EntryId -> Form Comment
commentForm entryId = renderDivs $ Comment
     pure entryId
     aformM (liftIO getCurrentTime)
     aformM requireAuthId
     areq textField (fieldSettingsLabel MsgCommentName) Nothing
     areq textareaField (fieldSettingsLabel MsgCommentText) Nothing <* recaptchaAForm
</pre>

So how does the <* thing work? How are the two forms combined so that the ReCAPTCHA functionality works, and the result of the first form is the actual output? 

For the sake of this blog post I'll use a simpler form and datatype. So let's define a Person type that just stores a name of type Text: 

<pre>data Person = Person { personName :: Text }
    deriving Show
</pre>

The form in Yesod just has a single required field (areq = Applicative REQuired) for the name: 

<pre>personForm :: Form Person
personForm = renderDivs $ Person  areq textField "Name" Nothing
</pre>

(If you aren't familiar with Yesod's applicative forms, have a look at this very informative [Stack Overflow](http://stackoverflow.com/questions/15869376/yesod-applicative-forms) question.) 

Using ghci, let's check the type of everything after renderDivs $: 

<pre>> :t Person  areq textField "Name" Nothing
Person  areq textField "Name" Nothing
  :: RenderMessage master FormMessage => AForm sub master Person
</pre>

All we are interested in is the final bit, namely AForm sub master Person. The sub and master refer to Yesod subsites, which we aren't using here. So basically we can say that 

<pre>Person  areq textField "Name" Nothing
</pre>

is of type AForm sub master Person, a form that returns a Person. 

If we tack a ReCAPTCHA form onto the end, the data type stays the same: 

<pre>> :t (Person  areq textField "Name" Nothing) <* recaptchaAForm
(Person  areq textField "Name" Nothing)  AForm sub master Person
</pre>

So how does this work? First, we look up <* using :info on the ghci prompt: 

<pre>> :info ( Applicative f where
  ...
  ( f b -> f a
        -- Defined in `Control.Applicative'
infixl 4 <*
</pre>

Looking at [Control.Applicative](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Applicative.html) we see that: 

<pre>-- | Sequence actions, discarding the value of the second argument.
( f b -> f a
( (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f  a  b
</pre>

Since Haskell respects equational reasoning, we can make substitutions as follows: 

<pre>(Person  areq textField "Name" Nothing)  liftA2 const (Person  areq textField "Name" Nothing) recaptchaAForm
    --> const  (Person  areq textField "Name" Nothing)  recaptchaAForm
</pre>

and we can do a basic check on our working by verifying that the type of the final expression is still an AForm sub master Person: 

<pre>> :t const  (Person  areq textField "Name" Nothing)  recaptchaAForm
const  (Person  areq textField "Name" Nothing)  recaptchaAForm
  :: YesodReCAPTCHA master => AForm sub master Person
</pre>

All good. Now, is just short for fmap, so we can make another substitution: 

<pre>--> const  (Person  areq textField "Name" Nothing)  recaptchaAForm
    --> fmap const (Person  areq textField "Name" Nothing)  recaptchaAForm
    --> (fmap const (Person  areq textField "Name" Nothing))  recaptchaAForm
</pre>

where the last line makes it clear that fmap takes two parameters, and this blob is the first argument to . 

Next we have to find the Applicative instance definition for an AForm, which is in [Yesod.Form.Types](http://hackage.haskell.org/packages/archive/yesod-form/1.2.1.3/doc/html/src/Yesod-Form-Types.html#AForm). It's a bit gnarly: 

<pre>newtype AForm sub master a = AForm
    { unAForm :: (master, [Text]) -> Maybe (Env, FileEnv) -> Ints -> GHandler sub master (FormResult a, [FieldView sub master] -> [FieldView sub master], Ints, Enctype)
    }
instance Functor (AForm sub master) where
    fmap f (AForm a) =
        AForm $ x y z -> liftM go $ a x y z
      where
        go (w, x, y, z) = (fmap f w, x, y, z)
instance Applicative (AForm sub master) where
    pure x = AForm $ const $ const $ ints -> return (FormSuccess x, mempty, ints, mempty)
    (AForm f)  (AForm g) = AForm $ mr env ints -> do
        (a, b, ints', c) <- f mr env ints
        (x, y, ints&#039;&#039;, z) <- g mr env ints&#039;
        return (a  x, b `mappend` y, ints'', c `mappend` z)
</pre>

The original question was how the result of the form composition is just a Person. Looking at the definition of AForm, we see the FormResult data type, and in the definition for (AForm f) (AForm g), the FormResult component is just a x where a and x are the FormResult components of the forms f and g, respectively. 

So what happens to the FormResult component when we use fmap const? We can get an idea by writing a small function that rips out the FormResult of an applicative form. By poking around in [Yesod.Form.Functions](http://hackage.haskell.org/packages/archive/yesod-form/1.2.1.3/doc/html/Yesod-Form-Functions.html) we find aFormToForm which does the trick: 

<pre>getFormResult f = do
    (result, _) <- aFormToForm f
    return result
</pre>

So let's try it out: 

<pre>*Handler.Home> :t getFormResult (Person  areq textField "Name" Nothing)
getFormResult (Person  areq textField "Name" Nothing)
  :: RenderMessage master FormMessage =>
     Control.Monad.Trans.RWS.Lazy.RWST
       (Maybe (Env, FileEnv), master, [Yesod.Form.Types.Lang])
       Enctype
       Ints
       (GHandler sub master)
       (FormResult Person)
</pre>

and with fmap const: 

<pre>*Handler.Home> :t getFormResult (fmap const ((Person  areq textField "Name" Nothing)))
getFormResult (fmap const ((Person  areq textField "Name" Nothing)))
  :: RenderMessage master FormMessage =>
     Control.Monad.Trans.RWS.Lazy.RWST
       (Maybe (Env, FileEnv), master, [Yesod.Form.Types.Lang])
       Enctype
       Ints
       (GHandler sub master)
       (FormResult (b -> Person))
</pre>

There's a lot there, but the only change was that (FormResult Person) became (FormResult (b -> Person)). This is what we expect from the definition of fmap and const. The FormResult part becomes a function that just returns itself. 

So we can finally move on to the applicative definition for a FormResult which is in [Yesod.Form.Types](http://hackage.haskell.org/packages/archive/yesod-form/1.2.1.3/doc/html/Yesod-Form-Types.html): 

<pre>1. instance Applicative FormResult where
2.     pure = FormSuccess
3.     (FormSuccess f)  (FormSuccess g) = FormSuccess $ f g
4.     (FormFailure x)  (FormFailure y) = FormFailure $ x ++ y
5.     (FormFailure x)  _ = FormFailure x
6.     _  (FormFailure y) = FormFailure y
7.     _  _ = FormMissing
</pre>

Line 3 is the case where the Person form was successful and the ReCAPTCHA form was also successful. In our case the left hand side might be

<pre>fmap const (FormSuccess (Person "bob"))
</pre>

and the right hand side, representing the ReCAPTCHA form, will be 

<pre>fmap const (FormSuccess ())
</pre>

since a Yesod.Recaptcha form [returns an empty result](http://hackage.haskell.org/packages/archive/yesod-recaptcha/0.1.2/doc/html/src/Yesod-ReCAPTCHA.html#recaptchaAForm). So at the end, this is what happens to the result of the form: 

<pre>> (fmap const (FormSuccess (Person "bob")))  (FormSuccess ())
FormSuccess (Person {personName = "bob"})
</pre>

We can reason our way to this result: 

<pre>> :t fmap const (FormSuccess (Person "bob"))
fmap const (FormSuccess (Person "bob")) :: FormResult (b -> Person)

*Handler.Home> :t FormSuccess ()
FormSuccess () :: FormResult ()

(fmap const (FormSuccess (Person "bob")))  (FormSuccess ())
    --> FormSuccess $ (fmap const (Person "bob")) () -- using line 3.
    --> FormSuccess $ (Person "bob")                 -- defn of Functor on FormResult
    --> FormSuccess (Person "bob")
</pre>

Note: I have been a bit hand-wavey with the use of getFormResult but the behaviour does follow from the Functor instance definition for AForm. Similar for the Functor instance definition for FormResult.