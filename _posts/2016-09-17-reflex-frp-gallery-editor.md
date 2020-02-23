---
id: 748
title: Reflex FRP gallery editor
date: 2016-09-17T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2016/09/17/reflex-frp-gallery-editor/
permalink: /2016/09/17/reflex-frp-gallery-editor/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
When I post a series of photos to a personal blog I find myself editing HTML in Vim and switching back and forth to a browser to see if I have written comments in the right places and ordered the photos correctly. I could use a HTML editor to do this, but why not try FRP with Haskell? 🙂 Apparently I sort of use FRP [at work](https://www.reddit.com/r/haskell/comments/41icy5/haskell_developer_roles_at_standard_chartered/cz37u9f) so trying out [Reflex](https://github.com/reflex-frp/reflex-platform) wasn’t too much of a leap.

This post is adapted from the [todo list](https://github.com/reflex-frp/reflex-examples/tree/master/BasicTodo) and [drag ‘n’ drop](https://github.com/reflex-frp/reflex-examples/tree/master/drag-and-drop) examples in [this Reflex repo](https://github.com/reflex-frp/reflex-examples). 

Let’s start with the types. My basic blob of data is an image (a URL), a comment about the image, and a flag to indicate if the image should appear in the final output: 

<pre>&gt; data Image = Image
&gt;     { imageFileName :: T.Text   -- ^ e.g. "file:///tmp/cat.jpg"
&gt;     , imageVisible  :: Bool     -- ^ Output in HTML render?
&gt;     , imageRemark   :: T.Text   -- ^ Comment that goes before the image.
&gt;     }
&gt;     deriving (Eq, Ord, Show)
</pre>

The images have to be rendered in a particular order, so we’ll use a Map 

<pre>&gt; Map Int a
</pre>

where the integer keys provide the ordering and a is some type. 

To toggle visibility in the final rendering, we flip imageVisible: 

<pre>&gt; toggleVisibility :: Int -&gt; Map Int Image -&gt; Map Int Image
&gt; toggleVisibility k m = M.adjust f k m
&gt;   where
&gt;     f (Image x b c) = Image x (not b) c
</pre>

We can set the description for an image: 

<pre>&gt; setDesc :: (Int, T.Text) -&gt; Map Int Image -&gt; Map Int Image
&gt; setDesc (k, c) m = M.adjust f k m
&gt;   where
&gt;     f (Image x b _) = Image x b c
</pre>

We can move the kth image up: 

<pre>&gt; moveUp :: Int -&gt; Map Int Image -&gt; Map Int Image
&gt; moveUp 0 m = m
&gt; moveUp k m
&gt;   = let xs = M.elems m in
&gt;     M.fromList $ zip [0..] $ take (k-1) xs ++ [xs !! k, xs !! (k-1)] ++ drop (k+1) xs
&gt;     -- ^^^ Assumes contiguous keys!
</pre>

and down: 

<pre>&gt; moveDown :: Int -&gt; Map Int Image -&gt; Map Int Image
&gt; moveDown k m
&gt;   | k == fst (M.findMax m) = m
&gt;   | otherwise = let xs = M.elems m in
&gt;       M.fromList $ zip [0..] $ take k xs ++ [xs !! (k+1), xs !! k] ++ drop (k+2) xs
</pre>

It’s not efficient to completely rebuild the map by converting it to a list and back again, but this’ll do for now. 

In terms of the user interface there are a few events to consider: 

  * user toggles visibility of the kth image; 
  * user moves the kth image up; 
  * user moves the kth image down; and 
  * user changes the comment text for the kth image. 

We’ll put these four events into our own type. The first three are of type Event t Int where the Int is the key for the image in question. The last one has type Event t (Int, T.Text) since we need the key and the text that was entered into the textbox. In Reflex, the event type is [Event](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Class.html#t:Event).

<pre>&gt; data ImageEvent t = ImageEvent
&gt;     { evToggle  :: Event t Int
&gt;     , evUp      :: Event t Int
&gt;     , evDown    :: Event t Int
&gt;     , evKey     :: Event t (Int, T.Text)
&gt;     }
</pre>

Next, imageW creates an unnumbered list of images, consisting of a text field indicating if the image will be visible; a text box for writing a comment; buttons to toggle visibility and move the image up and down; and finally the image itself.

<pre>&gt; imageW
&gt;     :: forall m t. (MonadWidget t m)
&gt;     =&gt; Dynamic t (Map Int Image)
&gt;     -&gt; m (Dynamic t (Map Int (ImageEvent t)))
&gt; imageW xs = elClass "ul" "list" $ listWithKey xs $ k x -&gt; elClass "li" "element" $ do
&gt;     dynText $ fmap (T.pack . show . imageVisible) x
&gt;     el "br" $ return ()
&gt;
&gt;     let xEvent = imageRemark  uniqDyn x
&gt;
&gt;     ti 
&gt;     tEvent &lt;- updated  return (zipDynWith (,) (constDyn k) (_textInput_value ti))
&gt;
&gt;     el "br" $ return ()
&gt;
&gt;     (visibleEvent, moveUpEvent, moveDownEvent)                                                      visibleEvent  &lt;- (fmap $ const k)  button "visible"
&gt;                                                     moveUpEvent   &lt;- (fmap $ const k)  button "up"
&gt;                                                     moveDownEvent &lt;- (fmap $ const k)  button "down"
&gt;                                                     return (visibleEvent, moveUpEvent, moveDownEvent)
&gt;
&gt;     elClass "p" "the image" $ elDynAttr "img" (fmap f x) (return ())
&gt;
&gt;     return $ ImageEvent visibleEvent moveUpEvent moveDownEvent tEvent
&gt;
&gt;   where
&gt;
&gt;     f :: Image -&gt; Map T.Text T.Text
&gt;     f i = M.fromList
&gt;             [ ("src",   imageFileName i)
&gt;             , ("width", "500")
&gt;             ]
&gt;
&gt;     textBoxAttrs :: TextInputConfig t
&gt;     textBoxAttrs = def { _textInputConfig_attributes = constDyn $ M.fromList [("size", "100")] }
</pre>

To process the dynamic map we use [listWithKey](https://hackage.haskell.org/package/reflex-dom-0.3/docs/Reflex-Dom-Widget-Basic.html#v:listWithKey):

<pre>&gt; listWithKey
&gt;   :: forall t k v m a. (Ord k, MonadWidget t m)
&gt;   =&gt; Dynamic t (Map k v)
&gt;   -&gt; (k -&gt; Dynamic t v -&gt; m a)
&gt;   -&gt; m (Dynamic t (Map k a))
</pre>

Specialised to our usage, the type is: 

<pre>&gt; listWithKey
&gt;   :: forall t m. (MonadWidget t m)
&gt;   =&gt; Dynamic t (Map Int Image)
&gt;   -&gt; (Int -&gt; Dynamic t Image -&gt; m (ImageEvent t))
&gt;   -&gt; m (Dynamic t (Map Int (ImageEvent t)))
</pre>

It’s like mapping over the elements of the dynamic input: 

<pre>&gt; listWithKey xs $ k x -&gt; ...
</pre>

We use [elClass](https://hackage.haskell.org/package/reflex-dom-0.3/docs/Reflex-Dom-Widget-Basic.html#v:elClass) to produce the elements on the page. For example the text attribute showing if the image is visible or not can be rendered using [dynText](https://hackage.haskell.org/package/reflex-dom-0.3/docs/Reflex-Dom-Widget-Basic.html#v:dynText):

<pre>&gt;     dynText $ fmap (T.pack . show . imageVisible) x
</pre>

We have an fmap since x :: Dynamic t Image and [Dynamic](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Dynamic.html#t:Dynamic) has a Functor instance.

The image list and all the events are wrapped up in imageListW. Here’s the main part: 

<pre>&gt; imageListW
&gt;     :: forall t m. MonadWidget t m
&gt;     =&gt; Dynamic t T.Text
&gt;     -&gt; m ()
&gt; imageListW dynDrop = do
&gt;     let eventDrop = fmap const $ updated $ fmap parseDrop dynDrop :: Event t (MM Image -&gt; MM Image)
&gt;
&gt;     rec xs              [ eventDrop
&gt;             , switch . current $ toggles
&gt;             , switch . current $ ups
&gt;             , switch . current $ downs
&gt;             , switch . current $ keys
&gt;             ]
&gt;
&gt;         bs 
&gt;         let toggles :: Dynamic t (Event t (M.Map Int Image -&gt; M.Map Int Image))
&gt;             ups     :: Dynamic t (Event t (M.Map Int Image -&gt; M.Map Int Image))
&gt;             downs   :: Dynamic t (Event t (M.Map Int Image -&gt; M.Map Int Image))
&gt;             keys    :: Dynamic t (Event t (M.Map Int Image -&gt; M.Map Int Image))
&gt;
&gt;             toggles = (mergeWith (.) . map (fmap $ toggleVisibility) . map evToggle . M.elems)  bs
&gt;             ups     = (mergeWith (.) . map (fmap $ moveUp)           . map evUp     . M.elems)  bs
&gt;             downs   = (mergeWith (.) . map (fmap $ moveDown)         . map evDown   . M.elems)  bs
&gt;             keys    = (mergeWith (.) . map (fmap $ setDesc)          . map evKey    . M.elems)  bs
&gt;
&gt;     ta                          { _textAreaConfig_setValue   = (T.concat . map rawHTML . M.elems)  updated xs
&gt;                         , _textAreaConfig_attributes = taAttrs
&gt;                         }
&gt;
&gt;     return ()
</pre>

Notice that toggles is used before it is defined! This is made possible by using the [recursive do](https://wiki.haskell.org/MonadFix) extension which provides the ability to do _value_ recursion.

The key bit is the use of [mergeWith](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Class.html#v:mergeWith) that combines all of the events. 

<pre>&gt; mergeWith :: Reflex t =&gt; (a -&gt; a -&gt; a) -&gt; [Event t a] -&gt; Event t a
</pre>

Here, mergeWith (.) will left-fold simultaneous events.

<pre>&gt;     rec xs              [ eventDrop
&gt;             , switch . current $ toggles
&gt;             , switch . current $ ups
&gt;             , switch . current $ downs
&gt;             , switch . current $ keys
&gt;             ]
</pre>

The toggles has type 

<pre>&gt; Dynamic t (Event t (M.Map Int Image -&gt; M.Map Int Image))
</pre>

so we use [switch](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Class.html#v:switch) and [current](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Dynamic.html#v:current) to get to an Event type:

<pre>ghci&gt; :t switch
switch :: Reflex t =&gt; Behavior t (Event t a) -&gt; Event t a

ghci&gt; :t current
current :: Reflex t =&gt; Dynamic t a -&gt; Behavior t a

ghci&gt; :t switch . current
switch . current :: Reflex t =&gt; Dynamic t (Event t a) -&gt; Event t a
</pre>

This merge is also where we bring in the drag ‘n’ drop event via eventDrop which is how we get a list of images into the dynamic map.

**Try it out** 

To try it out without setting up Reflex, grab [gallery_editor.zip](https://github.com/carlohamalainen/playground/raw/master/haskell/reflex/gallery_editor.zip), unzip it, and open gallery_editor/gallery.jsexe/index.html in your browser. Drag some images onto the top area of the page using your file manager. Tested on Ubuntu 16.

Or, grab the source from [Github](https://github.com/carlohamalainen/playground/tree/master/haskell/reflex). 

<img src="https://i1.wp.com/raw.githubusercontent.com/carlohamalainen/playground/master/haskell/reflex/dropped01.png?w=600&#038;ssl=1"  data-recalc-dims="1" /> 

<img src="https://i1.wp.com/raw.githubusercontent.com/carlohamalainen/playground/master/haskell/reflex/dropped02.png?w=600&#038;ssl=1"  data-recalc-dims="1" />