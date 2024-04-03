---
author: Carlo Hamalainen

date: "2016-09-17T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2016/09/17/reflex-frp-gallery-editor/
title: Reflex FRP gallery editor
url: /2016/09/17/reflex-frp-gallery-editor/
---


When I post a series of photos to a personal blog I find myself editing HTML in Vim and switching back and forth to a browser to see if I have written comments in the right places and ordered the photos correctly. I could use a HTML editor to do this, but why not try FRP with Haskell? :) Apparently I sort of use FRP [at work](https://www.reddit.com/r/haskell/comments/41icy5/haskell_developer_roles_at_standard_chartered/cz37u9f) so trying out [Reflex](https://github.com/reflex-frp/reflex-platform) wasn't too much of a leap.

This post is adapted from the [todo list](https://github.com/reflex-frp/reflex-examples/tree/master/BasicTodo) and [drag 'n' drop](https://github.com/reflex-frp/reflex-examples/tree/master/drag-and-drop) examples in [this Reflex repo](https://github.com/reflex-frp/reflex-examples).

Let's start with the types. My basic blob of data is an image (a URL), a comment about the image, and
a flag to indicate if the image should appear in the final output:

```haskell
data Image = Image
    { imageFileName :: T.Text   -- ^ e.g. "file:///tmp/cat.jpg"
    , imageVisible  :: Bool     -- ^ Output in HTML render?
    , imageRemark   :: T.Text   -- ^ Comment that goes before the image.
    }
    deriving (Eq, Ord, Show)
```

The images have to be rendered in a particular order, so we'll use a ``Map``

```haskell
Map Int a
```

where the integer keys provide the ordering and ``a`` is some type.

To toggle visibility in the final rendering, we flip ``imageVisible``:

```haskell
toggleVisibility :: Int -> Map Int Image -> Map Int Image
toggleVisibility k m = M.adjust f k m
  where
    f (Image x b c) = Image x (not b) c
```

We can set the description for an image:

```haskell
setDesc :: (Int, T.Text) -> Map Int Image -> Map Int Image
setDesc (k, c) m = M.adjust f k m
  where
    f (Image x b _) = Image x b c
```

We can move the ``k``th image up:

```haskell
moveUp :: Int -> Map Int Image -> Map Int Image
moveUp 0 m = m
moveUp k m
  = let xs = M.elems m in
    M.fromList $ zip [0..] $ take (k-1) xs ++ [xs !! k, xs !! (k-1)] ++ drop (k+1) xs
    -- ^^^ Assumes contiguous keys!
```

and down:

```haskell
moveDown :: Int -> Map Int Image -> Map Int Image
moveDown k m
  | k == fst (M.findMax m) = m
  | otherwise = let xs = M.elems m in
      M.fromList $ zip [0..] $ take k xs ++ [xs !! (k+1), xs !! k] ++ drop (k+2) xs
```

It's not efficient to completely rebuild the map by converting
it to a list and back again, but this'll do for now.

In terms of the user interface there are a few events to
consider:

* user toggles visibility of the ``k``th image;
* user moves the ``k``th image up;
* user moves the ``k``th image down; and
* user changes the comment text for the ``k``th image.

We'll put these four events into our own type. The first three are of
type ``Event t Int`` where the ``Int`` is the key for the
image in question. The last one has type ``Event t (Int, T.Text)``
since we need the key and the text that was entered into the textbox.
In Reflex, the event type is [Event](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Class.html#t:Event).

```haskell
data ImageEvent t = ImageEvent
    { evToggle  :: Event t Int
    , evUp      :: Event t Int
    , evDown    :: Event t Int
    , evKey     :: Event t (Int, T.Text)
    }
```

Next, ``imageW`` creates an unnumbered list of images, consisting of
a text field indicating if the image will be visible; a text box for writing a comment;
buttons to toggle visibility and move the image up and down; and finally the image itself.

```haskell
imageW
    :: forall m t. (MonadWidget t m)
    => Dynamic t (Map Int Image)
    -> m (Dynamic t (Map Int (ImageEvent t)))
imageW xs = elClass "ul" "list" $ listWithKey xs $ \k x -> elClass "li" "element" $ do
    dynText $ fmap (T.pack . show . imageVisible) x
    el "br" $ return ()

    let xEvent = imageRemark <$> uniqDyn x

    ti <- textInput $ textBoxAttrs & setValue .~ (updated xEvent)

    tEvent <- updated <$> return (zipDynWith (,) (constDyn k) (_textInput_value ti))

    el "br" $ return ()

    (visibleEvent, moveUpEvent, moveDownEvent) <- elClass "div" "my buttons" $ do
                                                    visibleEvent  <- (fmap $ const k) <$> button "visible"
                                                    moveUpEvent   <- (fmap $ const k) <$> button "up"
                                                    moveDownEvent <- (fmap $ const k) <$> button "down"
                                                    return (visibleEvent, moveUpEvent, moveDownEvent)

    elClass "p" "the image" $ elDynAttr "img" (fmap f x) (return ())

    return $ ImageEvent visibleEvent moveUpEvent moveDownEvent tEvent

  where

    f :: Image -> Map T.Text T.Text
    f i = M.fromList
            [ ("src",   imageFileName i)
            , ("width", "500")
            ]

    textBoxAttrs :: TextInputConfig t
    textBoxAttrs = def { _textInputConfig_attributes = constDyn $ M.fromList [("size", "100")] }
```

To process the dynamic map
we use [listWithKey](https://hackage.haskell.org/package/reflex-dom-0.3/docs/Reflex-Dom-Widget-Basic.html#v:listWithKey):

```haskell
listWithKey
  :: forall t k v m a. (Ord k, MonadWidget t m)
  => Dynamic t (Map k v)
  -> (k -> Dynamic t v -> m a)
  -> m (Dynamic t (Map k a))
```

Specialised to our usage, the type is:

```haskell
listWithKey
  :: forall t m. (MonadWidget t m)
  => Dynamic t (Map Int Image)
  -> (Int -> Dynamic t Image -> m (ImageEvent t))
  -> m (Dynamic t (Map Int (ImageEvent t)))
```

It's like mapping over the elements of the dynamic input:

```haskell
listWithKey xs $ \k x -> ...
```

We use [elClass](https://hackage.haskell.org/package/reflex-dom-0.3/docs/Reflex-Dom-Widget-Basic.html#v:elClass) to
produce the elements on the page. For example the text attribute showing if the image is visible or not can be rendered
using [dynText](https://hackage.haskell.org/package/reflex-dom-0.3/docs/Reflex-Dom-Widget-Basic.html#v:dynText):

```haskell
    dynText $ fmap (T.pack . show . imageVisible) x
```

We have an ``fmap`` since ``x :: Dynamic t Image`` and [Dynamic](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Dynamic.html#t:Dynamic)
has a ``Functor`` instance.

The image list and all the events are wrapped up in ``imageListW``. Here's the main part:

```haskell
imageListW
    :: forall t m. MonadWidget t m
    => Dynamic t T.Text
    -> m ()
imageListW dynDrop = do
    let eventDrop = fmap const $ updated $ fmap parseDrop dynDrop :: Event t (MM Image -> MM Image)

    rec xs <- foldDyn ($) emptyMap $ mergeWith (.)
            [ eventDrop
            , switch . current $ toggles
            , switch . current $ ups
            , switch . current $ downs
            , switch . current $ keys
            ]

        bs <- imageW xs

        let toggles :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
            ups     :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
            downs   :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
            keys    :: Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))

            toggles = (mergeWith (.) . map (fmap $ toggleVisibility) . map evToggle . M.elems) <$> bs
            ups     = (mergeWith (.) . map (fmap $ moveUp)           . map evUp     . M.elems) <$> bs
            downs   = (mergeWith (.) . map (fmap $ moveDown)         . map evDown   . M.elems) <$> bs
            keys    = (mergeWith (.) . map (fmap $ setDesc)          . map evKey    . M.elems) <$> bs

    ta <- textArea $ (def :: TextAreaConfig t)
                        { _textAreaConfig_setValue   = (T.concat . map rawHTML . M.elems) <$> updated xs
                        , _textAreaConfig_attributes = taAttrs
                        }

    return ()
```

Notice that ``toggles`` is used before it is defined! This is made possible by
using the [recursive do](https://wiki.haskell.org/MonadFix) extension which provides
the ability to do _value_ recursion.

The key bit is the use
of [mergeWith](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Class.html#v:mergeWith)
that combines all of the events.

```haskell
mergeWith :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a
```

Here, ``mergeWidth (.)`` will left-fold simultaneous events.

```haskell
    rec xs <- foldDyn ($) emptyMap $ mergeWith (.)
            [ eventDrop
            , switch . current $ toggles
            , switch . current $ ups
            , switch . current $ downs
            , switch . current $ keys
            ]
```

The ``toggles`` has type

```haskell
Dynamic t (Event t (M.Map Int Image -> M.Map Int Image))
```

so we use [switch](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Class.html#v:switch)
and [current](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Dynamic.html#v:current)
to get to an ``Event`` type:

```
ghci> :t switch
switch :: Reflex t => Behavior t (Event t a) -> Event t a

ghci> :t current
current :: Reflex t => Dynamic t a -> Behavior t a

ghci> :t switch . current
switch . current :: Reflex t => Dynamic t (Event t a) -> Event t a
```

This merge is also where we bring in the drag 'n' drop event via ``eventDrop`` which is how we get
a list of images into the dynamic map.

### Try it out

To try it out without setting up Reflex, grab [gallery_editor.zip](https://github.com/carlohamalainen/playground/raw/master/haskell/reflex/gallery_editor.zip), unzip it, and open ``gallery_editor/gallery.jsexe/index.html`` in your browser. Drag some images onto the top area of the page using your file manager. Tested on Ubuntu 16.

Or, grab the source from [Github](https://github.com/carlohamalainen/playground/tree/master/haskell/reflex).

{{< figure src="https://raw.githubusercontent.com/carlohamalainen/playground/master/haskell/reflex/dropped01.png" >}}
{{< figure src="https://raw.githubusercontent.com/carlohamalainen/playground/master/haskell/reflex/dropped02.png" >}}
