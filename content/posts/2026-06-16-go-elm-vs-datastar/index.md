---
date: 2026-06-16
title: Go+Elm vs Datastar/Go
url: /2026/06/16/go-elm-vs-datastar
---

Back in the old days, people used to leave comments on blogs. Imagine such a time. One time
a guy even sent me a bottle of his homemade wine in appreciation for a post I made about getting
[MinionPro fonts](https://carlo-hamalainen.net/2007/12/11/installing-minion-pro-fonts/) to work in LaTeX!

My own blog has migrated from Wordpress (self-hosted), to Haskell/Yesod (custom framework, self-hosted), and finally to Github static pages.

Wordpress handled comments, so that was easy. Some 10+ years ago when I moved to the custom
[Yesod blog](https://github.com/carlohamalainen/cli-yesod-blog) I wrote my own
[comment system](https://github.com/carlohamalainen/cli-yesod-blog/blob/master/blog/Handler/Home.hs#L29). It looks like it was fairly static,
I certainly didn't write any JavaScript to give the UI any [juice](https://www.youtube.com/watch?v=Fy0aCDmgnxg).

Eventually I got sick of self-hosting and switched to 
a [Hugo blog on GitHub pages](https://github.com/carlohamalainen/carlohamalainen.github.io). Despite blog comment traffic dropping nearly to zero, I decided to 
write a blog comment system using [Elm on the frontend and Go on the backend](https://github.com/carlohamalainen/carlo-comments). 

It was an interesting experience
writing Elm after having written View2 apps when I was a Strat at Standard Chartered. My erstwhile colleague Felipe Almeida Lessa talked about
View2 at Lambda Days 2024:

{{< youtube R51tulAtIoE >}}

I'm amazed that some of the detail escaped the event horizon that is bank compliance.

As he explained, View2 was essentially the Elm architecture:

{{< figure src="view2-01.png" link="view2-01.png" target="_blank" rel="noopener" alt="View2" width="80%" >}}

[Bubble Tea](https://github.com/charmbracelet/bubbletea) also copies the Elm architecture, and that's a fun way to 
make TUIs in Go. The only downside is that, being Go, one can do anything anywhere since there isn't a type system 
constraining where you can run IO actions and so on.

# Elm+Go blog comment system

My [Elm code](https://github.com/carlohamalainen/carlo-comments/blob/main/frontend/src/Main.elm) has a standard 
sort of `Model`, capturing all the runtime state:

```elm
type alias Model =
    { lc : State LoadedComments
    , currentUrl : Url.Url
    , author : String
    , authorEmail : String
    , commentBody : String
    , submitStatus : SubmitStatus
    , turnstileToken : Maybe String
    }
```

This captures front end state like whether the user has clicked submit, successfully clicked submit, had a failure, etc.
We are forced to model the complement as well, `NotSubmitted`, meaning the user is still editing:

```elm
type SubmitStatus
    = NotSubmitted
    | Submitting
    | SubmitSuccess
    | SubmitFailure String
```

The `LoadedComments` type was essentially a list of `Comment`:

```elm

type alias Comment t =
    { commentID : String
    , siteID : String
    , postID : String
    , timestamp : t
    , author : String
    , authorEmail : String
    , commentBody : String
    , isActive : Bool
    }
```

Elm is on the browser, so it has to communicate with the Go backend via some
kind of encoding, and the easiest thing that everyone reaches for is JSON:

```elm

encodeComment : Model -> Encode.Value
encodeComment model =
    Encode.object
        [ ( "siteID", Encode.string theSiteId )
        , ( "postID", Encode.string (trimTrailingSlashes model.currentUrl.path) )
        , ( "author", Encode.string model.author )
        , ( "authorEmail", Encode.string model.authorEmail )
        , ( "commentBody", Encode.string model.commentBody )
        , ( "turnstileToken", Encode.string (Maybe.withDefault "" model.turnstileToken) )
        ]
```

At the core of any Elm/View2 application is a message type; sometimes these
became nested and quite exciting to handle (profunctors, profunctors everywhere!). For the blog comment system we had:

```elm
type Msg
    = GotComments (Result Http.Error (List (Comment Int)))
    | UpdateAuthor String
    | UpdateAuthorEmail String
    | UpdateCommentBody String
    | SubmitForm
    | GotSubmitResponse (Result Http.Error ())
    | TurnstileToken String
```

Handling all of these cases is the job of the `update` function:

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAuthor newAuthor ->
            ( { model | author = newAuthor }, Cmd.none )

        UpdateAuthorEmail newEmail ->
            ( { model | authorEmail = newEmail }, Cmd.none )

        UpdateCommentBody newBody ->
            ( { model | commentBody = newBody }, Cmd.none )

        GotComments result ->
            case result of
                Ok data ->
                    let
                        parsedComments =
                            List.map
                                (\comment ->
                                    { commentID = comment.commentID
                                    , siteID = comment.siteID
                                    , postID = comment.postID
                                    , author = comment.author
                                    , authorEmail = comment.authorEmail
                                    , timestamp = Time.millisToPosix comment.timestamp
                                    , commentBody = comment.commentBody
                                    , isActive = comment.isActive
                                    }
                                )
                                data
                    in
                    ( { model | lc = Success { allComments = parsedComments, unmoderated = [] } }, Cmd.none )

                Err e ->
                    ( { model | lc = Failure (errorToString e) }, Cmd.none )

        SubmitForm ->
            ( { model | submitStatus = Submitting }
            , submitComment model
            )

        GotSubmitResponse result ->
            case result of
                Ok _ ->
                    ( { model | submitStatus = SubmitSuccess }, Cmd.none )

                Err error ->
                    ( { model | submitStatus = SubmitFailure (errorToString error) }, Cmd.none )

        TurnstileToken token ->
            ( { model | turnstileToken = Just token }, Cmd.none )
```

Phew!

On the Go side of things we have to deserialise the JSON, perform database updates, and let the Elm front end know what happened.
There's duplication here, because we've implemented a `Comment` type in Elm, then a JSON representation, and then an equivalent type for the backend:

```go
type Comment struct {
	CommentID     string    `json:"commentID"`
	SiteID        string    `json:"siteID"`
	PostID        string    `json:"postID"`
	Timestamp     Timestamp `json:"timestamp"`
	SourceAddress string    `json:"sourceAddress"`
	Author        string    `json:"author"`
	AuthorEmail   string    `json:"authorEmail"`
	CommentBody   string    `json:"commentBody"`
	IsActive      bool      `json:"isActive"`
}
```

But that's not quite right - we don't have a `PostID` when a user submits a comment, so we also need an intermediate type:

```go
type NewComment struct {
	SiteID         string `json:"siteID"`
	PostID         string `json:"postID"`
	Author         string `json:"author"`
	AuthorEmail    string `json:"authorEmail"`
	CommentBody    string `json:"commentBody"`
	TurnstileToken string `json:"turnstileToken"`
}
```

It just doesn't feel good implementing similar yet different types for the same
thing across the backend and front end, plus adding serialisation in between.

The Elm compiles to a 455K JavaScript file! 🤯

# Datastar

I rewrote the [Elm/Go](https://github.com/carlohamalainen/carlo-comments) system in [Datastar/Go](https://codeberg.org/carlohamalainen/carlo-comments-ds)

Let's walk through it.

The core of the [Hugo comment template](https://codeberg.org/carlohamalainen/carlo-comments-ds/src/branch/main/carlohamalainen.github.io/layouts/partials/posts/carlo-comments-ds.html)
is the comments block:

```html
<section id="comments"
         data-on-intersect__once="@get('https://comments.carlo-hamalainen.net/comments/v1/list?postID={{ strings.TrimSuffix "/" .RelPermalink }}')">
  <p>Loading comments…</p>
</section>
```

The attribute [data-on-intersect__once](https://data-star.dev/reference/attributes#data-on-intersect) is where the current
comments will appear; initially the user sees `Loading comments...` and the Go backend will morph the `id="comments"` HTML
using an [SSE patch](https://pkg.go.dev/github.com/starfederation/datastar-go/datastar#PatchSSE).

The next block is for the comment form, and it involves a Cloudflare Turnstile widget, so here it is in full:

```html
<div data-signals="{widgetId: '', turnstileToken: '', name: '', comment: '', submitting: false }">

  <div id="turnstile-container"
       data-init=
          "$widgetId = turnstile.render('#turnstile-container', 
              { sitekey: '0x4AAAAAAAfct15KixMdrAXv', 
                callback: (token) => $turnstileToken = token 
              })"
  >
  </div>

  <p data-show="$turnstileToken === ''">Comment form pending Cloudflare verification...</p>

  <form id="myform" data-show="$turnstileToken" style= "display: none" name="myform">
    <input type="hidden" name="cf-turnstile-response" data-bind= "turnstileToken">
    <input type="hidden" name="postID" value="{{ strings.TrimSuffix "/" .RelPermalink }}">


    <p><label for="name">Name (required):</label><br>
    <input type="text" id="name" name="name" required="" data-bind="name"></p>

    <p><label for="email">Email (optional):</label><br>
    <input type="email" id="email" name="email"></p>

    <p><label for="comment">Comment:</label><br>
    <textarea id="comment" name="comment" rows="8" cols="50" required="" data-bind="comment"></textarea></p>

    <button type="button"
      data-indicator:submitting
      data-attr:disabled="!$name.trim() || !$comment.trim() || $submitting"
      data-on:click="@post('https://comments.carlo-hamalainen.net/comments/v1/new', {contentType: 'form'})"
      >
      Submit comment</button>

    <span class="submitting-indicator" data-show="$submitting">Submitting...</span>
  </form>

  <div id="comment-status"></div>

</div>
```

Walking through this: first we declare our [signals](https://data-star.dev/guide/reactive_signals):

```html
<div data-signals="{widgetId: '', turnstileToken: '', name: '', comment: '', submitting: false }">
```

`widgetId` is the ID of the Cloudflare Turnstile widget; `turnstileToken` is
the token they return on successful human validation; `name` and `comment` are
for the content of the form fields.

The form itself is only visible when the user has validated with the Cloudflare
Turnstile (i.e. the signal `turnstileToken` is not `''`); for this we 
use [data-show](https://data-star.dev/guide/reactive_signals#data-show):

```html
  <form id="myform" data-show="$turnstileToken" style="display: none" name="myform">
```

We bind the content of the name field to the signal `name` using 
[data-bind](https://data-star.dev/guide/reactive_signals#data-bind):

```html
    <input type="text" id="name" name="name" required="" data-bind="name"></p>
```

In this way, we can disable the submit button if `$name.trim()` is empty; and similar for the 
comment field.

We use [data-indicator](https://data-star.dev/reference/attributes#data-indicator) to disable the submit button while
the POST is in flight; and the action of the button itself is defined using 
[data-on:click](https://data-star.dev/reference/attributes#data-on) and the `@post` action.

```html
    <button type="button"
      data-indicator:submitting
      data-attr:disabled="!$name.trim() || !$comment.trim() || $submitting"
      data-on:click="@post('https://comments.carlo-hamalainen.net/comments/v1/new', {contentType: 'form'})"
      >
      Submit comment</button>
```

On the [backend](https://codeberg.org/carlohamalainen/carlo-comments-ds/src/branch/main/main.go) we have a single 
type for a comment:

```go
type comment struct {
	commentID     string
	siteID        string
	postID        string
	timestampMS   int64
	timestamp     time.Time
	sourceAddress string
	author        string
	authorEmail   string
	comment       string
	isActive      bool
}
```

The HTTP handler for the comments on a page is remarkably short - it just loads the comments 
from SQLite, and then in hypermedia style it _produces the HTML and patches the comments element_:

```go
func handleList(w http.ResponseWriter, r *http.Request) {
	postID := r.URL.Query().Get("postID")
	if postID == "" {
        // error case...
		return
	}

	sse := datastar.NewSSE(w, r)

	// read comments from SQLite
	comments, err := getVisibleComments(r.Context(), postID)
	if err != nil {
		// error case...
		return
	}

	var buf bytes.Buffer
	if len(comments) == 0 {
		buf.WriteString(`<p>No comments yet.</p>`)
	} else {
		buf.WriteString(`<h2 class="comments-heading">Comments</h2>`)
		for _, c := range comments {
			writeCommentHTML(&buf, c)
		}
	}

	sse.PatchElements(buf.String(), datastar.WithSelectorID("comments"), datastar.WithModeInner())
}
```

We could use templating (tmpl? gomponents?) but the HTML is so short it can be done in an `Fprintf`:

```go
func writeCommentHTML(w io.Writer, c comment) {
	fmt.Fprintf(w, `<article class="comment">
  <header><span class="comment-author">%s</span> <time datetime="%s">%s</time></header>
  <div class="comment-body">%s</div>
</article>
`,
		html.EscapeString(c.author),
		c.timestamp.Format(time.RFC3339),
		c.timestamp.Format("2 Jan 2006"),
		renderCommentMarkdown(c.comment),
	)
}
```

That's it!

Load the comments from SQLite, produce the HTML fragments, and patch them in with SSE. No serialising and deserialising, 
no alternative almost-the-same-but-not-quite representation in the front end. 

So simple!

The Elm+Go version was a bit more general, supporting multiple backends (SQLite, S3, DynamoDB), but even 
being conservative and ignoring the non-SQLite backends, the Elm+Go version is over 2000 lines of code:

```
     761 carlo-comments/admin/src/Main.elm
     275 carlo-comments/api/server/comment.go
      19 carlo-comments/api/server/context.go
     139 carlo-comments/api/server/email.go
      23 carlo-comments/api/server/errors.go
      72 carlo-comments/api/server/middlewares.go
      60 carlo-comments/api/server/routes.go
     182 carlo-comments/api/server/server.go
      47 carlo-comments/api/server/turnstile.go
     100 carlo-comments/api/server/urls.go
      44 carlo-comments/api/server/user.go
      92 carlo-comments/api/server/utils.go
     130 carlo-comments/api/sqlite/comment.go
      47 carlo-comments/api/sqlite/db.go
       1 carlo-comments/api/sqlite/user.go
      53 carlo-comments/api/main.go

    2045 total
```

Meanwhile the Go+Datastar version is just over 800 lines, and that includes a fully functional admin page, something 
I never got around to doing properly in the old Elm version.

```
     680 carlo-comments-ds/main.go
     131 carlo-comments-ds/carlohamalainen.github.io/layouts/partials/posts/carlo-comments-ds.html
     811 total
```

The real win is removing duplicated data structures and intermediate representations of the data (the JSON form).

# Epilogue

Datastar is my framework of choice for building reactive web apps 🚀

I wonder if Cortex Live would benefit from a hypermedia/Datastar approach? 🤔

One benefit of the Datastar architecture is the easy "god mode" view; since
state is all on the server it is easy to create admin views of what users are
actually doing. Having users on a web browser instead of a desktop application
also removes the labour of making users to update a desktop client. Traders
are busy but even they can manage Ctrl-R during business hours.

{{< figure src="view2-cortex-live.png" link="view2-cortex-live.png" target="_blank" rel="noopener" alt="View2" width="80%" >}}

