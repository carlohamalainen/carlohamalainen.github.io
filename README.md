# carlohamalainen.github.io

## 2023-09 Debian Bookworm

Wiped gems and started afresh using the github-pages gem:

```bash
rm -fr $HOME/gems/*
git rm Gemfile.lock

gem install github-pages
./build.sh
./local_serve.sh
```

Also had to add ``webrick`` to ``Gemfile``.

## Dependencies on Mint 19

    sudo apt-get install ruby ruby-dev make build-essential

Install Ruby gems in my home directory:

    export GEM_HOME=$HOME/gems
    export PATH=$HOME/gems/bin:$PATH

    gem install jekyll bundler jekyll-paginate

Initialise in this repo:

    cd carlohamalainen.github.io
    jekyll new . --force

Let's not use minima from the gem, but from our own copy:

    $ bundle info minima
      * minima (2.5.1)
        Summary: A beautiful, minimal theme for Jekyll.
        Homepage: https://github.com/jekyll/minima
        Path: /home/carlo/gems/gems/minima-2.5.1

Mash the files over the top of our Jekyll install:

    $ cp -r  /home/carlo/gems/gems/minima-2.5.1/_includes .
    $ cp -r  /home/carlo/gems/gems/minima-2.5.1/_layouts .
    $ cp -r  /home/carlo/gems/gems/minima-2.5.1/_sass .

Make the theme wide:

    commit a788852dbd317cf3091caeb2b97e94c255e4f2a7 (HEAD -> master)
    Author: Carlo Hamalainen <carlo@carlo-hamalainen.net>
    Date:   Sun Feb 23 21:16:36 2020 +0800

        Set the width to 90%

    diff --git a/_sass/minima.scss b/_sass/minima.scss
    index cb0865b..9b5ed72 100644
    --- a/_sass/minima.scss
    +++ b/_sass/minima.scss
    @@ -21,7 +21,10 @@ $grey-color-dark:  darken($grey-color, 25%) !default;
     $table-text-align: left !default;

     // Width of the content area
    -$content-width:    800px !default;
    +// $content-width:    800px !default;
    +
    +// I like the theme to be wide.
    +$content-width:    90% !default;

     $on-palm:          600px !default;
     $on-laptop:        800px !default;
