# Frog

<a href="http://www.flickr.com/photos/doug88888/4717363945/" title="Happy Green frog by @Doug88888, on Flickr"><img src="http://farm5.staticflickr.com/4070/4717363945_b73afd78a9.jpg" width="300" height="300" alt="Happy Green frog"></a>
=
[<img src="https://raw.github.com/dcurtis/markdown-mark/master/png/48x30-solid.png">][Markdown]
**+**
[<img src="https://raw.github.com/twitter/bootstrap/master/docs/assets/ico/favicon.png">][Bootstrap]
**->**
[<img src="http://racket-lang.org/logo.png" width="30" height="30">][Racket]
**+**
[<img src="http://pygments.org/media/icon.png" width="40" height="40">][Pygments]

<sub><em><a href="http://www.flickr.com/photos/doug88888/4717363945/">Frog image by @Goug8888</a>, used under Creative Commons license [Attribution-NonCommercial-ShareAlike 2.0 Generic](http://creativecommons.org/licenses/by-nc-sa/2.0/)</em></sub>

## Overview

Frog is a static web site generator written in [Racket][].

You write content in [Markdown][]. You generate files. To deploy, you
push them to a GitHub Pages repo (or copy them to Amazon S3, or
whatever).

Posts get a variety of automatic blog features.

You can also create non-post pages.

The generated site uses [Bootstrap][], which is [responsive][],
automatically adapting to various screen sizes.

Yes, it's very much like Octopress and others. But Frog doesn't
require installing Ruby. Installing Racket is typically waaaay
simpler and faster.

The only non-Racket part is optionally using [Pygments][] to do syntax
highlighting.

Q: "Frog"?  
A: Frozen blog.

> Note: This has been tested on Mac OS/X; I'm using it for
> [my blog][].  It should work fine on Linux.  CAVEAT: It has _not_
> yet been tested on Windows--it's likely that the path handling
> isn't exactly right.

## Posts

You create new posts in `_src/posts`. They should be named
`YYYY-MM-DD-TITLE.md` and need to have some meta-data in the first few
lines.

You can do `raco frog -n "My Title"` to create such a file
easily. This will also fill in the required meta-data section. The
markdown file starts with a code block (indented 4 spaces) that must
contain these three lines:

```markdown
    Title: A blog post
    Date: 2012-01-01T00:00:00
    Tags: foo, bar, tag with spaces, baz

Everything from here to the end is your post's contents.

If you put `<--! more -->` on a line, that is the "above-the-fold"
marker. Contents above the line are the "summary" for index pages and
Atom feeds.

<!-- more -->

Contents below `<!-- more -->` are omitted from index pages and Atom
feeds. A "Continue reading..." link is provided instead.

```

`Title` can be anything.

`Date` must be an ISO-8601 datetime string: `yyyy-mm-ddThr:mn:sc`.

`Tags` are optional (although you have to include the `Tags:` part).

### Automatic post features

Posts are automatically included in index pages and feeds.

`/index.html` is an index for all posts, listed newest first.

`/feeds/all.atom.xml` is an Atom feed, and `/feeds/all.rss.xml` is an
RSS feed, for _all_ posts.

Each tag has an index page `tags/<tag>.html`, an Atom feed
`/feeds/<tag>.atom.xml`, and an RSS feed `/feeds/<tag>.rss.xml`.

If a post has any section headings (e.g. `# Heading Level 1`, `##
Heading level 2`), then an _"On this page"_ table of contents is
automatically generated and placed in the left side bar.

Twitter and Google+ sharing buttons are placed on each post page.

Optional: If your `.frogrc` has a Disqus shortname, then Disqus
comments are automatically included.

Optional: If your `.frogrc` has Google Analytics account information,
then the appropriate tracking code is automatically inserted.

### The `DRAFT` tag

The tag `DRAFT` (all uppercase) causes the post _not_ to be generated.

This way, you can commit the source `.md` file to your repo, and push,
but there will be no corresponding `.html` generated and pushed.  (The
use case here is GitHub pages. If you deploy to something like Amazon
S3, the similar point is that no `.html` file will be generated and
deployed to that.)  _I should rewrite this to be more clear about
different usage scenarios_.

## Non-post pages

You can put other `.md` files in `_src`, and in any subdirs of
it. They will be converted to HTML as non-post pages.  For example,
`_src/About.md` will be `/About.html` in the site.

Non-post pages are _not_ included in any automatically generated index
pages or feeds.  If you want them to be linked in, you must do so
manually.

## _src/footer.md

The special file `_src/footer.md` is converted to HTML and placed at
the foot of all pages (both posts and non-post pages).

## _src/navbar.md (optional)

The special file `_src/navbar.md` optionally adds extra items to the
top navbar.  It should consist of a single bulleted list of links, in
Markdown.  For example:

    - [About](/About.html)
    - [Something Else](/path/to/thing)

## _src/homehead.md (optional)

The special file `_src/homehead.md` replaces the text "All Posts" on
the home page with whatever you prefer.

Note that this is used _only_ for your blog's home page. This doesn't
appear on tag index pages like "All posts tagged _Some Tag_".

## sitemap.txt

A `/sitemap.txt` file (for web crawlers) is automatically generated
and includes all post and non-post pages. (It does _not_ include index
pages for tags.)

## Sharing buttons

Sharing buttons for Twitter and Google+ are automatically put at the
bottom of posts and non-post pages.

## Code blocks

Frog optionally uses [Pygments][] to do syntax
highlighting. In your markdown using backtick code blocks you can
specify a language:

    ```language
    some lines
    of code
    ```

That _language_ is given to Pygments as the lexer to use.

For example this:

    ```js
    /**
     * Some JavaScript
     */
    function foo()
    {
        if (counter <= 10)
            return;
        // it works!
    ```

Yields this:

```js
/**
 * Some JavaScript
 */
function foo()
{
    if (counter <= 10)
        return;
    // it works!
```

And this:

    ```racket
    #lang racket
    ;; Finds Racket sources in all subdirs
    (for ([path (in-directory)])
      (when (regexp-match? #rx"[.]rkt$" path)
        (printf "source file: ~a\n" path)))
    ```

Yields this:

```racket
#lang racket
;; Finds Racket sources in all subdirs
(for ([path (in-directory)])
  (when (regexp-match? #rx"[.]rkt$" path)
    (printf "source file: ~a\n" path)))
```

The appearance is controlled by your `css/pygments.css` file:
[examples](https://github.com/richleland/pygments-css).

I have a soft spot for [Pygments][] because it's actually the first
existing open source project to which I contributed. I added a lexer
for the [Racket][] language. More importantly it has lexers for tons
of languages and is used by things like GitHub (via [pygments.rb][]),
BitBucket, and so on. Plus, it fits the spirit of static web site
generation better than JavaScript options like [SyntaxHighlighter][].

### Wrapping

If you use larger font sizes the code may wrap, and get out of
alignment with the line numbers. To avoid the wrapping, add the
following to your `css/custom.css`:

```css
/* When highlighted code blocks are too wide, they wrap. Resulting in the */
/* line numbers column's rows not lining up with the code rows. Prevent */
/* wrapping. */
pre {
    white-space: pre;
    width: inherit;
}
```

----------------------------------------------------------------------

## Installing Frog

Install Racket 5.3.4 or newer.

Install Frog:

    $ raco pkg install frog

> Frog depends on two other Racket projects:
> 
> 1. [#lang rackjure](https://github.com/greghendershott/rackjure)
> 
> 2. [Racket Markdown parser](https://github.com/greghendershott/markdown)
> 
> The `raco pkg install frog` should have asked to install them. If you
> need to install them individually, use `raco pkg install rackjure` and
> `raco pkg install markdown`.

Install Pygments (optional, if you want code block syntax
highlighting). On Mac OS X:

    $ sudo easy_install Pygments

## Starting a new blog project

Create a subdirectory for the project.

[Example](https://github.com/greghendershott/frog/tree/master/example).

### File structure

Set up the directory layout like this:

```sh
project/
  # Files provided by you:
  .frogrc       # see next section of README for details
  _src/
    footer.md   # goes on every page
    navbar.md   # adds extra items to navbar (optional)
    posts/
      # You'll create these using `raco frog -n <post-title>`
      2013-01-01-a-blog-post.md
      2013-02-15-another-blog-post.md
      ...
    # Zero or more other .md files for non-post pages
  css/
    pygments.css  # used to style code elements from Pygments
    custom.css    # other styles you provide; must exist but may be empty
    bootstrap.css                #\
    bootstrap.min.css            # get these files
    bootstrap-responsive.css     # from Bootstrap
    bootstrap-responsive.min.css #/
  js/
    bootstrap.js                 # get these files
    bootstrap.min.js             # from Bootstrap
  img/
    navbar-logo.jpg
  favicon.ico
  
  # Files generated by Frog (just FYI, you don't create these
  # manually):
  index.html
  sitemap.txt
  About.html
  tags/
  feeds/
  # Post pages in YYYY/MM subdirs:
  2013/
    01/
      a-blog-post-title.md
      ...
  2013/
    02/
      another-blog-post-title.md
      ...
  ...
```

Tips:

For examples of files you can use and rename to `pygments.css`, see
<https://github.com/richleland/pygments-css>.

The official Bootstrap files are at
<http://twitter.github.io/bootstrap/>. For alternative "themes", see
<http://http://bootswatch.com/>.


### Per-project configuration: .frogrc

You need to place a `.frogrc` file in your project directory.

The contents:

```sh
# Required: Should NOT end in trailing slash.
scheme/host = http://www.example.com

title = My Awesome Blog
author = The Unknown Author

# Pattern for blog post permalinks
# Optional: Default is "/{year}/{month}/{title}.html".
# Here's an example of the Jekyl "pretty" style:
permalink = /blog/{year}/{month}/{day}/{title}/index.html

# Use Bootstrap responsive CSS?
bootstrap-responsive? = true

# Use Booststrap minified CSS and JS files?
bootstrap-minified? = true

# Older/newer buttons' text. One of:
# - "age" (just "Older or "Newer")
# - "title" (just the blog post title)
# - "both" (both)
older/newer-buttons = both

# Should index page items contain full posts -- more than just the
# portion above "the jump" <!-- more --> marker (if any)?
index-full? = true

# Should feed items contain full posts -- more than just the portion
# above "the jump" <!-- more --> marker (if any)?
feed-full? = true

# How many items to include on index pages?
max-index-items = 20

# How many items to include in feeds?
max-feed-items = 20

# Decorate feed URIs with Google Analytics query parameters like
# utm_source ?
decorate-feed-uris? = true

# Insert in each feed item an image bug whose URI is decorated with
# Google Analytics query parameters like utm_source ?
feed-image-bugs? = true

# Optional: The URI for the favicon. Defaults to "/favicon.ico".
# Change if you need e.g. /favicon.png or http://elsewhere/favicon.ico.
# favicon = /favicon.ico

# Optional: The Google Analytics UA-xxxx account number
# google-analytics-account = UA-xxxxxxx

# Optional: The Google Analytics domain to use.
# google-analytics-domain = example.com

# Optional: Disqus comment system "short name"
# disqus-shortname = MyShortName

# Optional: Path to Pygments executable
# pygments-pathname = ~/src/pygments-main/pygmentize

# Optional: Twitter name for follow button. (Do NOT include @ prefix.)
# twitter-name = xxx
```

## Command line

Run `raco frog -h` to see the options:

```
frog.rkt [ <option> ... ]
 where <option> is one of
  -n <title>, --new <title> : Create a file for a new post based on today's
    date and your supplied <title>.
  -m, --make, -b, --build : Generate files.
  -p, --preview : Run a local server and start your browser.
  -c, --clean : Delete generated files.
  --pygments-css <style-name> : Generate ./css/pygments.css using style-name (ex: 'default')
/ -v, --verbose : Verbose. Put first.
\ -V, --very-verbose : Very verbose. Put first.
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 /|\ Brackets indicate mutually exclusive options.
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
```

## Typical workflow

1. Create a new post with `raco frog -n "My Post Title"`. The
name of the new file is displayed to stdout.

2. Edit the Markdown file in your preferred plain text editor.

3. Regenerate your site and preview it with `raco frog -bp`. (You might
repeat steps 2 and 3 a few times until you're satisfied.)

4. Deploy. If you're using [GitHub Pages][], you can commit and push
to deploy to your real site. If you're using some other method, you
can copy or rsync the files to your static file server.

## Tips

- If you use Emacs, try markdown-mode. Markdown is _really_ simple,
  but markdown-mode makes it even more enjoyable.

## To-Do

See [open issues labeled `enhancement`][enhancements].



[my blog]: http://www.greghendershott.com
[Racket]: http://www.racket-lang.org
[Markdown]: http://daringfireball.net/projects/markdown/syntax
[Bootstrap]: http://twitter.github.com/bootstrap/index.html
[responsive]: https://en.wikipedia.org/wiki/Responsive_web_design
[Pygments]: http://pygments.org/
[pygments.rb]: https://github.com/tmm1/pygments.rb
[SyntaxHighlighter]: http://alexgorbatchev.com/SyntaxHighlighter/
[GitHub Pages]: https://help.github.com/articles/user-organization-and-project-pages
[enhancements]: https://github.com/greghendershott/frog/issues?labels=enhancement&page=1&state=open
