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

> Note: This has been tested on Mac OS/X and Ubuntu; I'm using it for
> [my blog][]. CAVEAT: It has _not_ yet been tested on Windows--it's
> likely that the path handling isn't exactly right.

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

> The tag `DRAFT` (all uppercase) causes the post `.html` file _not_
> to be generated.

### Automatic post features

Posts are automatically included in various index pages and feeds.

Posts with _any_ tag go on the home page `/index.html`, in an Atom feed
`/feeds/all.atom.xml`, and in an RSS feed `/feeds/all.rss.xml`.

Posts for each tag go on an index page `/tags/<tag>.html`, in an Atom
feed `/feeds/<tag>.atom.xml`, and in an RSS feed
`/feeds/<tag>.rss.xml`.

The example `page-template.html` has the following features:

- Twitter and Google+ sharing buttons are placed on each post page.
- Twitter follow button.
- Disqus comments.
- Google Analytics tracking.

## Non-post pages

You can put other `.md` files in `_src`, and in any subdirs of
it. They will be converted to HTML as non-post pages.  For example,
`_src/About.md` will be `/About.html` in the site.

Non-post pages are _not_ included in any automatically generated index
pages or feeds.  If you want them to be linked in, you must do so
manually.

## sitemap.txt

A `/sitemap.txt` file (for web crawlers) is automatically generated
and includes all post and non-post pages. (It does _not_ include index
pages for tags.)

## Templates

Frog uses the Racket
[`web-server/templates`](http://docs.racket-lang.org/web-server/templates.html)
system based on `scribble/text` `@` expressions. This means that the
files are basically normal HTML format, with the ability to use `@` to
reference a template variable --- or indeed "escape" to any Racket
code.

In contrast to most templating systems, you have a full programming
language available -- Racket -- should you need it. However most of
what you need to do will probably very simple, such as the occasional
`if` or `when` test, or perhaps defining a helper function to minimize
repetition.

## Page template: `_src/page-template.html`

The `_src/page-template.html` template determines every page on your
site.

Anything in the file that looks like `@variable` or `@|variable` is a
template variable supplied by Frog.  Most of these should be
self-explanatory from their name and from seeing how they are used in
the default template.

## Post template: `_src/post-template.html`

The `_src/post-template.html` template determines how blog posts are
laid out within a page that is dedicated to one post. If you represent
the post itself as one `<article>` element (recommended), then this
file is the contents of that element.

Anything in the file that looks like `@variable` or `@|variable` is a
template variable supplied by Frog.  Most of these should be
self-explanatory from their name and from seeing how they are used in
the default template.

> **NOTE**: This template does _not_ control how a blog post is laid
> out on an index page like `/index.html` or
> `/tags/<some-tag>.html`. Why?  The main purpose of this template is
> to specify things like Disqus comments, Tweet and +1 sharing
> buttons, and older/newer links --- things that only make sense in
> the context of the post's dedicated page.

## Code blocks

Frog optionally uses [Pygments][] to do syntax highlighting. In your
markdown using backtick code blocks you can specify a language:

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

Install Pygments (optional, if you want syntax highlighting for fenced
code blocks). On OS X and Linux:

    $ sudo easy_install Pygments

On Linux you might first need to install `easy_install`:

    $ sudo apt-get install python-setuptools

## Starting a new blog project

Creating a new blog project is 3 easy steps:

```sh
# Create a subdir
$ mkdir frog-project
# Go there
$ cd frog-project
# Tell Frog to create default files and dirs
$ raco frog --init
Configuration /tmp/frog-project/.frogrc not found; using defaults.
Creating files in /tmp/frog-project/:
/tmp/frog-project/.frogrc
/tmp/frog-project/_src/About.md
/tmp/frog-project/_src/page-template.html
/tmp/frog-project/_src/post-template.html
/tmp/frog-project/_src/posts/2012-01-01-a-2012-blog-post.md
/tmp/frog-project/css/
/tmp/frog-project/js/
/tmp/frog-project/img/
Project ready. Try `raco frog -bp` to build and preview.
$
```

You can go ahead and build/preview this to get a feel for the default
starting point:

```sh
# Build and preview it
$ raco frog -bp
Using configuration /tmp/frog-project/.frogrc
13:28 Done generating files
Your Web application is running at http://localhost:3000/.
Stop this program at any time to terminate the Web Server.
# The home page should launch in your web browser.
# Switch back to the command prompt and type C-c to quit:
^C
Web Server stopped.
$
```


### Project file tree

Here is the file tree that `raco frog --init` creates for you and Frog
expects:

```sh
project/
  # Files provided by you:
  .frogrc       # see next section of README for details
  _src/
    page-template.html  # lets you define the page layout
    post-template.html  # lets you define the <article> layout
    posts/
      # You'll create these using `raco frog -n <post-title>`
      2013-01-01-a-blog-post.md
      2013-02-15-another-blog-post.md
      ...
    # Zero or more other .md files for non-post pages.
    # May be in subdirs.
  css/
    bootstrap.css                #\
    bootstrap.min.css            # get these files
    bootstrap-responsive.css     # from Bootstrap
    bootstrap-responsive.min.css #/
    pygments.css                 # used to style code elements from Pygments
    custom.css                   # other styles you provide; may be empty
  js/
    bootstrap.js                 # get these files
    bootstrap.min.js             # from Bootstrap
  img/
    feed.png
  favicon.ico

  # Files generated by Frog.
  # (Just shown FYI, you don't create these manually)
  index.html
  sitemap.txt
  tags/
  feeds/
  # Post pages in subdirs.
  # Exact layout depends on `permalink1 in `.frogrc`.
  blog/
    2013/
      01/
        a-blog-post-title/
          index.html
        ...
    2013/
      02/
        another-blog-post-title/
          index.html
        ...
  ...
```

> **TIP**: The official Bootstrap files are at
> <http://twitter.github.io/bootstrap/>. For alternative "themes", see
> <http://http://bootswatch.com/>.

> **TIP**: For examples of files you can use and rename to
> `pygments.css`, see <https://github.com/richleland/pygments-css>.


### Per-project configuration: .frogrc

`raco frog --init` creates a `.frogrc` file in your project directory:

```sh
# Required: Should NOT end in trailing slash.
scheme/host = http://www.example.com

title = My Awesome Blog
author = The Unknown Author

# Whether to show the count of posts next to each tag in sidebar
show-tag-counts? = false

# Pattern for blog post permalinks
# Optional: Default is "/{year}/{month}/{title}.html".
# Here's an example of the Jekyll "pretty" style:
permalink = /blog/{year}/{month}/{day}/{title}/index.html
# There is also {filename}, which is the `this-part` portion of
# your post's YYYY-MM-DD-this-part.md file name. This is in case
# you don't like Frog's encoding of your post title and want to
# specify it exactly yourself, e.g. to match a previous blog URI.

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
```

## Creating blog posts

A typical workflow:

1. Create a new post with `raco frog -n "My Post Title"`. The
name of the new `.md` file is displayed to stdout.

2. Edit the `.md` file in your preferred plain text editor.

3. Regenerate your site and preview it with `raco frog -bp`. (You might
repeat steps 2 and 3 a few times until you're satisfied.)

4. Deploy. If you're using [GitHub Pages][], you can commit and push
to deploy to your real site. If you're using some other method, you
can copy or rsync the files to your static file server.

> **TIP**: If you use Emacs, try markdown-mode. Although Markdown is
> _really_ simple, markdown-mode makes it even more enjoyable.


# Bug reports? Feature requests?

Please use [GitHub Issues][].


[my blog]: http://www.greghendershott.com
[Racket]: http://www.racket-lang.org
[Markdown]: http://daringfireball.net/projects/markdown/syntax
[Bootstrap]: http://twitter.github.com/bootstrap/index.html
[responsive]: https://en.wikipedia.org/wiki/Responsive_web_design
[Pygments]: http://pygments.org/
[pygments.rb]: https://github.com/tmm1/pygments.rb
[SyntaxHighlighter]: http://alexgorbatchev.com/SyntaxHighlighter/
[GitHub Pages]: https://help.github.com/articles/user-organization-and-project-pages
[GitHub Issues]: https://github.com/greghendershott/frog/issues
