# frog

> Note: This has been tested on Mac OS/X. It should work fine on
> Linux.  CAVEAT: It has _not_ been tested on Windows--it's possible
> that the path handling might not be exactly right.

## Overview

Frog is a static web site generator written in [Racket][].

You write content in [Markdown][]. You generate files. To deploy, you
push them to a GitHub Pages repo, or copy them to Amazon S3, or
whatever.

Posts get a variety of automatic blog features.

You can also create non-post pages.

The generated site presumes you're using [Bootstrap][] for CSS.

Yes, it's very much like Octopress and countless others. But it
doesn't require any Ruby gemmage. The only non-Racket part is
optionally using [Pygments][] to do syntax highlighting.

Frog? Frozen blog.

## Layout

The layout is basically this:

```sh
project/
  # These are files you edit:
  .frogrc
  _src/
    footer.md
    About.md
    ...
    posts/
      2013-01-01-a-blog-post-title.md
      2013-02-15-another-blog-post-title.md
      ...
  css/
    bootstrap.css
    bootstrap.min.css
    bootstrap-responsive.css
    bootstrap-responsive.min.css
    pygments.css # control syntax highlighting
  js/
    bootstrap.js
    bootstrap.min.js
  img/
    navbar-logo.jpg
  favicon.ico
  # These are files that are generated by Frog:
  index.html
  About.html
  sitemap.txt
  tags/
  feeds/
  2013/01/a-blog-post-title.md
  2013/02/another-blog-post-title.md
  ...
```

## Posts

You create new posts in `_src/posts`. They should be named
`YYYY-MM-DD-TITLE.md` and need to have some meta-data in the first few
lines.

You can do `racket frog.rkt -n "My Title"` to create such a file
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

`/feeds/all.xml` is an Atom feed for all posts.

For each tag, there is a `tags/<tag-name>.html` index page (also
listed newest first) and a `/feeds/<tag-name>.xml` Atom feed.

If you post has any section headings (e.g. `# Heading Level 1`, `##
Heading level 2`), then an "On this page" table-of-contents is
automatically generated and placed in the left side bar.

Twitter and Google+ buttons are placed on each post page.

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

> Note: The navbar is currently hardcoded to look for `/About.html`, and
> that's it. It's a to-do item to let you specify more items, perhaps
> using a `_src/navbar.md` file.


## footer.md

The special file `_src/footer.md` is converted to HTML and placed at
the foot of all pages (both posts and non-post pages).


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

I have a soft spot for [Pygments][] because it's actually the first
existing open source project to which I contributed. I added a lexer
for the [Racket][] language. More importantly it has lexers for tons
of languages and is used by things like GitHub, BitBucket, and so
on. Plus, it fits the spirit of static web site generation better than
JavaScript options like [SyntaxHighlighter][].

## Dependencies and installations

This depends on two other Racket projects:

1. [#lang rackjure](https://github.com/greghendershott/rackjure)

2. [Racket Markdown parser](https://github.com/greghendershott/markdown)

Neither of these are Planet packages yet. As a result, you'll need to
`git clone` each one, and run `raco link` on each one.

## Per-project configuration: .frogrc

You need to place a `.frogrc` file in your project directory.

The contents:

```sh
# Required: Should NOT end in trailing slash.
scheme/host = http://www.example.com

title = My Awesome Blog
author = The Unknown Author

# Optional: The UA-xxxx account number
# google-analytics-account = UA-xxxxxxx

# Optional: The domain to use. Often just example.com not
# www.example.com
# google-analytics-domain = example.com

# Optional: Disqus comment system "short name"
# disqus-shortname = MyShortName

# Optional: Path to Pygments executable
# pygments-pathname = ~/src/pygments-main/pygmentize
```

## Command line

Run `racket /path/to/frog.rkt -h` to see the options.

```
frog.rkt [ <option> ... ]
 where <option> is one of
  -c, --clean : Delete generated files.
  -m, --make, -b, --build : Generate files.
  -p, --preview : Run a local server and starting your browser.
  -n <title>, --new <title> : Create a file for a new post based on today's
    date and your supplied <title>.
  --pygments-css <style-name> : Generate ./css/pygments.css using style-name (ex: 'default')
  --help, -h : Show this help
```

A typical usage would be:

1. Create a new post with `racket frog.rkt -n "My Post Title"`. The
name of the new file is displayed to stdout.

2. Edit the file in your preferred plain text editor.

3. Regenerate your site and preview it  with `racket frog.rkt
-bp`.

If you're using GitHub Pages, you can commit and push to deploy to
your real site.

## To-Do

Some things on the "roadmap" (provided anyone wants them):

- Paginate the index pages (show only N posts at a time, with
  older/newer links).
  
- Get the Bootstrap responsive mode working, then add this as a
  `.frogrc` item instead of hardcoding to off. (It was working for my
  previous hand-coded site, but for some reason it's not working for
  Frog.)
  
- Let the user supply a navbar.md to populate the top pnav
  bar. Probably as a Markdown unordered (bullet) list of links.


[Racket]: http://www.racket-lang.org
[Markdown]: http://daringfireball.net/projects/markdown/syntax
[Bootstrap]: http://twitter.github.com/bootstrap/index.html
[Pygments]: http://pygments.org/
[SyntaxHighlighter]: "http://alexgorbatchev.com/SyntaxHighlighter/
