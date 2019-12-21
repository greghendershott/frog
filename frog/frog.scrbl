#lang scribble/manual

@(require (for-label racket/base
                     racket/date
                     web-server/templates
                     scribble/text
                     scribble/manual
                     xml/xexpr
                     frog/enhance-body
                     frog/paths
                     frog/params
                     frog/scribble
                     frog/widgets
                     frog/verbosity)
          scribble/core
          scribble/extract)

@(define (grey . contents)
   (elem #:style (style #f (list (color-property "grey")))
         contents))

@(define (comment . contents)
   (grey contents))

@(define (pre #:title [title #f] . contents)
   (let* ([title    (cond [title (list (grey (italic (tt title))) "\n")]
                          [else  (list)])]
          [contents (append title contents)]
          [contents (apply verbatim contents)])
     (nested #:style 'code-inset contents)))

@(define-syntax-rule (deftv id contract contents ...)
  (defthing #:kind "template variable"
            #:link-target? #f
            id contract contents ...))


@title[#:tag "top"]{Frog}
@author[@hyperlink["https://github.com/greghendershott"]{Greg Hendershott}]

@margin-note{Please use
@hyperlink["https://github.com/greghendershott/frog/issues"]{GitHub
Issues} to report bugs or make feature requests.}

Frog is a static web site generator written in
@hyperlink["https://www.racket-lang.org"]{Racket}.

You write content in
@hyperlink["https://daringfireball.net/projects/markdown/syntax"]{markdown}
or
@hyperlink["https://docs.racket-lang.org/scribble/index.html"]{scribble} formats.

You generate files. To deploy, you push them to a GitHub Pages repo
(or copy them to Amazon S3, or whatever).

Posts get a variety of automatic blog features.

You can also create non-post pages.

The default templates use
@hyperlink["https://getbootstrap.com/"]{Bootstrap}, which is
@hyperlink["https://en.wikipedia.org/wiki/Responsive_web_design"]{``responsive''}
(adapts to various screen sizes).

Why ``Frog''? @bold{Fr}ozen bl@bold{og}.

@table-of-contents[]


@section{Quick start}

@subsection{Installing Frog}

@itemlist[#:style 'ordered

@item{Install @hyperlink["https://racket-lang.org/download/"]{Racket}.

On macOS you will need to add @tt{/Applications/Racket\
7.0/bin} (or similar) to your @envvar{PATH} in order to be able to run
things like @exec{racket} or @exec{raco} at the command line.}

@item{Install Frog: @pre{$ raco pkg install frog}}

@item{Optional: If you want syntax highlighting for fenced code blocks:

@itemlist[#:style 'ordered
@item{Install the latest version of @hyperlink["https://www.python.org/downloads/"]{Python}.}
@item{Install @hyperlink["http://pygments.org/"]{Pygments}: @pre{$ python3 -m pip install pygments}}]}]

@subsection{Updating Frog}

To update Frog and its dependencies:

@pre{$ raco pkg update --update-deps frog}

@subsection{Starting a new blog project}

Creating a new blog project is 3 easy steps:

@itemlist[#:style 'ordered

@item{Create a subdirectory.
@pre{$ mkdir frog-project}}

@item{Go there.
@pre{$ cd frog-project}}

@item{Tell Frog to create default files and directories.
@pre{
$ raco frog --init
Creating files in /tmp/frog-project/:
/tmp/frog-project/frog.rkt
/tmp/frog-project/_src/About.md
/tmp/frog-project/_src/page-template.html
/tmp/frog-project/_src/post-template.html
/tmp/frog-project/_src/posts/2012-01-01-a-2012-blog-post.md
/tmp/frog-project/css/
/tmp/frog-project/js/
/tmp/frog-project/img/
Project ready. Try `raco frog -bp` to build and preview.
}}

]

You can go ahead and build/preview this to get a feel for the default
starting point:

@pre{
$ raco frog -bp
Frog 0.26
Launching /usr/bin/python pipe.py
Your Web application is running at http://localhost:3000/index.html.
Stop this program at any time to terminate the Web Server.
  C-c C-c
Web Server stopped.
}


@subsubsection{Project file tree}

Here is the file tree that @exec{raco frog @DFlag{init}} creates for
you and Frog expects:

@pre{
project/
  @comment{# Files provided by you:}
  frog.rkt      @comment{# @secref["config"]}
  _src/         @comment{# default; see @racket[current-source-dir] in @secref["config"]}
    page-template.html  @comment{# entire page layout: @secref["page-template"]}
    post-template.html  @comment{# @tt{<article>} layout: @secref["post-template"]}
    index-template.html @comment{# index pages: @secref["index-template"]}
    posts/
      @comment{# You'll create these using @exec{raco frog @Flag{n} <post-title>}}
      2013-01-01-a-blog-post.md
      2013-02-15-another-blog-post.md
      @comment{...}
    @comment{# Zero or more other @tt{.md} files, for non-post pages.}
    @comment{# May be in subdirs.}
  css/
    bootstrap.min.css        @comment{# get these files}
    bootstrap.min.css.map    @comment{# from @url["https://getbootstrap.com"]}
    pygments.css             @comment{# style code elements from Pygments}
    custom.css               @comment{# other styles you provide; may be empty}
    scribble.css
  js/
    jquery-3.2.1.slim.min.js @comment{# local copy of jQuery for Bootstrap}
    bootstrap.bundle.min.js  @comment{# from @url["https://getbootstrap.com/"]}
  img/
    feed.png
  favicon.ico
}

Here are the files created by Frog when you run @exec{raco frog
@Flag{b}} to (re)build the blog:

@pre{
project/  @comment{# default; see @racket[current-output-dir] in @secref["config"]}
  .frog/build   @comment{# a cache to support minimal rebuilds}
  index*.html
  sitemap.txt
  tags/
  feeds/
  @comment{# Post pages are in subdirs.}
  @comment{# Exact layout depends on @racket[current-permalink] in @secref["config"].}
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
}

Although the Frog
@hyperlink["https://github.com/greghendershott/frog/tree/master/example"]{example}
project has copies for example purposes, for your own project you
should get the official/latest Bootstrap 4 files directly from
Bootstrap.


To design a Bootstrap 4 "theme", try
@hyperlink["http://pikock.github.io/bootstrap-magic/"]{Bootstrap
Magic}. Also,
@hyperlink["http://bootswatch.com/"]{http://bootswatch.com/} has some
ready-made themes.

For examples of @filepath{pygments.css} code highlighting styles see
@hyperlink["https://github.com/richleland/pygments-css"]{https://github.com/richleland/pygments-css}.


@subsubsection[#:tag "config"]{@tt{frog.rkt}}

When you used @exec{raco frog @DFlag{init}} it created a
@filepath{frog.rkt} file in your project directory. @margin-note*{If
you upgrade from an older version of Frog that used a
@filepath{.frogrc} file: The first time the newer version of Frog
runs, it automatically creates an equivalent @filepath{frog.rkt}
from your @filepath{.frogrc}. Thereafter the @filepath{.frogrc} is
ignored, and may be deleted.}

Your @filepath{frog.rkt} lets you use simple Racket code to configure
and customize your blog.

@defmodule[frog/config #:lang]

The @racketmodname[frog/config] language provides bindings from:
@itemlist[
@item{@racketmodname[racket/base]}
@item{@racketmodname[racket/contract/base]}
@item{@racketmodname[racket/contract/region]}
@item{@racketmodname[xml/xexpr]}
@item{@racketmodname[threading]}
@item{@racketmodname[frog/params]}
@item{@racketmodname[frog/paths]}
@item{@racketmodname[frog/enhance-body]}
]

Furthermore, the @racketmodname[frog/config] language ensures that you
define three functions that are used by Frog:

@defproc[#:link-target? #f (init) any]{

This is called once, early when Frog starts.

You can set various parameters provided by
@racketmodname[frog/params]. To start, you might only need to set a
few:

@itemlist[
@item{@racket[current-title]}
@item{@racket[current-author]}
@item{@racket[current-scheme/host]}]}

@defproc[#:link-target? #f
         (enhance-body [xs (listof xexpr/c)]) (listof xexpr/c)]{

This is called for each post or non-post page, giving you the
opportunity to modify the @racket[xexpr?]s representing the content.

You may call functions provided by @racketmodname[frog/enhance-body].
You may also @racket[require] and use functions provided by a
third-party package.}

@defproc[#:link-target? #f (clean) any]{

Called during @exec{raco frog @DFlag{clean}}.}


@subsection[#:tag "create-posts"]{Creating blog posts}

A typical workflow:

1. Create a new post with @exec{raco frog @Flag{n} "My Post Title"}.
The name of the new @tt{.md} file is displayed to stdout.

2. Edit the @tt{.md} file in your preferred plain text editor.

3. Regenerate your site and preview it with @exec{raco frog
@Flag{bp}}. (You might repeat steps 2 and 3 a few times until you're
satisfied.)

4. Deploy. If you're using GitHub Pages, you can commit and push to
update your site. If you're using some other method, you can copy or
rsync the files to your static file server.


@section{Posts}

You create new posts in @filepath{_src/posts/}. There are several
source formats.

@subsection{Markdown source files}

Post source files in markdown format should be named
@filepath{@italic{YYYY-MM-DD-TITLE}.md} and need to have some metadata
in the first few lines.

You can do @exec{raco frog @Flag{n} "My Title"} to create such a file
easily. This will also fill in the required metadata section. The
markdown file starts with a code block (indented 4 spaces) that must
contain these three lines.

@pre|{
    Title: A blog post
    Date: 2012-01-01T00:00:00
    Tags: foo, bar, tag with spaces, baz
    Authors: Alice Baker, Charlie Dean

Everything from here to the end is your post's contents.

If you put `<!-- more -->` on a line, that is the ``above-the-fold''
marker. Contents above the line are the ``summary'' for index pages
and Atom feeds.

<!-- more -->

Contents below `<!-- more -->` are omitted from index pages and Atom
feeds. A ``Continue reading...'' link is provided instead.
}|

@subsubsection[#:tag "metadata"]{Post metadata}

@itemlist[

@item{@tt{Title} can be anything.}

@item{@tt{Date} must be an ISO-8601 datetime string:
@tt{yyyy-mm-ddThr:mn:sc}.}

@item{@tt{Tags} is optional. The tag @tt{DRAFT} (all uppercase)
causes the post HTML file @italic{not} to be generated.}

@item{@tt{Authors} is optional. When present, this overrides the
default @tt{Author} in your @secref["config"].}

]


@subsubsection{Markdown template files (experimental)}

Files with a @tt{.mdt} extension are first evaluated as
@secref["templates"]. The resulting text is fed to the markdown
parser, as for a @tt{.md} plain markdown source.

Such files may be used as the source for both posts and non-post
pages.

There are no template variables --- not even when the file
is being used as the source of a post. The template evaluation occurs
prior to the extraction of the post metadata.


@subsubsection[#:tag "markdown-code-blocks"]{Code blocks in markdown files}

Frog optionally uses @hyperlink["http://pygments.org/"]{Pygments} to
do syntax highlighting. Pygments has lexers for many, many languages.
Plus, it fits the spirit of static web site generation better than
JavaScript options like SyntaxHighlighter.

When using fenced code blocks, you can specify a language (as on
@hyperlink["https://help.github.com/articles/github-flavored-markdown#syntax-highlighting"]{GitHub}):

@pre|{
    ```language
    some lines
    of code
    ```
}|

That @litchar{language} is given to Pygments as the lexer to use.

The colors are controlled by your @filepath{css/pygments.css} file.
There are
@hyperlink["https://github.com/richleland/pygments-css"]{examples of
many styles}.

If you use larger font sizes, code may wrap and get out of alignment
with the line numbers. To avoid the wrapping, add the following to
your @filepath{css/custom.css}:

@pre[#:title "custom.css"]|{
/* When highlighted code blocks are too wide, they */
/* wrap -- resulting in the line numbers column's */
/* rows not lining up with the code rows. Prevent */
/* wrapping. */
pre {
    white-space: pre;
    width: inherit;
}
}|

@subsection{Scribble source files}

Post source files in Scribble format should be named
@tt{YYYY-MM-DD-TITLE.scrbl} and need to have some @secref["metadata"]
in the first few lines.

You can do @exec{raco frog @Flag{N} "My Title"} to create such a file
easily. This will also fill in the required metadata section.

See the
@hyperlink["https://github.com/greghendershott/frog/blob/master/example/_src/posts/2013-06-19-a-scribble-post.scrbl"]{example
Scribble post} and
@hyperlink["https://github.com/greghendershott/frog/blob/master/example/_src/A-Non-Post-Scribble-Page.scrbl"]{example
Scribble non-post page} for more information.

@subsubsection{Pygments code blocks}

@defmodule[frog/scribble]

@defproc[(pygment-code [#:lang lang string?][str string?] ...) paragraph?]{
In Scribble source files of course you can use @racket[codeblock],
@racket[racketblock], and friends to write code blocks for languages
that have DrRacket style syntax-coloring parsers.

For other languages, you can emit a @tt{<pre>} block with a language
tag (as happens with @secref["markdown-code-blocks"]) so that it can
be highlighted by Pygments.

Example usage:
@pre|{
@(require frog/scribble)
@pygment-code[#:lang "js"]{function foo() {return 1;}}
}|
}

@subsection{Automatic post features}

Posts are automatically included in various index pages and feeds.

All posts go on the home page @filepath{/index.html}, in an Atom feed
@filepath{/feeds/all.atom.xml}, and in an RSS feed
@filepath{/feeds/all.rss.xml}.

Posts for each tag go on an index page
@filepath{/tags/@italic{<tag>}.html}, in an Atom feed
@filepath{/feeds/@italic{<tag>}.atom.xml}, and in an RSS feed
@filepath{/feeds/@italic{<tag>}.rss.xml}.

The default @secref["post-template"] provides:

@itemlist[
@item{Twitter and Google+ sharing buttons.}
@item{Disqus comments.}
]

The default @secref["page-template"] (used for all pages, not just
post pages) also provides:

@itemlist[
@item{Twitter follow button.}
@item{Google Analytics tracking.}
]


@section{Non-post pages}

You can put @tt{.md}, @tt{.mdt}, and @tt{.scrbl} files in
@filepath{_src/} and its subdirs --- @italic{except}
@filepath{_src/posts/} --- and they will be converted to HTML pages.

For example, @filepath{_src/About.md} will be @filepath{/About.html}
in the site.

Non-post pages are @italic{not} included in any automatically
generated index pages or feeds. You can manually add links to them in
the nav bar by editing that portion of your @secref["page-template"].


@section{sitemap.txt}

A @filepath{/sitemap.txt} file (for web crawlers) is automatically
generated and includes all post and non-post pages. It does
@italic{not} include index pages for tags.


@section[#:tag "templates"]{Templates}

A @secref["page-template"] is used to define the overall @tt{html}
element for all generated pages. Some types of pages also use a nested
@secref["post-template"] or an @secref["index-template"].

Frog uses the Racket @seclink["templates" #:doc
'(lib "web-server/scribblings/web-server.scrbl")]{@racket[web-server/templates]}
system based on @secref["reader" #:doc '(lib
"scribblings/scribble/scribble.scrbl")]. This means that the files are
basically normal HTML format, with the ability to use @tt{@"@"} to
reference a template variable --- or indeed to "escape" to arbitrary
Racket code.

In contrast to most templating systems, you have a full programming
language available should you need it. However most of what you need
to do will probably be very simple, such as the occasional @racket[if]
or @racket[when] test, or perhaps @racket[define]ing a helper function
to minimize repetition.


@subsection[#:tag "page-template"]{Page template}

The @filepath{page-template.html} template specifies an @tt{<html>}
element used by Frog to generate every page on your site.

Anything in the file that looks like @tt{@"@variable"} or
@tt{@"@|variable|"} is a template variable supplied by Frog. Most of
these should be self-explanatory from their name and from seeing how
they are used in the default template.

@deftv[contents string?]{The contents of the page.}

@deftv[title string?]{The title of the page (for @tt{<title>}).}

@deftv[description string?]{The description of the page (for @tt{<meta>} content element).}

@deftv[keywords string?]{The keywords for the page (for @tt{<meta>} keywords element).}

@deftv[uri-prefix string?]{Effectively @racket[(or (current-uri-prefix) "")].}

@deftv[uri-path string?]{The path portion of the URI, e.g. @tt{/path/to/file.html}.}

@deftv[full-uri string?]{The full URI, e.g. @tt{http://example.com/path/to/file.html}.}

@deftv[atom-feed-uri string?]{The full URI to the Atom feed.}

@deftv[rss-feed-uri string?]{The full URI to the RSS feed.}

@deftv[tag (or/c string? #f)]{If an index page, the name of the index (such as
@racket["All Posts"] or @racket["Posts tagged foo"]).}

@deftv[tags-list-items string?]{HTML text having a @tt{<li>} for every tag
on the blog, suitable for putting in a @tt{<ul>}. Each @tt{<li>} has a
link to that tag's index page.}

@deftv[tags/feeds string?]{HTML text having, for each tag, a link to its
index page and a link to its Atom feed.}

@deftv[rel-prev (or/c string? #f)]{
Next related page, if any.
@verbatim|{@(when rel-prev @list{<link rel="next" href="@|rel-next|">})}|}

@deftv[rel-next (or/c string? #f)]{
Previous related page, if any.
@verbatim|{@(when rel-next @list{<link rel="prev" href="@|rel-prev|">})}|}


@subsection[#:tag "post-template"]{Post template}

The @filepath{post-template.html} template determines how blog posts
are laid out on pages that are dedicated to one post. The default
template defines an @tt{<article>} element.

For pages that are blog posts, the result of
@filepath{post-template.html} becomes most of the @tt|{@|contents|}|
variable in the @secref["page-template"]. In other words, the post
template is effectively nested in the page template. (They are two
separate templates so that the page template can also be used for
pages that are not blog post pages.)


@nested[#:style 'inset
@verbatim{
+---------------------------+
| page-template             |
|                           |
|       +---------------+   |
|       | post-template |   |
|       +---------------+   |
|                           |
+---------------------------+
}]

This template does @italic{not} control how a blog post is laid out on
an index page such as @filepath{/index.html} or
@filepath{/tags/@italic{<some-tag>}.html} --- for that, see
@secref["index-template"].

The main purpose of this template is to specify things like Disqus
comments, Tweet and +1 sharing buttons, and older/newer links ---
things that only make sense in the context of pages dedicated to one
blog post.

Anything in the file that looks like @tt|{@variable}| or
@tt|{@|variable|}| is a template variable supplied by Frog. Most of
these should be self-explanatory from their name and from seeing how
they are used in the default template.

@deftv[title string?]{The title of the post.}

@deftv[uri-prefix string?]{Effectively @racket[(or (current-uri-prefix) "")].}

@deftv[uri-path string?]{The path portion of the URI, e.g. @tt{/path/to/file.html}.}

@deftv[full-uri string?]{The full URI, e.g. @tt{http://example.com/path/to/file.html}.}

@deftv[date-8601 string?]{The post date as a string, @tt{"YYYY-MM-DD"}.}

@deftv[date-struct date?]{The post date as a @racket[racket/date] @racket[date] struct.}

@deftv[date string?]{HTML to show the date of the post in a @tt{<time>} element.}

@deftv[tags string?]{HTML to show the tags of the post as links.}

@deftv[date+tags string?]{HTML to show the date and tags of the post.}

@deftv[content string?]{The content of the post.}

@deftv[older-uri (or/c string? #f)]{The URI string of the next older post, if any.}

@deftv[older-title (or/c string? #f)]{The title of the next older post, if any.}

@deftv[newer-uri (or/c string? #f)]{The URI string of the next newer post, if any.}

@deftv[newer-title (or/c string? #f)]{The title of the next newer post, if any.}


@subsection[#:tag "index-template"]{Index template}

The @filepath{index-template.html} template determines how blog posts
are laid out on index pages.

Typically it would be similar to your @secref["post-template"], but
without some "footer" items like comments or previous/next post
buttons.

@nested[#:style 'inset
@verbatim{
+----------------------------+
| page-template              |
|                            |
|       +----------------+   |
|       | index-template |   |
|       +----------------+   |
|                            |
|       +----------------+   |
|       | index-template |   |
|       +----------------+   |
|                            |
|       +----------------+   |
|       | index-template |   |
|       +----------------+   |
|              . . .         |
|                            |
+----------------------------+
}]

Anything in the file that looks like @tt|{@variable}| or
@tt|{@|variable|}| is a template variable supplied by Frog. Most of
these should be self-explanatory from their name and from seeing how
they are used in the default template.

@deftv[title string?]{The title of the post.}

@deftv[uri-prefix string?]{Effectively @racket[(or (current-uri-prefix) "")].}

@deftv[uri-path string?]{The path portion of the URI, e.g. @tt{/path/to/file.html}.}

@deftv[full-uri string?]{The full URI, e.g. @tt{http://example.com/path/to/file.html}.}

@deftv[date-8601 string?]{The post date as a string, @tt{"YYYY-MM-DD"}.}

@deftv[date-struct date?]{The post date as a @tt{date} struct.}

@deftv[date string?]{HTML to show the date of the post in a @tt{<time>} element.}

@deftv[tags string?]{HTML to show the tags of the post as links.}

@deftv[date+tags string?]{HTML to show the date and tags of the post.}

@deftv[content string?]{The content of the post plus a @tt{"More.."} link when needed.}

@deftv[content-only string?]{The content of the post, only.}

@deftv[more? boolean?]{Is the content just a blurb?}


@subsection{Template Example}

Let's say you want to customize the date display format of your posts.
Instead of the default ISO-8601 YYYY-MM-DD format, you want it to be
the default of Racket's @racket[date->string] function. Here is what
you could do in your @secref["index-template"]:

@pre[#:title "index-template.html"]|{
@(local-require racket/date)
<article>
  <header>
    <h2><a href='@|uri-path|'>@|title|</a></h2>
    <p class='date-and-tags'>
      <time datetime='@|date-8601|' pubdate='true'>
        @(date->string date-struct)
      </time>
      :: @|tags|</p>
  </header>
  @|content|
</article>
}|

If you need to require a Racket module in your template, you must use
@racket[local-require]. Plain @racket[require] won't work because the
template is not evaluated at a module level or top level.


@subsection{General Template Tips}

@subsubsection{Use @litchar{|}s to delimit template variables}

Because Racket is extremely liberal in valid characters for
identifiers, you may need to use pipe characters to delimit a template
variable from surrounding text.

For example @litchar|{<p>@foo</p>}| will read as if @racket[foo</p>]
were the identifier, because that is a perfectly valid identifier name
in Racket. Instead you can write: @litchar|{<p>@|foo|</p>}|.

It is probaby simplest to get in the habit of always using
@litchar|{@|variable|}| so you don't need to think about when it's
necessary.

@subsubsection{Use @racket[local-require] in templates}

If you need to require a Racket module in your template, you must use
@racket[local-require]. Plain @racket[require] won't work because the
template is not evaluated at a module level or top level.

@subsubsection{Literal @"@"}

In @secref["reader" #:doc '(lib
"scribblings/scribble/scribble.scrbl")], @litchar{@"@"} has special meaning.
How do you write a literal @tt{@"@"} in your template? Use
@litchar|{@"@"}|. For example if you need to write an email address:
@litchar|{foo@"@"bar.com}| renders as @tt{foo@"@"bar.com}.

Understanding @italic{why} this works may help you remember it:

@itemlist[#:style 'ordered

@item{@litchar{@"@"} means "evaluate the following s-expression as
Racket code".}

@item{@racket["@"] is the Racket expression for the string literal
@tt{@"@"}.}

@item{Therefore @litchar|{@"@"}| evaluates to the string @racket["@"]
and that is the text produced by your template.} ]

@subsubsection{More}

See also @secref["Gotchas" #:doc
'(lib "web-server/scribblings/web-server.scrbl")].


@subsection{Widgets}

In addition to the variables described above for each template, some
predefined functions are available for templates to use: ``widgets''.

Anything a widget can do, you could code directly in the
template. There's no magic. But widgets minimize clutter in the
templates. Plus they make clearer what are the user-specific
parameters (as opposed to putting stuff like @litchar{<!-- CHANGE THIS! -->}
in the template).

See the
@hyperlink["https://github.com/greghendershott/frog/blob/master/example/_src/page-template.html"]{example
page template} and
@hyperlink["https://github.com/greghendershott/frog/blob/master/example/_src/post-template.html"]{example
post template} for usage examples.

If you'd like to add a widget, pull requests are welcome!

@defmodule[frog/widgets]

In your templates, this module is already @racket[require]d.

@; See widget.rkt define/doc forms.
@include-extracted[frog/widgets]


@section{Embedding a blog in an existing site}

If you want to embed the entire blog in an existing site, one way is
to use a subdomain, e.g. @tt{blog.example.com}.

Another way is to embed your blog "under" the existing site's URI path
structure, e.g. @tt{example.com/blog/}. To do so:

@itemlist[#:style 'ordered

@item{In your @secref["post-template"] change URIs from @tt{/} to
@tt{/blog/} as appropriate.}

@item{In @secref["config"] use @racket[(current-uri-prefix "/blog")].
This causes URIs generated by Frog to be prefixed with
@tt{/blog}. (Other URIs --- such as @racket[current-posts-index-uri]
and @racket[current-permalink] --- will automatically be prefixed with
@tt{blog/}, so @italic{don't} change those!)}

]

Using @exec{raco frog @Flag{p}} should open on your blog's index page
at @filepath{/blog/index.html}, automatically. But you can add a
@DFlag{root} flag in case you need to control it more specifically.

@subsection{Tilde Club members}

Will your blog be hosted at @tt{http://example.com/~user}?

In your Frog project directory, create an output directory named
@filepath{~user}:

@pre{$ mkdir \~user  @comment{#use @litchar{\~} in shell for literal @tt{~}}}

Then follow the steps above, including
@racket[(current-output-dir "~user")] and
@racket[(current-uri-prefix "/~user")]} in your @secref["config"], and
adjusting your @secref["page-template"] and so on.


@section[#:tag "parameters"]{Parameters}

You may set these parameters in the @racket[init] function in your
@secref["config"].

@defmodule[frog/params]

(This module is automatically required for you by the
@racketmodname[frog/config] language used in @secref["config"].)

@defparam[current-title v string? #:value "My Awesome Blog"]{The title
of the blog. Used when generating feeds.}

@defparam[current-author v string? #:value "The Default Author"]{The
default author. Used when generating feeds, and provided to
@secref["page-template"] as the template variable @tt|{@author}|.

Note that each post may have @tt{Authors} @secref["metadata"] naming
one or more authors. In that case those author(s) are used for feed
data. Similarly, both @secref["index-template"] and
@secref["post-template"] get an @tt|{@authors}| template variable that
is either post-specific author(s) or the default author here.}

@defparam[current-scheme/host v string? #:value "http://www.example.com"]{
Should @italic{not} end in trailing slash. Used to form full URIs for
various purposes including @tt{urn:}s in feeds and the
@tt|{@full-uri}| variable supplied to @secref["templates"].}

@defparam[current-uri-prefix v (or/c string? #f) #:value #f]{A path
prepended to URIs, including @racket[current-permalink] and
@racket[current-posts-index-uri]. Changing this from the default
@racket["/"] is useful when you want to embed your blog in another web
site.}

@defparam[current-editor v string? #:value "$EDITOR"]{What editor to
launch with @exec{raco frog @DFlag{edit}}. @racket["$EDITOR"] means to
use the @envvar{$EDITOR} environment variable.}

@defparam[current-editor-command v string? #:value "{editor} {filename}"]{
The command to run, in case you need to customize how the editor is
called. For example, @racket["{editor} {filename}"] will do
@racket[(system "$EDITOR 2012-01-01-a-blog-post.md")]. For more
examples, in Frog's source code see the @tt{test} submodule in
@filepath{paths.rkt}.}

@defparam[current-show-tag-counts? v boolean? #:value #t]{When true,
show the count of posts next to each tag in the
@secref["page-template"] variable @tt{tags/feeds}.}

@defparam[current-permalink v string? #:value "/{year}/{month}/{title}.html"]{
Pattern for blog post permalinks. An example of the Jekyll "pretty"
style would be
@racket["/blog/{year}/{month}/{day}/{title}/index.html"].

Another available pattern is @tt{{filename}}, which is the
@tt{this-part} portion of your post's @tt{YYYY-MM-DD-this-part.md}
file name. This is in case you don't like Frog's encoding of your post
title and want to specify it exactly yourself, e.g. to match a
previous blog URI.}

@defparam[current-posts-index-uri v string? #:value "/index.html"]{
URI of the first index page for posts.}

@defparam[current-index-full? v boolean? #:value #t]{When true, index
page items contain full posts -- more than just the portion above
``the jump'' @litchar{<!-- more -->} marker (if any).}

@defparam[current-index-newest-first? v boolean? #:value #t]{When
true, sort index page items newest first.}

@defparam[current-feed-full? v boolean? #:value #t]{When true, feed
items contain full posts -- more than just the portion above ``the
jump'' @litchar{<!-- more -->} marker (if any).}

@defparam[current-posts-per-page n exact-positive-integer? #:value 10]{
The number of posts on each index page.}

@defparam[current-max-feed-items n exact-positive-integer? #:value 20]{
The number of items to include in feeds. Older items in excess of
this will not appear in the feed at all.}

@defparam[current-decorate-feed-uris? v boolean? #:value #t]{When
true, decorate feed URIs with Google Analytics query parameters like
@tt{utm_source}.}

@defparam[current-feed-image-bugs? v boolean? #:value #t]{This
parameter is obsolete and has no effect.}

@defparam[current-source-dir v path-string? #:value "_src"]{
The source directory.

If you deploy to GitHub pages then it is simplest to keep this under
the repo/project top directory.

This may be an absolute or relative path. If relative, it's relative
to the project top directory, i.e. to where @secref["config"] is
located.}

@defparam[current-output-dir v path-string? #:value "."]{
The output directory where generated HTML and other files should go.

If you deploy to e.g. GitHub pages then it is simplest to put the
output in the repo/project top directory, which is why this defaults
to @racket["."]. But you may change it if you prefer to copy the
output files to their final destination.

This may be an absolute or relative path. If relative, it's relative
to the project top directory, i.e. to where @secref["config"] is
located.}

@defparam[current-watch-rate rate exact-positive-integer? #:value 5]{
The number of seconds to wait between each change watching.}

@defparam[current-rebuild?
          rebuild?
          (path? (or/c 'create 'delete 'modify) . -> . boolean?)]{
The procedure to test if a file change during a watch needs a rebuild.

If Frog is run with the @tt{-w} or @tt{--watch} flag, @racket[rebuild?] will be invoked
on every file change (create, delete, modify) in the project. If @racket[rebuild?]
returns @racket[#f], then Frog will not rebuild the project
(for this particular change). Otherwise, Frog will rebuild the project.

By default, @racket[rebuild?] will return @racket[#t] and also make a beep,
except the case where the changed file has an extension either
@tt{html}, @tt{txt}, or @tt{xml} which will return @racket[#f],
as it is likely that the file will be an output.}

@section[#:tag "body-enhancers"]{Body enhancers}

You may call these functions in the @racket[enhance-body] function in
your @secref["config"].

@defmodule[frog/enhance-body]

(This module is automatically required for you by the
@racketmodname[frog/config] language used in @secref["config"].)

@include-extracted[frog/enhance-body]


@section[#:tag "paths"]{Paths}

Although you may use these definitions in your @secref["config"], they
are more likely to be used by third-party @secref["body-enhancers"].

@defmodule[frog/paths]

(This module is automatically required for you by the
@racketmodname[frog/config] language used in @secref["config"].)

These definitions represent locations of certain local directories and
files --- as well as translating between local filesystem paths and
URI paths that will work on the deployed web site. One wrinkle is that
the local file paths might be Windows style, which differs from the
Unix style used by URI paths.

@include-extracted[frog/paths]
