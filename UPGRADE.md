If you had started to use Frog before version 0.3:

1. Thank you!

2. You'll need to move some things form `navbar.md`, `footer.md`, and
`homehead.md` to a new template file, `_src/page-template.html`.

3. You'll need to move some things from `.frogrc` to
`_src/page-template.html` and to another new template file,
`_src/post-template.html`.

Starting with version 0.3, Frog uses `web-server/templates` to do more
of the work when it comes to layout and appearance, instead of
building the HTML itself using `.frogrc` variables.

# `navbar.md`

> **NOTE:** This is no longer a special file. You should delete it,
> otherwise Frog will treat it like any other `.md` file in/under `_src`
> and create a `navbar.html` file from it!

In `page-template.html`, create navbar `<li>` items using the
`@nav-item` function. The example `page-template.html` defines All
Posts (home) and About items, you can add/change:

```html
    <!-- A standard Twitter Bootstrap nav bar -->
    <div class="navbar navbar-inverse">
      <div class="navbar-inner">
        <div class="container">
          <ul class="nav">
            <!-- Notice we can define Racket functions. Here's a
                 helper to check if a nav bar item is for this page, as
                 indiated by `uri-path` -->
            @define[(nav-item uri label [a-attribs ""])]{
              @list{
                <li@(when (string-ci=? uri uri-path) " class=\"active\"")>
                  <a href="@|uri|"@|a-attribs|>@|label|</a>
                </li>
              }}
            @nav-item["/index.html" "All Posts" " class=\"brand\""]
            @nav-item["/About.html" "About"]
          </ul>
        </div>
      </div>
    </div>
```

# `footer.md`

> **NOTE:** This is no longer a special file. You should delete it,
> otherwise Frog will treat it like any other `.md` file in/under `_src`
> and create a `footer.html` file from it!

In `page-template.html`, simply edit the `<footer>` section:

```html
      <footer>
        <hr />
        <p>Site generated
        by <a href="https://github.com/greghendershott/frog">Frog</a>,
        the <strong>fr</strong>ozen bl<strong>og</strong> tool.</p>
        <p>Using <a href="http://twitter.github.com/bootstrap/index.html">Bootstrap</a>.</p>
        <p><em>Your legal notice here</em>.</p>
      </footer>
```

# `homehead.md`

> **NOTE:** This is no longer a special file. You should delete it,
> otherwise Frog will treat it like any other `.md` file in/under `_src`
> and create a `homehead.html` file from it!

In `page-template.html`, edit the section of the example file that
checks whether the `@uri-path` template variable is `string-ci=?` to
`"/index.html"`:

```html
      <div class="row-fluid">
        <!-- Main column -->
        <div id="content" class="span9">
          <!-- To put text only on the home page, check for `uri-path`
               being "/index.html" -->
          @(when (string-ci=? uri-path "/index.html")
            @list{
            <h1>Welcome</h1>
            <p>Here is some text that only goes on the home page,
              because we matched on <code>uri-path</code>
              being <code>/index.html</code>.</p>
            }
          )
      ....
```

# `.frogrc` settings

## `bootstrap-responsive?` and `bootstrap-minified?`

In `page-template.html`, set the style sheets yourself:

```html
    <!-- CSS -->
    <link rel="stylesheet" type="text/css" href="/css/bootstrap.min.css">
    <link rel="stylesheet" type="text/css" href="/css/bootstrap-responsive.min.css">
```

## `older/newer-buttons`

In `post-template.html`, set them as you like:

```html
  </footer>
    ...
    <!-- Older/Newer post buttons -->
    <ul class="pager">
      <li class="previous @if[newer-uri "" "disabled"]">
        <a href="@|newer-uri|">&larr; <em>@|newer-title|</em></a>
      </li>
      <li class="next @if[older-uri "" "disabled"]">
        <a href="@|older-uri|"><em>@|older-title|</em> &rarr;</a>
      </li>
    </ul>
  </footer>
```

## `favicon`

In `page-template.html`, set it:

```html
    <link rel="icon" href="/favicon.ico">
```

## `google-analytics-account` and `google-analytics-domain`

In `page-template.html`, set these:

```html
    <script type="text/javascript">
      var _gaq = _gaq || [];
      _gaq.push(['_setAccount', 'UA-xxxxxx']);       <!-- SET THIS -->
      _gaq.push(['_setDomainName', 'example.com']);  <!-- SET THIS -->
      _gaq.push(['_trackPageview']);
      ....
```

## `disqus-shortname`

In `post-template.html`, set it:

```html
    <!-- Disqus comments -->
    <script type="text/javascript">
      var disqus_shortname = 'SHORTNAME';  <!-- CHANGE THIS -->
      ....
```

## `twitter-name` (for Follow button)

In `page-template.html`, change it:

```html
        <!-- Right column -->
        <div id="right-sidebar" class="span3">
          <!-- `tags/feeds` is a list of tags and feeds -->
          @|tags/feeds|
          <!-- Example of a Twitter follow button -->
          <a href="https://twitter.com/racketlang"
             class="twitter-follow-button"
             data-show-count="false"
             data-lang="en">
            "Follow RacketLang"
          </a>
```

# Conclusion

Of course, the point of Frog leveraging templates is that now you have
more freedom to choose what services you use, and how and where they
appear. I'm sorry for the disruption making this change, but I think
this will let people accomplish more.
