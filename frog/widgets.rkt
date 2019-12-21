#lang at-exp racket/base

(require racket/require
         (for-syntax racket/base
                     racket/syntax)
         (multi-in racket (contract/base format string function))
         scribble/srcdoc
         (for-doc racket/base
                  scribble/manual)
         "private/define-doc.rkt")

;; Functions provided by this module are made available by
;; `render-template` to templates like `page-template.html` and
;; `post-template.html`. You could describe functions here as
;; "widgets": handy bits of functionality to use in templates.
;;
;; Not only does this make the templates simpler, it clarifies the
;; parameterization (the stuff the user is supposed to supply) better
;; than "CHANGE ME!" comments in the templates. Clearly, the
;; user-supplied stuff is what's passed in arguments to the widgets.
;;
;; Widgets should return `string?` or `(listof string?)` -- the latter
;; via @-expressions like `@list{ .... }`. You can write them using
;; normal Racket code like `format`. However for longer bits of HTML
;; and/or JavaScript it can be clearer to use at-exps, as the examples
;; here use.
;;
;; Pull requests welcome! If you've customized your template files
;; with something you'd like to share, please fork, add to this file,
;; and submit a pull request.
;;
;; Note: This file may confuse editors -- do you want Scheme/Racket
;; mode, HTML mode, or JavaScript mode? Well, you probably want each
;; at different times. Just please do your best to follow the
;; indentation style already being used.

(define/doc (older/newer-links [older-uri (or/c #f string?)]
                               [older-title (or/c #f string?)]
                               [newer-uri (or/c #f string?)]
                               [newer-title (or/c #f string?)]
                               [#:for-bs for-bs (or/c 3 4) 3]
                               list?)
  @{Returns HTML either for for a Bootstrap 3 @tt{pager} style
    older/newer navigation, or for a Bootstrap 4 @tt{page-navigation} one,
    depending on the @tt{for-bs} argument which can be either 3 or 4}
  (case for-bs
    [(3)
     @list{
           <ul class="pager">
           @(when newer-uri
             @list{
                   <li class="previous">
                     <a href="@newer-uri">&larr; <em>@|newer-title|</em></a>
                   </li>
                   })
           @(when older-uri
             @list{
                   <li class="next">
                     <a href="@older-uri"><em>@|older-title|</em> &rarr;</a>
                   </li>
                   })
           </ul>
           }]
    [(4)
     @list{
       <div class="row justify-content-center">
         <nav aria-label="Page Navigation">
           <ul class="pagination">
             @(when older-uri
               @list{
                 <li class="page-item">
                   <a class="page-link" href="@|older-uri|"
                      aria-label="Previous">
                     <span aria-hidden="true">&larr; @|older-title|</span>
                   </a>
                 </li>
               })
             @(when newer-uri
               @list{
                 <li class="page-item">
                   <a class="page-link" href="@|newer-uri|"
                      aria-label="Next">
                     <span aria-hidden="true">@|newer-title| &rarr;</span>
                   </a>
                 </li>
               })
           </ul>
         </nav>
       </div>}]))

(define/doc (disqus-comments [short-name string?]
                             [#:identifier identifier (or/c #f string? number?) #f]
                             [#:title title (or/c #f string?) #f]
                             [#:url url (or/c #f string?) #f]
                             [#:category-id category-id (or/c #f string?) #f]
                             list?)
  @{@hyperlink["https://disqus.com/"]{Disqus} comments. Typically used in
    @secref["post-template"]. See
    @hyperlink[
      "https://help.disqus.com/developer/javascript-configuration-variables"
    ]{the documentation} for the usage of each parameter. @racket[#f] in an optional
    parameter indicates the parameter is left undefined.}
  ;; a helper function that converts a Racket value to a JavaScript value
  (define (config val)
    (cond
      [(number? val) (number->string val)]
      [(string? val) (~v val)]
      [(not val) "undefined"]
      [else (error 'disqus-comments "unexpected value: ~a" val)]))
  @list{
        <div id="disqus_thread"></div>
        <script type="text/javascript">
          var disqus_config = function () {
            this.page.identifier = @(config identifier);
            this.page.url = @(config url);
            this.page.title = @(config title);
            this.page.category_id = @(config category-id);
          };
          var disqus_shortname = '@|short-name|';
          (function() {
              var dsq = document.createElement('script');
              dsq.type = 'text/javascript';
              dsq.async = true;
              dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
              dsq.setAttribute('data-timestamp', +new Date());
              (document.head || document.body).appendChild(dsq);
          })();
        </script>
        <noscript>
          Please enable JavaScript to view the
          <a href="https://disqus.com/?ref_noscript">comments powered by Disqus.</a>
        </noscript>
        })

(define/doc (livefyre [site-id string?] list?)
  @{@url["http://livefyre.com"]}
  @list{
        <div id="livefyre-comments"></div>
        <script type="text/javascript" src="//zor.livefyre.com/wjs/v3.0/javascripts/livefyre.js"></script>
        <script type="text/javascript">
        (function () {
            var articleId = fyre.conv.load.makeArticleId(null);
            fyre.conv.load({}, [{
                el: 'livefyre-comments',
                network: "livefyre.com",
                siteId: "@|site-id|",
                articleId: articleId,
                signed: false,
                collectionMeta: {
                    articleId: articleId,
                    url: fyre.conv.load.makeCollectionUrl(),
                }
            }], function() {});
        }());
        </script>})

(define/doc (intense-debate [id-account string?] list?)
  @{@url["https://intensedebate.com/"]}
  @list{
        <script type="text/javascript">
        var idcomments_acct = '@|id-account|';
        var idcomments_post_id;
        var idcomments_post_url;
        </script>
        <span id="IDCommentsPostTitle" style="display:none"></span>
        <script type='text/javascript' src='//www.intensedebate.com/js/genericCommentWrapperV2.js'></script>})

(define/doc (google-universal-analytics [account string?] list?)
  @{For users of ``universal'' analytics (@filepath{analytics.js}).}
  @list{
         <script type="text/javascript">
           (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
           (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
           m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
           })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

           ga('create', '@|account|', 'auto');
           ga('send', 'pageview');
         </script>
         })

(define/doc (google-analytics [account string?]
                              [domain string?]
                              list?)
  @{For users of ``classic'' analytics (@filepath{ga.js}).}
  @list{
         <script type="text/javascript">
           var _gaq = _gaq || [];
           _gaq.push(['_setAccount', '@|account|']);
           _gaq.push(['_setDomainName', '@|domain|']);
           _gaq.push(['_trackPageview']);
           setTimeout(function(){_gaq.push(['_trackEvent', '30_seconds', 'read'])}, 30000); // http://drawingablank.me/blog/fix-your-bounce-rate.html
           (function() {
               var ga = document.createElement('script');
               ga.type = 'text/javascript'; ga.async = true;
               ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
               var s = document.getElementsByTagName('script')[0];
               s.parentNode.insertBefore(ga, s);
           })();
         </script>
         })

(define/doc (google-plus-share-button [full-uri string?] list?)
  @{Typically used in a @secref["post-template"].}
  @list{<script type="text/javascript" src="https://apis.google.com/js/plusone.js"></script>
        <g:plusone size="medium" href="@full-uri"></g:plusone>})

(define/doc (twitter-follow-button [name string?]
                                   [label (or/c #f string?) #f]
                                   list?)
  @{Typically used in a @secref["page-template"].}
  (let ([label (or label (string-append "Follow " name))])
    @list{
          <a href="https://twitter.com/@|name|"
             class="twitter-follow-button"
             data-show-count="false"
             data-lang="en">
            "@|label|"
          </a>
          <script type="text/javascript">
            !function(d,s,id){
                var js,fjs=d.getElementsByTagName(s)[0];
                if(!d.getElementById(id)){
                    js=d.createElement(s);
                    js.id=id;
                    js.src="//platform.twitter.com/widgets.js";
                    fjs.parentNode.insertBefore(js,fjs);
                }
            }(document,"script","twitter-wjs");
          </script>
          }))

(define/doc (twitter-timeline
             [user string?]
             [widget-id string?]
             [#:width width (or/c #f number?) #f]
             [#:height height (or/c #f number?) #f]
             [#:lang lang (or/c #f string?) #f]
             [#:theme data-theme (or/c #f string?) #f]
             [#:link-color data-link-color (or/c #f string?) #f]
             [#:border-color data-border-color (or/c #f string?) #f]
             [#:tweet-limit data-tweet-limit (or/c #f string?) #f]
             [#:chrome data-chrome (or/c #f string?) #f]
             [#:aria-polite data-aria-polite (or/c #f string?) #f]
             [#:related data-related (or/c #f string?) #f]
             list?)
  @{See @url["https://dev.twitter.com/docs/embedded-timelines"]
    for instructions how to create a timeline and get its
    ``widget ID''.}
  ;; Reduce the tedium of translating optional arguments into HTML
  ;; attributes, where #f means no values at all.
  (define-syntax (and/attrs stx)
    (syntax-case stx ()
      [(_ id ...)
       #'(string-join (filter identity
                              (list (and id
                                         (format "~a=\"~a\"" 'id id)) ...)))]))
  (define attrs (and/attrs width
                           height
                           lang
                           data-theme
                           data-link-color
                           data-border-color
                           data-tweet-limit
                           data-chrome
                           data-aria-polite
                           data-related))
  @list{<a class="twitter-timeline" href="https://twitter.com/@|user|"
           data-widget-id="@|widget-id|"
           @|attrs|>
        </a>
        <script>
          !function(d,s,id){
              var js,fjs=d.getElementsByTagName(s)[0];
              if(!d.getElementById(id)){
                  js=d.createElement(s);
                  js.id=id;
                  js.src="//platform.twitter.com/widgets.js";
                  fjs.parentNode.insertBefore(js,fjs);
              }
          }(document,"script","twitter-wjs");
        </script>
        })

(define/doc (twitter-share-button [uri string?] list?)
  @{Typically used in a @secref["post-template"].}
  @list{
        <script type="text/javascript">
          !function(d,s,id){
              var js,fjs=d.getElementsByTagName(s)[0];
              if(!d.getElementById(id)){
                  js=d.createElement(s);
                  js.id=id;
                  js.src="//platform.twitter.com/widgets.js";
                  fjs.parentNode.insertBefore(js,fjs);
              }
          }(document,"script","twitter-wjs");
        </script>
        <a href="https://twitter.com/share"
           class="twitter-share-button"
           data-url="@uri"
           data-dnt="true">
          "Tweet"</a>
          })

(provide gittip-receiving)
(define (gittip-receiving username)
  @list{
        <script data-gittip-username="@|username|" src="//gttp.co/v1.js">
        })

(provide gittip-button)
(define (gittip-button username)
  @list{
        <script data-gittip-username="@|username|"
        data-gittip-widget="button" src="//gttp.co/v1.js"></script>
        })

(define/doc (gist [username string?]
                  [id string?]
                  list?)
  @{Include a @hyperlink["https://gist.github.com/"]{gist.}}
  @list{
        <script src="//gist.github.com/@|username|/@|id|.js"></script>
       })

(define/doc (math-jax [#:src src string?
                       "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js"]
                      [#:config config string? "TeX-AMS-MML_HTMLorMML"]
                      list?)
  @{@itemlist[#:style 'ordered
              @item{Use this in the @tt{<head>} of your @secref["page-template"].}
              @item{In your markdown source files, use @litchar{\\( some math \\)}
                    for inline and @litchar{\\[ some math \\]} for display. Note
                    the @italic{double} backslashes, @litchar{\\}, because in
                    markdown @litchar{\} already has a meaning.}
              @item{You can specify the source URL and configuration options
                    with the @tt{src} & @tt{config} arguments: they default to
                    something reasonable.}]}
  @list{
        <script type="text/javascript" async
                src="@|src|?config=@|config|">
        </script>
       })

 (define/doc (piwik [site string?]
                    [domain string?]
                    list?)
   @{Add Piwik tracking code to your template.}
   @list{
         <!-- Piwik -->
         <script type="text/javascript">
         var _paq = _paq || [];
         /* tracker methods like "setCustomDimension" should be called before "trackPageView" */
         _paq.push(['trackPageView']);
         _paq.push(['enableLinkTracking']);
         (function() {
             var u="//@|domain|/";
             _paq.push(['setTrackerUrl', u+'piwik.php']);
             _paq.push(['setSiteId', '@|site|']);
             var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
             g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'piwik.js'; s.parentNode.insertBefore(g,s);
         })();
         </script>
         <!-- End Piwik Code -->
         })
