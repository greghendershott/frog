#lang at-exp racket

(provide (all-defined-out))

;; Functions provided by this module are made available by
;; `render-template` totemplates like `page-template.html` and
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

(define (google-analytics account domain)
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

(define (twitter-follow-button name [label #f])
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
  
(define (twitter-share-button uri)
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

(define (google-plus-share-button full-uri)
  @list{<script type="text/javascript" src="https://apis.google.com/js/plusone.js"></script>
        <g:plusone size="medium" href="@full-uri"></g:plusone>})

(define (disqus-comments short-name)
  @list{
        <script type="text/javascript">
          var disqus_shortname = '@|short-name|';
          (function() {
              var dsq = document.createElement('script');
              dsq.type = 'text/javascript';
              dsq.async = true;
              dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
              (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
          })();
        </script>
        <div id="disqus_thread"></div>
        })

(define (older/newer-links older-uri older-title newer-uri newer-title)
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
      })

;; See https://dev.twitter.com/docs/embedded-timelines for instructions
;; how to create a timeline and get its "widget ID".
(define (twitter-timeline user
                          widget-id
                          #:width [width #f]
                          #:height [height #f]
                          #:lang [lang #f]
                          #:theme [data-theme #f]
                          #:link-color [data-link-color #f]
                          #:border-color [data-border-color #f]
                          #:tweet-limit [data-tweet-limit #f]
                          #:chrome [data-chrome #f]
                          #:aria-polite [data-aria-polite #f]
                          #:related [data-related #f])
  (define opt-attrs (and/attrs width
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
           data-widget-id="@|widget-id|" @|opt-attrs|></a>
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

;; Reduce the tedium of translating optional arguments into HTML
;; attributes, where #f means no values at all.
(define-syntax (and/attrs stx)
  (syntax-case stx ()
    [(_ id ...)
     #'(string-join (filter identity
                            (list (and
                                   id (format "~a=\"~a\"" 'id id)) ...)))]))
