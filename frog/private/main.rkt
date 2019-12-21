#lang racket/base

(require racket/require
         (multi-in racket (cmdline
                           contract/base
                           contract/region
                           help
                           file
                           function
                           match
                           set
                           vector))
         threading
         (multi-in web-server (dispatchers/dispatch
                               servlet-env))
         (except-in xml xexpr->string)
         (only-in find-parent-dir find-parent-containing)
         "bodies-page.rkt"
         (prefix-in user-frog.rkt: "../config/private/load.rkt")
         "new-post.rkt"
         "non-posts.rkt"
         "upgrade/old-config.rkt"
         "params.rkt"
         "paths.rkt"
         "post-struct.rkt"
         "posts.rkt"
         "serialize-posts.rkt"
         "stale.rkt"
         "tags.rkt"
         (except-in "util.rkt" path-get-extension)
         "verbosity.rkt"
         "version.rkt"
         "watch-dir.rkt")

(provide main
         serve)

(module+ test
  (require rackunit))

(define (main)
  (when (eq? 'windows (system-type 'os))
    (file-stream-buffer-mode (current-output-port) 'line)
    (file-stream-buffer-mode (current-error-port) 'line))
  (printf "Frog ~a\n" (frog-version))
  (parameterize ([top (find-frog-root)])
    (when (vector-member "--init" (current-command-line-arguments))
      (init-project)
      (exit 0))
    ;; When the user has supplied help args (or no args at all) then
    ;; we want to just show the help and exit. (We specifically don't
    ;; want to deal with a missing frog.rkt config file.) But doing so
    ;; is awkward because `command-line` doesn't expose a `show-help`
    ;; function we can just call directly and exit. So, mutate
    ;; current-command-line-arguments as necessary to prevent
    ;; attempting certain non-help commands below.
    (define help?
      (cond [(or (equal? #() (current-command-line-arguments))
                 (ormap (curryr vector-member (current-command-line-arguments))
                        '("-h" "--help")))
             (current-command-line-arguments #("--help"))
             #t]
            [(vector-member "--doc" (current-command-line-arguments))
             (current-command-line-arguments #("--doc"))
             #t]
            [else #f]))
    (unless help?
      (maybe-frogrc->frog.rkt (top))
      (user-frog.rkt:load (top))
      (user-frog.rkt:init))
    (define watch? #f)
    (define port 3000)
    (define root
      ;; Default the server root to be the number of parent dirs
      ;; above (www-path) as there are dirs in current-uri-prefix.
      (let* ([prefix (or (current-uri-prefix) "/")]
             [depth (sub1 (length (explode-path prefix)))])
        (simplify-path (apply build-path (list* (www-path)
                                                (build-list depth (λ _ 'up)))))))
    (define edit-new-post? (make-parameter #f))
    (command-line
     #:program "raco frog"
     #:once-each
     [("--doc")
      (""
       "Browse full documentation for Frog.")
      (help "T:frog")]
     [("--init")
      (""
       "Initialize current directory as a new Frog project, creating"
       "default files as a starting point.")
      (void)] ;; handled above
     [("--edit")
      (""
       "Opens the file created by -n or -N in `current-editor` in frog.rkt"
       "Supply this flag before one of those flags.")
      (edit-new-post? #t)]
     #:multi
     [("-n" "--new" "--new-markdown") title
      (""
       "Create a .md file for a new post based on today's date and <title>.")
      (new-post title 'markdown (edit-new-post?))]
     [("-N" "--new-scribble") title
      (""
       "Create a .scrbl file for a new post based on today's date and <title>.")
      (new-post title 'scribble (edit-new-post?))]
     [("-b" "--build")
      (""
       "Generate files.")
      (build)]
     [("-c" "--clean")
      (""
       "Delete generated files.")
      (clean)]
     #:once-each
     [("-w" "--watch")
      (""
       "(Experimental!) Supply this flag before -s/--serve or -p/--preview."
       "Watch for changed files, and regenerate the project."
       "(You'll need to refresh the browser yourself.)"
       "Customize which files should trigger the regeneration"
       "by using current-rebuild?")
      (set! watch? #t)]
     [("--port") number
      (""
       "The port number for -s/--serve or -p/--preview."
       "Supply this flag before one of those flags."
       "Default: 3000.")
      (set! port (string->number number))]
     [("--root") path
      (""
       "The root directory for -s/--serve or -p/--preview."
       "Supply this flag before one of those flags."
       "If frog.rkt says (current-uri-prefix \"/path/to/site/blog\"),
       try using `--root /path/to/site`."
       "Default: One less than the number of dirs in current-uri-prefix,
        above current-output-dir.")
      (set! root path)]
     #:once-any
     [("-s" "--serve")
      (""
       "Run a local web server.")
      (serve #:launch-browser? #f
             #:watch? watch?
             #:watch-callback watch-callback
             #:watch-path (src-path)
             #:port port
             #:root root)]
     [("-p" "--preview")
      (""
       "Run a local web server and start your browser on blog home page.")
      (serve #:launch-browser? #t
             #:watch? watch?
             #:watch-callback watch-callback
             #:watch-path (src-path)
             #:port port
             #:root root)]
     #:once-any
     [("-S" "--silent") "Silent. Put first."
      (current-verbosity -1)]
     [("-v" "--verbose") "Verbose. Put first."
      (current-verbosity 1)
      (prn1 "Verbose mode")]
     [("-V" "--very-verbose") "Very verbose. Put first."
      (current-verbosity 2)
      (prn2 "Very verbose mode")])))

(define (find-frog-root)
  (define (try file)
    (find-parent-containing (current-directory) file))
  (or (let ([x (or (try "frog.rkt")
                   (try ".frogrc"))])
        (and x (simplify-path x)))
      (current-directory)))

(define (init-project)
  (define (copy path)
    (define from (~> (build-path example path) simplify-path))
    (define to   (~> (build-path (top) path) simplify-path))
    (prn0 "~a" to)
    (make-directories-if-needed to)
    (copy-directory/files from to))
  (prn0 "Creating files in ~a:" (build-path (top)))
  (copy "frog.rkt")
  (copy "_src/About.md")
  (copy "_src/page-template.html")
  (copy "_src/post-template.html")
  (copy "_src/posts/2012-01-01-a-2012-blog-post.md")
  (copy "css/")
  (copy "js/")
  (copy "img/")
  (prn0 "Project ready. Try `raco frog -bp` to build and preview."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build)
  ;; [0] Build `old-posts`, `new-posts`, and `sorted-posts`.
  ;;
  ;; We use a file that's a serialization of (hash/c path? post?)
  ;; where the hash key is the source file pathname. The file is the
  ;; state from our last build. Read this into `old-posts`. Will be an
  ;; empty hash if file doesn't exist (yet).
  (define old-posts (deserialize-posts))
  ;; Now let's traverse the source files to build `new-posts`.
  (define (on-file path type new-posts)
    (define-values (_ name __) (split-path path))
    (when (and (eq? type 'file)
               (regexp-match? post-file-px name))
      (define old-post (hash-ref old-posts path #f))
      (cond [;; If the post exists in `old-posts` and its modified
             ;; time isn't older than the source file, just reuse the
             ;; old parse of the source -- but reset the `older` and
             ;; `newer` fields to #f.
             (and old-post
                  (>= (post-modified old-post)
                      (file-or-directory-modify-seconds path)))
             (prn2 "Needn't read: ~a" (abs->rel/src path))
             (hash-set! new-posts path
                        (struct-copy post old-post
                                     [older #f]
                                     [newer #f]))]
            ;; Else do the relatively expensive work of read-post
            ;; (parsing for markdown + Pygments + doc links, or
            ;; Scribble evaluation).
            [else (define new-post (read-post path))
                  (when new-post
                    (hash-set! new-posts path new-post))]))
      new-posts)
  (define new-posts
    (fold-files* on-file (make-hash) (src/posts-path) #f))
  ;; Make hashes of tags -> posts, old and new.
  (define old-tags (posts->tags old-posts))
  (define new-tags (posts->tags new-posts))
  (all-tags new-tags)
  ;; Make a list of the post source paths, sorted by post date.
  ;; (This is needed by index pages, which list posts ordered by date.)
  (define sorted-posts-src-paths
    (map post-src-path (sort (filter linked-post? (hash-values new-posts))
                             (negate post-date<=?))))
  ;; Update the `older` and `newer` fields for each post in new-posts.
  ;; These act like pointers in a doubly-linked list, except they are
  ;; source file paths not pointers, and #f is equivalent to NULL.
  ;; (Because this mutates new-posts, later when we compare it to
  ;; old-posts, any changes will count... therefore trigger rebuilding
  ;; those posts... therefore ensure previous/next post links in
  ;; post-template.html get updated.)
  (let loop ([xs sorted-posts-src-paths])
    (match xs
      [(list* this next more)
       (hash-update! new-posts this (lambda (v) (struct-copy post v [older next])))
       (hash-update! new-posts next (lambda (v) (struct-copy post v [newer this])))
       (loop (cons next more))]
      [_ (void)]))   ;older & newer were intialized to #f; leave as-is

  ;; Great, now we're ready to use `old-post` and `new-posts` to
  ;; determine what needs to be built -- and more importantly, what
  ;; does NOT need to be built.
  ;;
  ;; [1] Posts
  ;;
  ;; (a) Delete obsolete output files. When post-dest-path has
  ;; changed, delete the old post-dest-path.
  (for ([(path old-post) (in-hash old-posts)])
    (define new-post (hash-ref new-posts path #f))
    (unless (and new-post
                 (equal? (post-dest-path new-post) (post-dest-path old-post)))
      (delete-file* (post-dest-path old-post) abs->rel/www)))
  ;; (b) Write output files, as necessary.
  (for ([(path post) (in-hash new-posts)])
    (define file-modified file-or-directory-modify-seconds) ;brevity
    (when (or ;; Target doesn't exist
              (not (file-exists? (post-dest-path post)))
              ;; Target is older than its dependents
              (let ([secs (file-modified (post-dest-path post))])
                (or (< secs (post-modified post))
                    (< secs (file-modified (post-template.html)))
                    (< secs (file-modified (page-template.html)))))
              ;; Post isn't in old-posts or differs from version in
              ;; old-posts (includes case of older/newer links
              ;; changing).
              (not (equal? post (hash-ref old-posts path #f))))
      (write-post-page post
                       (hash-ref new-posts (post-older post) #f)
                       (hash-ref new-posts (post-newer post) #f))))

  ;; [2] Tags: Output to index pages and feed files
  ;;
  ;; (a) Delete obsolete output files, for tags no longer in use.
  (for ([tag (in-list (hash-keys old-tags))])
    (unless (hash-has-key? new-tags tag)
      (delete-file* (index-path-for-tag tag) abs->rel/www)
      ;; ^ FIXME. Also delete multi-page index files
      (delete-file* (atom-path-for-tag tag) abs->rel/www)
      (delete-file* (rss-path-for-tag tag) abs->rel/www)
      (prn0 "Deleted index pages and feeds for tag no longer used: `~a`" tag)))
  ;; (b) Write output files, as necessary.
  (define (stale/templates? tag)
    ;; Any of the index pages or feed files older than template files
    ;; used to make them?
    (for/or ([f (in-list (list index-path-for-tag atom-path-for-tag rss-path-for-tag))])
      (stale? (f tag)
              (page-template.html) (post-template.html) (index-template.html))))
  (define (stale/posts? posts)
    ;; Any post using this tag has changed? (Because post contents
    ;; like the blurb, date, etc. show up on index pages and feeds).
    (for/or ([post (in-list posts)])
      (not (equal? post (hash-ref old-posts (post-src-path post) #f)))))
  (define (post-has-tag? tag post)
    (or (equal? tag "all")
        (member tag (post-tags post))))
  (for ([(tag paths) (in-hash new-tags)])
    (define posts-this-tag
      (filter values
              (for/list ([src (in-list sorted-posts-src-paths)])
                (define post (hash-ref new-posts src #f))
                (and post (post-has-tag? tag post) post))))
    (when (or (not (equal? paths (hash-ref old-tags tag #f)))
              (stale/posts? posts-this-tag)
              (stale/templates? tag))
      (write-stuff-for-tag tag posts-this-tag)))

  ;; [3] Save `new-posts` (to be used as the `old-posts` for our next
  ;;     build).
  (serialize-posts new-posts)

  ;; [4] Non-post pages.
  (define non-post-pages (build-non-post-pages))

  ;; [5] sitemap.txt, populated from new-posts and non-post-pages.
  ;;     (Generating this is cheap, so just always do it.)
  (prn1 "Generating sitemap.txt")
  (with-output-to-file (build-path (www-path) "sitemap.txt") #:exists 'replace
    (thunk
     (for-each displayln
               (map full-uri
                    (append (map post-uri-path (filter linked-post?
                                                       (hash-values new-posts)))
                            non-post-pages))))))

;;----------------------------------------------------------------------------

;; Like `fold-files`, but if `start-path` is not #f and does not exist,
;; this returns `init-val` rather than abending with "path
;; disappeared" error.
(define (fold-files* proc init-val [start-path #f] [follow-links? #f])
  (cond [(and start-path (not (directory-exists? start-path))) init-val]
        [else (fold-files proc init-val start-path follow-links?)]))

(define (post-date<=? a b)
  (let ([a (post-date a)]
        [b (post-date b)])
    (cond [(current-index-newest-first?) (string<=? a b)]
          [else                          (string<=? b a)])))

(define (linked-post? p)
  (not (member "UNLINKED" (post-tags p))))

;; Given a (hash/c path? post?) return a (hash/c string? (set/c
;; path?)), where the string is a tag. In other words this returns a
;; hash that maps a tag to all the posts that are marked with the tag.
(define/contract (posts->tags posts)
  ((hash/c path? post?) . ->  . (hash/c string? (set/c path?)))
  (define h (make-hash))
  (for ([(path post) (in-hash posts)])
    (when (linked-post? post)
      (for ([tag (in-list (cons "all" (post-tags post)))])
        (unless (equal? tag "")
          (hash-update! h tag (lambda (v) (set-add v path)) (set))))))
  h)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (clean)
  (clean-post-output-files)
  (clean-non-post-output-files)
  (clean-tag-output-files)
  (clean-serialized-posts)
  (user-frog.rkt:clean))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (watch-callback path change-type)
  (when ((current-rebuild?) path change-type)
    (with-handlers ([exn:fail? (compose1 displayln exn->string)])
      (build))))

(define (serve #:launch-browser? launch-browser?
               #:watch? watch?
               #:watch-callback [watch-callback #f]
               #:watch-path [watch-path #f]
               #:port port
               #:root root)
  (define watcher-thread
    (cond [watch? (unless (and watch-path watch-callback)
                    (error 'frog "No watch callback given"))
                  (watch-directory (build-path watch-path)
                                   '(file)
                                   watch-callback
                                   #:rate (current-watch-rate))]
          [else (thread (thunk (sync never-evt)))]))
  (when launch-browser?
    (ensure-external-browser-preference))
  (serve/servlet (lambda (_) (next-dispatcher))
                 #:servlet-path (canonical-uri (current-posts-index-uri))
                 #:extra-files-paths (list root)
                 #:port port
                 #:listen-ip #f
                 #:launch-browser? launch-browser?)
  (kill-thread watcher-thread))

(define (ensure-external-browser-preference)
  ;; `serve/servlet` uses the `send-url` from `net/sendurl`, which
  ;; (unlike the `send-url` from `external/browser`) doesn't prompt
  ;; the user if no external browser preference is set. This can
  ;; matter on some Linux distributions, e.g. Ubuntu which needs
  ;; xdg-open or sensible-browser, but `net/sendurl` uses neither and
  ;; doesn't even prompt the user. So check for this case here, and if
  ;; no browser preference set yet, ask the user, like the `send-url`
  ;; from `external/browser` would do.
  (when (eq? 'unix (system-type 'os))
    (unless (get-preference 'external-browser)
      ((dynamic-require 'browser/external
                        'update-browser-preference) #f))))
