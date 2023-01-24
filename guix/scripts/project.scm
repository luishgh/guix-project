(define-module (guix scripts project)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:autoload   (ice-9 pretty-print) (pretty-print)
  #:use-module (guix store)
  #:use-module (guix channels)
  #:use-module (guix scripts)
  #:use-module (guix describe)
  #:use-module (guix inferior)
  #:export (guix-project))


;;;
;;; Helpers.
;;;

(define-syntax-rule (todo! ...)
  (let* ((current-location (current-source-location)))
    (leave (G_ "~a:~a:~a: not implemented yet!~%")
           (assq-ref current-location 'filename)
           (assq-ref current-location 'line)
           (assq-ref current-location 'column))))


;;;
;;; Main.
;;;

(define-syntax-rule (get-unlocked-channels-path path)
  (string-append path "/channels.scm"))

(define-syntax-rule (get-locked-channels-path path)
  (string-append path "/channels-lock.scm"))

(define-syntax-rule (get-project-package-path path)
  (string-append path "/guix.scm"))

(define-syntax-rule (get-channels-path path)
  (if (file-exists? (get-locked-channels-path path))
      (get-locked-channels-path path)
      (get-unlocked-channels-path path)))

;; NOTE: initially, I don't think we need to support generations
(define-syntax-rule (get-profile-path path)
  (string-append path "/.profile"))

(define* (write-instances-to-port instances port #:optional locked?)
  (let* ((output-list
          (fold
           (lambda (instance lst)
             (let ((instance-channel (channel-instance-channel instance)))
               (append lst
                       (list
                        ;; NOTE: maybe the code below could be simpler
                        ;; Include commit in channel code according to LOCKED?
                        (if locked?
                            (channel->code
                             (channel
                              (name (channel-name instance-channel))
                              (url (channel-url instance-channel))
                              (branch (channel-branch instance-channel))
                              (commit (channel-instance-commit instance))
                              (introduction (channel-introduction instance-channel))
                              (location (channel-location instance-channel))))
                            (channel->code
                             (channel
                              (name (channel-name instance-channel))
                              (url (channel-url instance-channel))
                              (branch (channel-branch instance-channel))
                              (introduction (channel-introduction instance-channel))
                              (location (channel-location instance-channel)))))))))
           '(list)
           instances)))
    (pretty-print output-list port)))

;; NOTE: at first we will update all channels, later we may introduce selection
(define (update-channels path)
  (let* ((unlocked-channels-path (get-unlocked-channels-path path))
         (channels-path (get-locked-channels-path path))
         (unlocked-channels (load* unlocked-channels-path (make-user-module '((guix channels)))))
         (current-channels (load* channels-path (make-user-module '((guix channels))))))
    (call-with-output-file channels-path
      (lambda (port)
        (with-store store
                    (write-instances-to-port
                     (latest-channel-instances store unlocked-channels #:current-channels current-channels)
                     port
                     #t))))))

(define* (init-project path #:optional no-lock?)
  "Create a channels.scm at PATH. Also create a channels-lock.scm, unless NO-LOCK? is #t."
  (let ((unlocked-channels-path (get-unlocked-channels-path path)))
    (call-with-output-file unlocked-channels-path
      (lambda (port)
        (with-store store
                    (write-instances-to-port
                     (latest-channel-instances store %default-channels)
                     port)))))
  (unless no-lock?
    (lock-project path)))

(define (lock-project path)
  "Create lock file for project in PATH. Can also be used to write lock file without checking the current lock."
  (let* ((unlocked-channels-path (get-unlocked-channels-path path))
         (locked-channels-path (get-locked-channels-path path))
         (unlocked-channels (load* unlocked-channels-path (make-user-module '((guix channels))))))
    (call-with-output-file locked-channels-path
      (lambda (port)
        (with-store store
                    (write-instances-to-port
                     (latest-channel-instances store unlocked-channels)
                     port
                     #t))))))

;; TODO: support additional arguments, support -r option from guix shell
(define (project-shell path)
  (let* ((channels-path (get-channels-path path))
         ;; FIXME: specifying --root option from guix shell gives an
         ;; error for some reason
         ;; (profile-path (get-profile-path path))
         )
    (system* "guix" "time-machine" "-C" channels-path "--" "shell"
             ;; " -r " profile-path
             )))

(define (build-project path)
  (let ((channels-path (get-channels-path path))
        (package-path (get-project-package-path path)))
    (if (file-exists? package-path)
        (system* "guix" "time-machine" "-C" channels-path "--" "build" "-f" package-path)
        (leave (G_ "guix.scm not found in current project~%")))))

(define (guix-project* opts)
  "Run the 'guix project' command on OPTS, an alist resulting for command-line
option processing with 'parse-command-line'."
  (let ((action (assoc-ref opts 'action))
        (path (assoc-ref opts 'path)))
    (match action
      ('update (update-channels path))
      ('init (init-project path))
      ('lock (lock-project path))
      ('shell (project-shell path))
      ('build (build-project path))
      (_ (leave (G_ "~a: missing action~%") action)))))


;;;
;;; CLI.
;;;

;; TODO: update this according to new commands
(define (show-subcommands)
  (display (G_ "Available commands:\n"))
  (display (G_ "
    update [PATH]         update channels lock file
    init   [PATH]         create new project
    lock   [PATH]         create lock file for existing project
    shell  [PATH]         run `guix shell` inside project
    build  [PATH]         build the project's guix.scm")))

(define (show-flags)
  (display (G_ "
  -h, --help             display this help and exit")))

(define (show-help)
  (display (G_ "Usage: guix project COMMAND [OPTION]...
Management facilities for Guix projects.\n"))
  (newline)
  (show-subcommands)
  (newline)
  (show-flags)
  (newline))

(define %options
  ;; Specification of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda _
                  (show-help)
                  (exit 0)))
        (option '(#\p "path") #t #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'path arg result)
                         rest)))))

(define %default-options
  ;; Alist of default option values.
  `((path . ,(getcwd))))

(define-command (guix-project . args)
  (synopsis "manages projects")

  (define (parse-sub-command arg result)
    ;; Parse sub-command ARG and augment RESULT accordingly.
    (if (assoc-ref result 'action)
        (alist-cons 'argument arg result)
        (let ((action (string->symbol arg)))
          (case action
            ((init
              lock
              update
              shell
              build)
             (alist-cons 'action action result))
            (else (leave (G_ "~a: unknown action~%") action))))))

  (define (parse-options args)
    (parse-command-line args %options
                        (list %default-options)
                        #:argument-handler parse-sub-command))

  (guix-project* (parse-options args)))
