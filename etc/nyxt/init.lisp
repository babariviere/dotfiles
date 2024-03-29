;; Local Variables:
;; sly-port: 4006
;; End:

(require :slynk)

;; TODO: use https://github.com/aartaka/nx-search-engines
(defvar *search-engines*
  (list '("spg" "https://startpage.com/do/search?query=~a" "https://startpage.com"))
  "Define my search engines. The last one will be used as default.")

(defvar *amber/keymap* (make-keymap "amber/keymap"))
(define-key *amber/keymap*
  "M-j" 'nyxt/web-mode:follow-hint-new-buffer)

(define-mode amber-keymap-mode ()
  "Mode to define custom key bindings from *amber/keymap*."
  ((keymap-scheme (keymap:make-scheme
                   scheme:emacs *amber/keymap*))))

(define-configuration browser
  ((session-restore-prompt :always-restore)))

(define-configuration buffer
  ((default-modes (append '(amber-keymap-mode emacs-mode) %slot-default%))
   (search-engines (mapcar (lambda (engine) (apply 'make-search-engine engine))
                           *search-engines*))))

(define-command-global start-slynk (&optional (slynk-port *swank-port*))
  "Start a Slynk server that can be connected to, for instance, in
    Emacs via SLY.

    Warning: This allows Nyxt to be controlled remotely, that is, to execute
    arbitrary code with the privileges of the user running Nyxt.  Make sure
    you understand the security risks associated with this before running
    this command."
  (slynk:create-server :port slynk-port :dont-close t)
  (echo "Slynk server started at port ~a" slynk-port))
