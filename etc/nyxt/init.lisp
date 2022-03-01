;; TODO: use https://github.com/aartaka/nx-search-engines
(defvar *search-engines*
  (list '("spg" "https://startpage.com/do/search?query=~a" "https://startpage.com"))
  "Define my search engines. The last one will be used as default.")

(define-configuration browser
  ((session-restore-prompt :always-restore)))

(define-configuration buffer
  ((default-modes (append '(emacs-mode) %slot-default%))
   (search-engines (append (mapcar (lambda (engine) (apply 'make-search-engine engine))
                                   *search-engines*)
                           %slot-default%))))
