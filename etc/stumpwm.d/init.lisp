(in-package :stumpwm)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (require 'asdf)
;;   (asdf:load-system :slynk))

;; (asdf:load-system :slynk)
(require :asdf)

;; Slynk

(require :slynk)
(defcommand stump-slynk-server () ()
  (slynk:create-server :port 4004
		       :dont-close t))
(stump-slynk-server)

;; Fonts

(add-to-load-path "~/.guix-home/profile/share/common-lisp/sbcl/stumpwm-ttf-fonts/")
(load-module "ttf-fonts")
(setf xft:*font-dirs* '("~/.local/share/fonts")
      clx-truetype:+font-cache-filename+ "~/.local/share/fonts/font-cache.sexp")
(xft:cache-fonts)

(set-font (make-instance 'xft:font :family "MonoLisa" :subfamily "Regular" :size 10))

;; Misc

(setf *mouse-focus-policy* :click)

(setq *message-window-gravity* :top
      *message-window-padding* 10
      *message-window-y-padding* 10
      *suppress-frame-indicator* t)

(setq *input-window-gravity* :top)

(setf *screen-mode-line-format* "[^B%n^b] %d %W")

(dolist (h (screen-heads (current-screen)))
  (enable-mode-line (current-screen) h t))

;; Bindings / Commands

(set-prefix-key (kbd "C-i"))

(defcommand firefox () ()
  "Start Firefox or switch to it."
  (run-or-raise "firefox" '(:class "(Nightly|Firefox)")))
(define-key *root-map* (kbd "f") "firefox")
(define-key *root-map* (kbd "C-a") "fselect")

(define-key *root-map* (kbd "c") "exec alacritty")
(define-key *root-map* (kbd "C-c") "exec alacritty")

(define-key *root-map* (kbd "w") "windowlist")
(define-key *root-map* (kbd "C-w") "windowlist")

(define-key *top-map* (kbd "s-End") "exec xlock -mode swarm")
