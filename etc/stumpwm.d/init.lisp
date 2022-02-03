(in-package :stumpwm)

(require :asdf)

;; Modules

(dolist (module '("ttf-fonts" "stumptray" "battery-portable" "cpu" "mem" "net" "notify" "wifi"))
  (add-to-load-path (format nil "~a/.guix-home/profile/share/common-lisp/sbcl/stumpwm-~a/"
                            (uiop:getenv "HOME")
                            module)))

;; Slynk

(require :slynk)

(defvar *slynk-port* 4004)

(defcommand start-slynk () ()
  (slynk:create-server
   :port *slynk-port*
   :dont-close t))

(defcommand stop-slynk () ()
  (slynk:stop-server *slynk-port*))

(ignore-errors (start-slynk))

(add-hook *restart-hook* 'stop-slynk)

;; Fonts

(load-module "ttf-fonts")
(setf xft:*font-dirs* '("~/.local/share/fonts")
      clx-truetype:+font-cache-filename+ "~/.local/share/fonts/font-cache.sexp")
(xft:cache-fonts)

(set-font (make-instance 'xft:font :family "MonoLisa" :subfamily "Regular" :size 10))

;; Misc

(setq *ignore-wm-inc-hints* t)

(setf *mouse-focus-policy* :click)

(setq *message-window-gravity* :top
      *message-window-padding* 10
      *message-window-y-padding* 10
      *suppress-frame-indicator* t)

(setq *input-window-gravity* :top)

;; Modeline

(setf *group-format* "%t"
      *window-format* "%n: %c"
      *time-modeline-string* "%F %H:%M")

(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")
(load-module "net")
(load-module "wifi")

(setf cpu::*cpu-modeline-fmt*        "%c"
      cpu::*cpu-usage-modeline-fmt*  "CPU: ^[~A~2D%^]"
      mem::*mem-modeline-fmt*        "%a%p"
      wifi:*wifi-modeline-fmt*       "%e%P"
      wifi:*use-colors*              nil
      *hidden-window-color*          "^**")

(setf *mode-line-border-width* 0
      *mode-line-pad-x* 10
      *mode-line-timeout* 2
      *screen-mode-line-format* "[^B%n^b] %W ^> %I | %l | %C | %M | %B | %d  %T")

(dolist (h (screen-heads (current-screen)))
  (enable-mode-line (current-screen) h t))

(load-module "stumptray")
(stumptray:stumptray)

;; Notifications

(asdf:load-system :notify)
(load-module "notify")
(notify:notify-server-on)

(setf notify:*notify-server-title-color* "^20"
      notify:*notify-server-body-color* "^70")

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
(define-key *root-map* (kbd "C-w") "pull-from-windowlist")

(define-key *top-map* (kbd "s-End") "exec xlock -mode swarm")
