;; Local Variables:
;; sly-port: 4004
;; End:

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
      *time-modeline-string* "%Y-%m-%e %H:%M")

(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")
(load-module "net")
(load-module "wifi")

(setf cpu::*cpu-modeline-fmt*        "%c"
      ;; cpu::*cpu-usage-modeline-fmt*  "CPU: ^[~A~2D%^]"
      mem::*mem-modeline-fmt*        "%a%p"
      wifi:*wifi-modeline-fmt*       "%e%P"
      wifi:*use-colors*              nil
      *hidden-window-color*          "^**")

(defun get-current-volume ()
  (parse-integer (run-shell-command "pamixer --get-volume" t)))

(defun volume-muted-p ()
  (string-equal (string-trim '(#\newline) (run-shell-command "pamixer --get-mute" t)) "true"))

(defvar *current-volume* (get-current-volume))
(defvar *volume-muted* (volume-muted-p))

(defvar *volume-timer*
  (run-with-timer
   0 60
   (lambda () (setq *current-volume* (get-current-volume)
                    *volume-muted* (volume-muted-p)))))

(defun format-volume (ml)
  (declare (ignore ml))
  (if *volume-muted*
      "Volume: muted"
      (format nil "Volume: ~A%" *current-volume*)))

(add-screen-mode-line-formatter #\v #'format-volume)

(setf *mode-line-border-width* 0
      *mode-line-pad-x* 10
      *mode-line-timeout* 2
      *screen-mode-line-format*
      (list "[^B%n^b] %W ^> %I | %l | %C | %M | %B | %v | %d  %T"))

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

(defcommand emacsclient () ()
  "Start emacsclient or switch to it."
  (run-or-raise "emacsclient -c" '(:class "Emacs")))
(define-key *root-map* (kbd "e") "emacsclient")

(defcommand volume-up () ()
  "Raise volume."
  (setq *current-volume* (min (+ *current-volume* 3) 100))
  (run-shell-command "pamixer --increase 3"))

(defcommand volume-down () ()
  "Lower volume."
  (setq *current-volume* (max (- *current-volume* 3) 0))
  (run-shell-command "pamixer --decrease 3"))

(defcommand volume-toggle-mute () ()
  "Toggle mute."
  (setq *volume-muted* (not *volume-muted*))
  (run-shell-command "pamixer --toggle-mute"))

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "volume-toggle-mute")
