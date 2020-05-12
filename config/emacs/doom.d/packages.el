;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! systemd :pin "51c148e09a")
(package! dart-mode :pin "04fcd649f1")
(package! org-fancy-priorities :pin "819bb993b7")
(package! graphql :pin "e2b309689f")
(package! polymode :pin "2a61fb6d3e")
(package! mmm-mode :pin "ac9a741b96")
(package! graphql-mode :pin "7c37aee28b")
(package! protobuf-mode
  :recipe (:host github :repo "emacsmirror/protobuf-mode" :files (:defaults "*"))
  :pin "94b7bd7e8b")
(package! org-roam-server :recipe (:host github :repo "org-roam/org-roam-server" :files ("*")))
