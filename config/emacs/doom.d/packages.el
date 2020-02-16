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
(package! org-roam
  :recipe (:host github :repo "jethrokuan/org-roam" :branch "develop")
  :pin "5eb1a87123")
