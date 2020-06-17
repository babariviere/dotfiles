;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! systemd)
(package! dart-mode)
(package! org-fancy-priorities)
(package! graphql)
(package! polymode)
(package! mmm-mode)
(package! graphql-mode)
(package! protobuf-mode
  :recipe (:host github :repo "emacsmirror/protobuf-mode" :files (:defaults "*")))
(package! org-roam-server :recipe (:host github :repo "org-roam/org-roam-server" :files ("*")))
(package! po-mode)
(when (featurep! :lang haskell)
  (package! ormolu))
