(define-module (baba packages virtualization)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages attr)
  #:use-module ((gnu packages virtualization) #:prefix v:))

(define-public lxd
  (package
    (inherit v:lxd)
    (version (package-version v:lxd))
    (arguments
     (substitute-keyword-arguments (package-arguments v:lxd)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'install
             (lambda* (#:key inputs outputs import-path #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin-dir
                       (string-append out "/bin/"))
                      (doc-dir
                       (string-append out "/share/doc/lxd-" ,version))
                      (completions-dir
                       (string-append out "/share/bash-completion/completions")))
                 (with-directory-excursion (string-append "src/" import-path)
                   ;; Wrap lxd with run-time dependencies.
                   (wrap-program (string-append bin-dir "lxd")
                     `("PATH" ":" prefix
                       ,(fold (lambda (input paths)
                                (let* ((in (assoc-ref inputs input))
                                       (bin (string-append in "/bin"))
                                       (sbin (string-append in "/sbin")))
                                  (append (filter file-exists?
                                                  (list bin sbin)) paths)))
                              '()
                              '("bash" "acl" "rsync" "tar" "xz" "btrfs-progs"
                                "gzip" "dnsmasq" "squashfs-tools" "iproute2"
                                "criu" "iptables" "attr"))))
                   ;; Remove unwanted binaries.
                   (for-each (lambda (prog)
                               (delete-file (string-append bin-dir prog)))
                             '("deps" "macaroon-identity" "generate"))
                   ;; Install documentation.
                   (for-each (lambda (file)
                               (install-file file doc-dir))
                             (find-files "doc"))
                   ;; Install bash completion.
                   (rename-file "scripts/bash/lxd-client" "scripts/bash/lxd")
                   (install-file "scripts/bash/lxd" completions-dir)))))))))
    (inputs
     (cons (list "attr" attr) (package-inputs v:lxd)))))
