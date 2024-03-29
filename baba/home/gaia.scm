(define-module (baba home gaia)
  #:use-module (baba)
  #:use-module (baba home services browsers)
  #:use-module (baba home services elixir)
  #:use-module (baba home services emacs)
  #:use-module (baba home services fonts)
  #:use-module (baba home services shells)
  #:use-module (baba home services terminals)
  #:use-module (baba home services wm)
  #:use-module (baba packages fonts)
  #:use-module (baba packages pingu)
  #:use-module (baba packages security)
  #:use-module (baba packages tex)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services emacs)
  #:use-module (baba home services gnupg)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home-services shellutils)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home-services wm)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (flat packages emacs)
  #:use-module (nongnu packages mozilla)
  #:use-module (srfi srfi-1))

(define gmail-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "[Gmail]/Sent Mail")
    ("drafts"  . "[Gmail]/Drafts")
    ("archive" . "[Gmail]/All Mail")
    ("trash"   . "[Gmail]/Trash")
    ("spam"    . "[Gmail]/Spam")))

(define fastmail-folder-mapping
  '(("inbox" . "INBOX")
    ("sent" . "Sent")
    ("drafts" . "Drafts")
    ("archive" . "Archive")
    ("trash" . "Trash")
    ("spam" . "Spam")))

(define (prep-str sym str)
  (symbol-append sym '- (string->symbol str)))

(define (isync-channel id local remote)
  `((Channel ,(prep-str id local))
    (Near ,(format #f ":~a-local:~a" id local))
    (Far ,(format #f ":~a-remote:~a" id remote))
    ,#~""))

(define (isync-group-with-channels id isync-mapping)
  (append
   (append-map
    (lambda (x) (isync-channel id (car x) (cdr x)))
    isync-mapping)
   `((Group ,(symbol-append id))
     ,@(map
        (lambda (x) (list 'Channel (prep-str id (car x))))
        isync-mapping)
     ,#~"")))

(define (gpg-cmd id host field)
  (string-append "gpg -q --pinentry-mode loopback --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '/machine " host " id " (symbol->string id) "/ {print " field "}'"))

;; Expected authinfo format:
;; machine <host> id <id> login <mail> port 993 password <pwd>
(define (mail-account id host folders-mapping)
  (let ((user-cmd (gpg-cmd id host "$(NF-4)"))
        (pass-cmd (gpg-cmd id host "$NF")))
    `((IMAPAccount ,id)
      (Host ,host)
      (UserCmd ,user-cmd)
      (PassCmd ,pass-cmd)
      (SSLType IMAPS)
      ,#~""
      (IMAPStore ,(symbol-append id '-remote))
      (Account ,id)
      ,#~""
      (MaildirStore ,(symbol-append id '-local))
      (Subfolders Verbatim)
      (Path ,(string-append (getenv "HOME") "/.mail/" (symbol->string id) "/"))
      (Inbox ,(string-append (getenv "HOME") "/.mail/" (symbol->string id) "/inbox"))
      ,#~""
      ,@(isync-group-with-channels id folders-mapping))))

(define default-isync-global-settings
  `((Create Both)
    (Expunge Both)
    (SyncState *)
    ,#~""))


(define (notmuch-move-out-untagged-messages tag)
  "If tag was removed -> move out of the related folder."
  (format #f "for f in $(notmuch search --output=files \
'path:/.*\\/~a/ and not tag:~a' | grep '/~a/'); \
do mv -v $f \
$(echo $f | sed 's;/~a/;/archive/;' | sed 's/,U=[0-9]*:/:/'); done"
          tag tag tag tag))

(define* (notmuch-move-in-tagged-messages
          tag
          #:key (exclude-dir "nothing-will-match-this"))
  (format #f "for f in $(notmuch search --output=files \
'not path:/.*\\/~a/ and tag:~a' | grep -v \"/~a/\"); \
do mv -v $f \
$(echo $f | sed 's;/[[:alnum:]]*/cur/;/~a/cur/;' | sed 's/,U=[0-9]*:/:/'); done"
          tag tag exclude-dir tag))

(define notmuch-move-rules
  (append
   (map notmuch-move-out-untagged-messages '(inbox trash spam))
   (map notmuch-move-in-tagged-messages '(trash spam))
   (list (notmuch-move-in-tagged-messages 'inbox #:exclude-dir "archive"))))

(define notmuch-tag-update
  (list "notmuch tag +inbox -- path:/.*\\/inbox/"
        "notmuch tag +draft -- path:/.*\\/drafts/"
        "notmuch tag +archive -- path:/.*\\/archive/"
        "notmuch tag +sent -- path:/.*\\/sent/"
        "notmuch tag +trash -- path:/.*\\/trash/"
        "notmuch tag +spam -- path:/.*\\/spam/"
        "notmuch tag -inbox -- not path:/.*\\/inbox/ and tag:inbox"
        "notmuch tag -archive -- not path:/.*\\/archive/ and tag:archive"
        "notmuch tag -trash -- not path:/.*\\/trash/ and tag:trash"
        "notmuch tag -spam  -- not path:/.*\\/spam/  and tag:spam"
        ;; Fix gmail case where we have duplicate mails (since there is no proper archive folder)
        "notmuch tag -archive -- tag:inbox and tag:archive"
        ;; Gmail mark sent email as archive
        "notmuch tag -archive -- tag:sent"
        "notmuch tag -unread -new -- tag:replied"))

(use-modules (guix git-download)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:))
;; TODO: move me to packages


;; TODO: make service for mbsync and notmuch
(home-environment
 (packages
  (list htop notmuch bat direnv keychain gnupg firefox

        ;; flatpak
        flatpak
        xdg-desktop-portal
        xdg-desktop-portal-gtk

        xclip

        pingu

        ;; latex
        texlive-base texlive-bin texlive-latex-minted texlive-latex-kvoptions texlive-latex-fvextra texlive-etoolbox
        texlive-latex-fancyvrb texlive-latex-upquote texlive-latex-lineno texlive-latex-ifplatform
        texlive-latex-pdftexcmds texlive-latex-catchfile texlive-generic-etexcmds
        texlive-generic-xstring texlive-latex-framed texlive-latex-float texlive-lm
        texlive-amsfonts texlive-xcolor texlive-latex-colortbl
        texlive-hyperref texlive-titling texlive-microtype
        texlive-latex-hyphenat texlive-latex-ragged2e
        texlive-latex-everysel texlive-latex-subfig
        texlive-caption texlive-latex-geometry texlive-latex-titlesec
        texlive-enumitem))
 (services
  (append emacs-service
          nyxt-service
          stumpwm-service
          home-elixir-service
          home-picom-service
          home-xmonad-service
          (home-xsession-service xmonad-next "xmonad")
          home-zsh-profile
          (list
           (service home-bash-service-type
                    (home-bash-configuration
                     (guix-defaults? #t)))
           (service home-fish-service-type
                    (home-fish-configuration
                     (config
                      (list
                       (local-file (etc-file "/fish/config.fish"))))
                     (abbreviations '(("gco" . "git checkout")
                                      ("gs" . "git status")
                                      ("gsr" . "sudo -E guix system reconfigure")
                                      ("ghr" . "guix home reconfigure")
                                      ("cat" . "bat -pp")))))
           (service home-gnupg-service-type
                    (home-gnupg-configuration
                     (gpg-config
                      (home-gpg-configuration
                       (extra-config
                        ;; Use hardened config from https://github.com/drduh/YubiKey-Guide#harden-configuration=
                        '((personal-cipher-preferences . ("AES256" "AES192" "AES"))
                          (personal-digest-preferences . ("SHA512" "SHA384" "SHA256"))
                          (personal-compress-preferences . ("ZLIB" "BZIP2" "ZIP" "Uncompressed"))
                          (default-preference-list . ("SHA512" "SHA384" "SHA256" "AES256" "AES192" "AES" "ZLIB" "BZIP2" "ZIP" "Uncompressed"))
                          (cert-digest-algo . "SHA512")
                          (s2k-digest-algo . "SHA512")
                          (s2k-cipher-algo . "AES256")
                          (charset . "utf-8")
                          (fixed-list-mode)
                          (no-comments)
                          (no-emit-version)
                          (keyid-format . "0xlong")
                          (list-options . "show-uid-validity")
                          (verify-options . "show-uid-validity")
                          (with-fingerprint)
                          (require-cross-certification)
                          (no-symkey-cache)
                          (use-agent)))))
                     (gpg-agent-config
                      (home-gpg-agent-configuration
                       (ssh-agent? #t)
                       (extra-config
                        `((log-file . ,(string-append
                                        (or (getenv "XDG_LOG_HOME")
                                            (string-append
                                             (getenv "HOME")
                                             "/.local/var/log"))
                                        "/gpg-agent.log"))))))
                     (scdaemon-config
                      (home-scdaemon-configuration
                       (extra-config
                        `((disable-ccid . #t)
                          (reader-port . "Yubico Yubi")
                          (log-file . ,(string-append
                                        (or (getenv "XDG_LOG_HOME")
                                            (string-append
                                             (getenv "HOME")
                                             "/.local/var/log"))
                                        "/scdaemon.log"))))))))
           (service home-git-service-type
                    (home-git-configuration
                     (config
                      `((user
                         ((name . "Bastien Riviere")
                          (email . "me@babariviere.com")
                          (signingKey . "39035CC0B75D1142")))
                        (github
                         ((user . "babariviere")))
                        (remote
                         ((pushDefault . "origin")))
                        (commit
                         ((gpgSign . #t)))
                        (tag
                         ((gpgSign . #t)))))))
           (service home-font-service-type
                    (home-font-configuration
                     (sans-serif (make-font-spec font-abattis-cantarell "Cantarell"))
                     (serif (make-font-spec font-liberation "Times New Roman"))
                     (monospace (make-font-spec font-biosevka "Biosevka"))))
           (simple-service 'setup-flatpak
                           home-environment-variables-service-type
                           `(("XDG_DATA_DIRS" . "$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS")
                             ("PATH" . "$HOME/.local/share/flatpak/exports/bin:$PATH")
                             ("GTK_USE_PORTAL" . "1")))
           (service home-isync-service-type
                    (home-isync-configuration
                     (config
                      (append default-isync-global-settings
                              (mail-account 'prv-fm
                                            "imap.fastmail.com"
                                            fastmail-folder-mapping)
                              ;; (mail-account 'prv-gm
                              ;;               "imap.gmail.com"
                              ;;               gmail-folder-mapping)
                              ))))
           (simple-service 'isync-ensure-mail-dirs
                           home-activation-service-type
                           #~(map mkdir-p '#$(map (lambda (id) (string-append (getenv "HOME") "/.mail/" (symbol->string id))) '(prv-fm))))
           (service home-notmuch-service-type
                    (home-notmuch-configuration
                     (pre-new
                      (list
                       (with-imported-modules
                           '((guix build utils))
                         #~(begin
                             (for-each system '#$notmuch-move-rules)))))
                     (post-new
                      (list
                       (with-imported-modules
                           '((guix build utils))
                         #~(begin
                             (for-each system '#$notmuch-tag-update)))))
                     (config
                      `((user ((name . "Bastien Riviere")
                               (primary_email . "me@babariviere.com")
                               (other_email . ("babathriviere@gmail.com"))))
                        (database ((path . ,(string-append (getenv "HOME") "/.mail"))))
                        (maildir ((synchronize_flags . true)))
                        (search ((exclude_tags . (trash spam deleted))))
                        (new ((tags . new)
                              (ignore . (.mbsyncstate .uidvalidity .mbsyncstate.new .mbsyncstate.journal))))))))
           (service home-mcron-service-type)
           (simple-service 'direnv-config
                           home-xdg-configuration-files-service-type
                           `(("direnv/lib/use_asdf.sh" ,(local-file (string-append %channel-root "/etc/direnv/lib/use_asdf.sh")))))
           (simple-service 'mcron-mail-sync
                           home-mcron-service-type
                           (list #~(job '(next-minute (range 0 60 5))
                                        (lambda ()
                                          (system* "mbsync" "-a")
                                          (system* "notmuch" "new")))))))))
