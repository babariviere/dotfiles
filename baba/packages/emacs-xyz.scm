(define-module (baba packages emacs-xyz)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-sly-repl-ansi-color
  (package
    (name "emacs-sly-repl-ansi-color")
    (version "20171020.1516")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PuercoPop/sly-repl-ansi-color.git")
             (commit "b9cd52d1cf927bf7e08582d46ab0bcf1d4fb5048")))
       (sha256
        (base32 "0fgcn6bwgz8yyjza07kfi86siargvpq4kp4j20hs6b67ckxjxx0x"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-sly" ,emacs-sly)))
    (home-page "https://github.com/PuercoPop/sly-repl-ansi-color")
    (synopsis "Add ANSI colors support to the sly mrepl.")
    (description
     "This package adds ANSI colors support to the sly mrepl. It is a port of
slime-repl-ansi-color.el which was originally written by Max Mikhanosha.
")
    (license #f)))

(define-public emacs-demangle-mode
  (package
    (name "emacs-demangle-mode")
    (version "20210822.2210")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/liblit/demangle-mode.git")
             (commit "04f545adab066708d6151f13da65aaf519f8ac4e")))
       (sha256
        (base32 "0jkw3n7fp0fd1pmaxszncx2lvb5g3hszsm2n6axbvy8waqxydz2w"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/liblit/demangle-mode")
    (synopsis "Automatically demangle C++, D, and Rust symbols")
    (description
     "`demangle-mode' is an Emacs minor mode that automatically demangles
C++, D, and Rust symbols.  For example, in this mode:

- the mangled C++ symbol `_ZNSaIcED2Ev' displays as
  `std::allocator<char>::~allocator()'

- the mangled C++ symbol `_GLOBAL__I_abc' displays as `global
  constructors keyed to abc'

- the mangled D symbol `_D4test3fooAa' displays as `test.foo'

- the mangled Rust symbol `_RNvNtNtCs1234_7mycrate3foo3bar3baz'
  displays as `mycrate::foo::bar::baz'

See <https://github.com/liblit/demangle-mode#readme> for additional
documentation: usage suggestions, background & motivation,
compatibility notes, and known issues & design limitations.

Visit <https://github.com/liblit/demangle-mode/issues> or use
command `demangle-mode-submit-bug-report' to report bugs or offer
suggestions for improvement.
")
    (license #f)))

(define-public emacs-disaster
  (package
    (name "emacs-disaster")
    (version "20171016.2152")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jart/disaster.git")
             (commit "10a785facc60d89d78e0d5177985ab1af1741bb4")))
       (sha256
        (base32 "0iz43jdkh5qdllqdchliys84gn9bpj6688rpc4jnycp64141m6cx"))))
    (build-system emacs-build-system)
    (arguments '(#:include '("^disaster.el$") #:exclude '()))
    (home-page "https://github.com/jart/disaster")
    (synopsis "Disassemble C/C++ code under cursor in Emacs")
    (description
     "![Screenshot](http://i.imgur.com/kMoN1m6.png)

Disaster lets you press `C-c d` to see the compiled assembly code for the
C/C++ file you're currently editing. It even jumps to and highlights the
line of assembly corresponding to the line beneath your cursor.

It works by creating a `.o` file using make (if you have a Makefile) or the
default system compiler. It then runs that file through objdump to generate
the human-readable assembly.

; Installation:

Make sure to place `disaster.el` somewhere in the load-path and add the
following lines to your `.emacs` file to enable the `C-c d` shortcut to
invoke `disaster':

    (add-to-list 'load-path \"/PATH/TO/DISASTER\")
    (require 'disaster)
    (define-key c-mode-base-map (kbd \"C-c d\") 'disaster)
")
    (license #f)))

(define-public emacs-modern-cpp-font-lock
  (package
    (name "emacs-modern-cpp-font-lock")
    (version "20210405.1155")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ludwigpacifici/modern-cpp-font-lock.git")
             (commit "43c6b68ff58fccdf9deef11674a172e4eaa8455c")))
       (sha256
        (base32 "1wp90225g1pm8sr3hlknwr2q29iq90wds4n6vf1ls4cf2b71nnq2"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/ludwigpacifici/modern-cpp-font-lock")
    (synopsis "Font-locking for \"Modern C++\"")
    (description
     "Syntax highlighting support for \"Modern C++\" - until C++20 and
Technical Specification. This package aims to provide a simple
highlight of the C++ language without dependency.

It is recommended to use it in addition with the c++-mode major
mode for extra highlighting (user defined types, functions, etc.)
and indentation.

Melpa: [M-x] package-install [RET] modern-cpp-font-lock [RET]
In your init Emacs file add:
    (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
or:
    (modern-c++-font-lock-global-mode t)

For the current buffer, the minor-mode can be turned on/off via the
command:
    [M-x] modern-c++-font-lock-mode [RET]

More documentation:
https://github.com/ludwigpacifici/modern-cpp-font-lock/blob/master/README.md

Feedback is welcome!
")
    (license #f)))

(define-public emacs-highlight-quoted
  (package
    (name "emacs-highlight-quoted")
    (version "20140916.1822")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fanael/highlight-quoted.git")
             (commit "24103478158cd19fbcfb4339a3f1fa1f054f1469")))
       (sha256
        (base32 "1gq8inxfni9zgz2brqm4nlswgr8b0spq15wr532xfrgr456g10ks"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/highlight-quoted")
    (synopsis "Highlight Lisp quotes and quoted symbols")
    (description
     "Minor mode proving highlight of Lisp quotes and quoted symbols.
")
    (license #f)))

(define-public emacs-alchemist
  (package
    (name "emacs-alchemist")
    (version "20180312.1304")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tonini/alchemist.el.git")
             (commit "6f99367511ae209f8fe2c990779764bbb4ccb6ed")))
       (sha256
        (base32 "12f95rwxs11sqf1w9pnf6cxc2lh2jz4nqkq33p8b5yamnl8cq9kg"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-elixir-mode" ,emacs-elixir-mode)
       ("emacs-dash" ,emacs-dash)
       ("emacs-company" ,emacs-company)
       ("emacs-pkg-info" ,emacs-pkg-info)
       ("emacs-s" ,emacs-s)))
    (arguments
     '(#:include
       '("^[^/]+.el$"
         "^[^/]+.el.in$"
         "^dir$"
         "^[^/]+.info$"
         "^[^/]+.texi$"
         "^[^/]+.texinfo$"
         "^doc/dir$"
         "^doc/[^/]+.info$"
         "^doc/[^/]+.texi$"
         "^doc/[^/]+.texinfo$"
         "^[^/]+.exs$"
         "^alchemist-server$")
       #:exclude
       '("^.dir-locals.el$"
         "^test.el$"
         "^tests.el$"
         "^[^/]+-test.el$"
         "^[^/]+-tests.el$")))
    (home-page "http://www.github.com/tonini/alchemist.el")
    (synopsis "Elixir tooling integration into Emacs")
    (description
     "
 What Does Alchemist Do For You?

   Alchemist brings you all the Elixir tooling and power inside your Emacs editor.

 Alchemist comes with a bunch of features, which are:

   * Mix integration
   * Compile & Execution of Elixir code
   * Inline code evaluation
   * Inline macro expanding
   * Documentation lookup
   * Definition lookup
   * Powerful IEx integration
   * Smart code completion
   * Elixir project management
   * Phoenix support
")
    (license #f)))

(define-public emacs-exunit
  (package
    (name "emacs-exunit")
    (version "20210222.1453")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ananthakumaran/exunit.el.git")
             (commit "5bb115f3270cfe29d36286da889f0ee5bba03cfd")))
       (sha256
        (base32 "0xz7vnj2wjzih0rm1bsf1ynjy46wmm0aifa9g8362d8570anmkj5"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-s" ,emacs-s) ("emacs-f" ,emacs-f)))
    (home-page "http://github.com/ananthakumaran/exunit.el")
    (synopsis "ExUnit test runner")
    (description
     "Provides commands to run ExUnit tests.  The output is properly
syntax highlighted and stacktraces are navigatable
")
    (license #f)))

(define-public emacs-flycheck-inline
  (package
    (name "emacs-flycheck-inline")
    (version "20200808.1019")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/flycheck/flycheck-inline.git")
               (commit "8e00b4c5951a9515a450a14aefe92e9f6ddcfbde")))
        (sha256
          (base32 "1s505lk5rdc3p40w5g4cpzviaksclvfdisl457gpwjpjv0m7fwd4"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-flycheck" ,emacs-flycheck)))
    (home-page "https://github.com/flycheck/flycheck-inline")
    (synopsis "Display Flycheck errors inline")
    (description
      "Provide an error display function to show Flycheck errors inline, directly
below their location in the buffer.

# Setup

Enable the local minor mode for all flycheck-mode buffers:

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))
")
    (license #f)))

(define-public emacs-go-guru
  (package
    (name "emacs-go-guru")
    (version "20181012.330")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dominikh/go-mode.el.git")
             (commit "5bd8efab64352dccf31dbc99c4fc96d3b985ef27")))
       (sha256
        (base32 "0j430sd72pkh00773yqrg1jllli9yccdf645yxrxsf3n3k95s603"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-go-mode" ,emacs-go-mode)))
    (arguments '(#:include '("^go-guru.el$") #:exclude '()))
    (home-page "unspecified")
    (synopsis "Integration of the Go 'guru' analysis tool into Emacs.")
    (description
     "To enable the Go guru in Emacs, use this command to download,
build, and install the tool in $GOROOT/bin:

    $ go get golang.org/x/tools/cmd/guru

Verify that the tool is on your $PATH:

    $ guru -help
    Go source code guru.
    Usage: guru [flags] <mode> <position>
    ...

Then copy this file to a directory on your `load-path',
and add this to your ~/.emacs:

    (require 'go-guru)

Inside a buffer of Go source code, select an expression of
interest, and type `C-c C-o d' (for \"describe\") or run one of the
other go-guru-xxx commands.  If you use `menu-bar-mode', these
commands are available from the Guru menu.

To enable identifier highlighting mode in a Go source buffer, use:

    (go-guru-hl-identifier-mode)
\nTo enable it automatically in all Go source buffers,
add this to your ~/.emacs:

    (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

See http://golang.org/s/using-guru for more information about guru.
")
    (license #f)))

(define-public emacs-consult-lsp
  (package
    (name "emacs-consult-lsp")
    (version "20211104.1506")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gagbo/consult-lsp.git")
             (commit "aaa9a31bc82259d743186c53d8b01f043c6fec13")))
       (sha256
        (base32 "1d4l930gwfp2syxkm129lxbvrwcqv3rz2qzb3m18v6aklk0si2db"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-lsp-mode" ,emacs-lsp-mode)
       ("emacs-consult" ,emacs-consult)
       ("emacs-f" ,emacs-f)
       ("emacs-marginalia" ,emacs-marginalia)))
    (home-page "https://github.com/gagbo/consult-lsp")
    (synopsis "LSP-mode Consult integration")
    (description
     "Provides LSP-mode related commands for consult

The commands are autoloaded so you don't need to require anything to make them
available. Just use M-x and go!

;; Diagnostics
M-x consult-lsp-diagnostics provides a view of all diagnostics in the current
workspace (or all workspaces if passed a prefix argument).

You can use prefixes to filter diagnostics per severity, and
previewing/selecting a candidate will go to it directly.

;; Symbols
M-x consult-lsp-symbols provides a selection/narrowing command to search
and go to any arbitrary symbol in the workspace (or all workspaces if
passed a prefix argument).

You can use prefixes as well to filter candidates per type, and
previewing/selecting a candidate will go to it.

;; File symbols
M-x consult-lsp-file-symbols provides a selection/narrowing command to search
and go to any arbitrary symbol in the selected buffer (like imenu)

;; Contributions
Possible contributions for ameliorations include:
- using a custom format for the propertized candidates
  This should be done with :type 'function custom variables probably.
- checking the properties in LSP to see how diagnostic-sources should be used
- checking the properties in LSP to see how symbol-sources should be used
")
    (license #f)))

(define-public emacs-nix-update
  (package
    (name "emacs-nix-update")
    (version "20190124.1935")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jwiegley/nix-update-el.git")
             (commit "fc6c39c2da3fcfa62f4796816c084a6389c8b6e7")))
       (sha256
        (base32 "01cpl4w49m5dfkx7l8g1q183s341iz6vkjv2q4fbx93avd7msjgi"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/jwiegley/nix-update-el")
    (synopsis "Update \"fetch\" blocks in .nix expressions")
    (description
     "Bind nix-update-fetch to a key (I use `C-. u'), and then you can very
easily update the rev/sha of a fetchgit declaration.
")
    (license #f)))

(define-public emacs-poetry
  (package
    (name "emacs-poetry")
    (version "20211016.834")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/galaunay/poetry.el.git")
             (commit "5b9ef569d629d79820e73b5380e54e443ba90616")))
       (sha256
        (base32 "1zk5ps9ax456400aa16hcjygpzq4mlhdnhv1d31yy4agqjql927h"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-transient" ,emacs-transient) ("emacs-pyvenv" ,emacs-pyvenv)))
    (home-page "https://github.com/galaunay/poetry.el")
    (synopsis "Interface to Poetry")
    (description
     "This package offers an interface to poetry (https://poetry.eustace.io/),
a Python dependency management and packaging command line tool.

Poetry.el uses transient to provide a magit-like interface. The
entry point is simply: `poetry'

Poetry.el also provides a global minor mode that automatically
activates the associated virtualenv when visiting a poetry project.
You can activate this feature with `poetry-tracking-mode'.
")
    (license #f)))

(define-public emacs-lsp-python-ms
  (package
    (name "emacs-lsp-python-ms")
    (version "20210513.1019")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emacs-lsp/lsp-python-ms.git")
             (commit "4061bc25aaddacb2fb848df08dd8bbbc12975814")))
       (sha256
        (base32 "1ds19l8gvilc6bkqh7s1b5f1v4p79xkdjrq3kln0zawqsszr2crs"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-lsp-mode" ,emacs-lsp-mode)))
    (home-page "https://github.com/emacs-lsp/lsp-python-ms")
    (synopsis "The lsp-mode client for Microsoft python-language-server")
    (description
     "from https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/
")
    (license #f)))

(define-public emacs-kaolin-themes
  (package
    (name "emacs-kaolin-themes")
    (version "20211023.1347")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ogdenwebb/emacs-kaolin-themes.git")
             (commit "ea6394619219b6d54b843836e3a6b2e7d8aaecec")))
       (sha256
        (base32 "1awgh70g7k7bjjga8kd6rfd8k3fqzkswgxyf4qgrsjci80is8jcn"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-autothemer" ,emacs-autothemer)))
    (arguments
     '(#:include
       '("^[^/]+.el$"
         "^[^/]+.el.in$"
         "^dir$"
         "^[^/]+.info$"
         "^[^/]+.texi$"
         "^[^/]+.texinfo$"
         "^doc/dir$"
         "^doc/[^/]+.info$"
         "^doc/[^/]+.texi$"
         "^doc/[^/]+.texinfo$"
         "^themes/[^/]+.el$")
       #:exclude
       '("^.dir-locals.el$"
         "^test.el$"
         "^tests.el$"
         "^[^/]+-test.el$"
         "^[^/]+-tests.el$")))
    (home-page "https://github.com/ogdenwebb/emacs-kaolin-themes")
    (synopsis "A set of eye pleasing themes")
    (description
     "
Kaolin is a set of eye pleasing themes for GNU Emacs
With support a large number of modes and external packages.
Kaolin themes are based on the pallete that was originally
inspired by Sierra.vim with adding some extra colors.

-------  This package includes the following themes  -------

 * kaolin-dark - a dark jade variant inspired by Sierra.vim.
 * kaolin-light - light variant of the original kaolin-dark.
 * kaolin-aurora - Kaolin meets polar lights.
 * kaolin-bubblegum - Kaolin colorful theme with dark blue background.
 * kaolin-eclipse - a dark purple variant.
 * kaolin-galaxy - bright theme based on one of the Sebastian Andaur arts.
 * kaolin-mono-dark - almost monochrome dark green Kaolin theme.
 * kaolin-mono-light - light variant of monochrome theme.
 * kaolin-ocean - dark blue variant.
 * kaolin-shiva - theme with autumn colors and melanzane background.
 * kaolin-temple - dark brown background with syntax highlighting based on orange and cyan shades.
 * kaolin-valley-dark - colorful Kaolin theme with brown background.
 * kaolin-valley-light - light version of kaolin-valley-dark theme.


-------  Configuration example  -------

 (require 'kaolin-themes)
 (load-theme 'kaolin-dark t)

 ;; Apply treemacs customization for Kaolin themes, requires the all-the-icons package.
 (kaolin-treemacs-theme)

 ;; Or if you have use-package installed
 (use-package kaolin-themes
   :config
   (load-theme 'kaolin-dark t)
   (kaolin-treemacs-theme))

 ;;  Custom theme settings

 ;; The following set to t by default
 (setq kaolin-themes-bold t       ; If nil, disable the bold style.
       kaolin-themes-italic t     ; If nil, disable the italic style.
       kaolin-themes-underline t) ; If nil, disable the underline style.

-------  Some extra theme features, disabled by default  -------

 ;; If t, use the wave underline style instead of regular underline.
 (setq kaolin-themes-underline-wave t)

 ;; When t, will display colored hl-line style
 (setq kaolin-themes-hl-line-colored t)

")
    (license #f)))

(define-public emacs-zoom
  (package
    (name "emacs-zoom")
    (version "20201215.1913")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cyrus-and/zoom.git")
             (commit "c7beef180bc4037404e2d56b9ab9b7c76d1713a0")))
       (sha256
        (base32 "0vzrg5vfb4rnripdxlnl2gs6mm85ysiwnn8rclsaljrmk5xrp5ls"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/cyrus-and/zoom")
    (synopsis "Fixed and automatic balanced window layout")
    (description
     "This minor mode takes care of managing the window sizes by enforcing a fixed
and automatic balanced layout where the currently selected window is resized
according to `zoom-size' which can be an absolute value in lines/columns, a
ratio between the selected window and frame size or even a custom callback.
")
    (license #f)))

(define-public emacs-inkpot-theme
  (package
   (name "emacs-inkpot-theme")
   (version "20211101.558")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.com/ideasman42/emacs-inkpot-theme.git")
           (commit "1ca71416869e7515a9c2587b35f21a11921686f3")))
     (sha256
      (base32 "0pl2hpcy9165np17gwa9qhqxb43kwm0z746pxcga7rfg6apy6krc"))))
   (build-system emacs-build-system)
   (home-page "https://gitlab.com/ideasman42/emacs-inkpot-theme")
   (synopsis "A port of vim's inkpot theme")
   (description
    "This file is based on Per Vognsen's port of the original vim theme.
")
   (license #f)))
