(define-module (baba packages emacs-xyz)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix packages)

  #:use-module (gnu packages emacs-xyz))

(define-public emacs-kaolin-themes
  (package
   (name "emacs-kaolin-themes")
   (version "20211014.318")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://melpa.org/packages/kaolin-themes-"
	   version
	   ".tar"))
     (sha256
      (base32 "19mry7gp8916cfhy55qy7v6fcfgdqpvdskfrwhwia8w0dbzhp726"))))
   (build-system emacs-build-system)
   (propagated-inputs `(("emacs-autothemer" ,emacs-autothemer)))
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
   (license #f))
  )

(define-public emacs-exunit
  (package
   (name "emacs-exunit")
   (version "20210222.1453")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
	   "https://melpa.org/packages/exunit-"
	   version
	   ".el"))
     (sha256
      (base32 "0sbyq992i2l2gxwar53y7gw148ymy6lw5jcgwqmzcb1alwvp8p0g"))))
   (build-system emacs-build-system)
   (propagated-inputs `(("emacs-s" ,emacs-s) ("emacs-f" ,emacs-f)))
   (home-page "http://github.com/ananthakumaran/exunit.el")
   (synopsis "ExUnit test runner")
   (description
    "Provides commands to run ExUnit tests.  The output is properly
syntax highlighted and stacktraces are navigatable
")
   (license #f)))

(define-public emacs-embark-consult
  (package
    (name "emacs-embark-consult")
    (version "20211012.1921")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/embark-consult-"
               version
               ".el"))
        (sha256
          (base32 "1dd4im4mpjplnk5751ha3my1dwyyvaa61g45m5qbb8b4l45mvyci"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-embark" ,emacs-embark) ("emacs-consult" ,emacs-consult)))
    (home-page "https://github.com/oantolin/embark")
    (synopsis "Consult integration for Embark")
    (description
      "This package provides integration between Embark and Consult. To
use it, arrange for it to be loaded once both of those are loaded:

(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))

Some of the functionality here was previously contained in Embark
itself:

- Support for consult-buffer, so that you get the correct actions
for each type of entry in consult-buffer's list.

- Support for consult-line, consult-outline, consult-mark and
consult-global-mark, so that the insert and save actions don't
include a weird unicode character at the start of the line, and so
you can export from them to an occur buffer (where occur-edit-mode
works!).

Just load this package to get the above functionality, no further
configuration is necessary.

Additionally this package contains some functionality that has
never been in Embark: access to Consult preview from auto-updating
Embark Collect buffer that is associated to an active minibuffer
for a Consult command. For information on Consult preview, see
Consult's info manual or its readme on GitHub.

If you always want the minor mode enabled whenever it possible use:

(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

If you don't want the minor mode automatically on and prefer to
trigger the consult previews manually use this instead:

(define-key embark-collect-mode-map (kbd \"C-j\")
  #'consult-preview-at-point)
")
    (license #f)))

(define-public emacs-alchemist
  (package
    (name "emacs-alchemist")
    (version "20180312.1304")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/alchemist-"
               version
               ".tar"))
        (sha256
          (base32 "0nlxc7rzkglyfxgzj0vjsbfj3wmkzl4qby3vb2bg7bzzxfppsvq2"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-elixir-mode" ,emacs-elixir-mode)
        ("emacs-dash" ,emacs-dash)
        ("emacs-company" ,emacs-company)
        ("emacs-pkg-info" ,emacs-pkg-info)
        ("emacs-s" ,emacs-s)))
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

(define-public emacs-sly-repl-ansi-color
  (package
    (name "emacs-sly-repl-ansi-color")
    (version "20171020.1516")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/sly-repl-ansi-color-"
               version
               ".el"))
        (sha256
          (base32 "0z85y22k8r890s6m0hbb6lirf1fs2m5vrf5g1kxmarww1kbqxc8w"))))
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
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/demangle-mode-"
               version
               ".el"))
        (sha256
          (base32 "0kdyvlyq9nqmx50g4x0lkdqc0wc96brbj2l9rbhb3irj23np1jmy"))))
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
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/disaster-"
               version
               ".el"))
        (sha256
          (base32 "1vwwcx02zxxiv9x9dkd2f217w1hz3agqd3am1zla8dlq4wb6l7fr"))))
    (build-system emacs-build-system)
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
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/modern-cpp-font-lock-"
               version
               ".el"))
        (sha256
          (base32 "0s5chssf6118xbl2jz3xi2s20a7npjwips2r03fmfj4jbndwiqwg"))))
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
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/highlight-quoted-"
               version
               ".el"))
        (sha256
          (base32 "12d21lx97fh7hv00n02bfacn62g3dv99176fxdkrf7zkzcnpv75h"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/Fanael/highlight-quoted")
    (synopsis "Highlight Lisp quotes and quoted symbols")
    (description
      "Minor mode proving highlight of Lisp quotes and quoted symbols.
")
    (license #f)))

(define-public emacs-flycheck-inline
  (package
    (name "emacs-flycheck-inline")
    (version "20200808.1019")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/flycheck-inline-"
               version
               ".el"))
        (sha256
          (base32 "1p48ywz36zf1wys2imd8hs0l5abrskbgzyspx2zy881pw9w1nal6"))))
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

(define-public emacs-language-id
  (package
    (name "emacs-language-id")
    (version "20210916.831")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/language-id-"
               version
               ".el"))
        (sha256
          (base32 "1cfh6cg26lm9zrk4fvfrkgx6gzrc8aapabi3f8dpvpqxpl17znpy"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/lassik/emacs-language-id")
    (synopsis "Library to work with programming language identifiers")
    (description
      "language-id is a small, focused library that helps other Emacs
packages identify the programming languages and markup languages
used in Emacs buffers.  The main point is that it contains an
evolving table of language definitions that doesn't need to be
replicated in other packages.

Right now there is only one public function, `language-id-buffer'.
It looks at the major mode and other variables and returns the
language's GitHub Linguist identifier.  We can add support for
other kinds of identifiers if there is demand.

This library does not do any statistical text matching to guess the
language.
")
    (license #f)))

(define-public emacs-format-all
  (package
    (name "emacs-format-all")
    (version "20211011.1029")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/format-all-"
               version
               ".el"))
        (sha256
          (base32 "0nby6sbkc7qvq3ii4mz02bi0azxqwywyygca65rznn8qp6kfmbyr"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-inheritenv" ,emacs-inheritenv)
        ("emacs-language-id" ,emacs-language-id)))
    (home-page "https://github.com/lassik/emacs-format-all-the-code")
    (synopsis "Auto-format C, C++, JS, Python, Ruby and 50 other languages")
    (description
      "Lets you auto-format source code in many languages using the same
command for all languages, instead of learning a different Emacs
package and formatting command for each language.

Just do M-x format-all-buffer and it will try its best to do the\nright thing.  To auto-format code on save, use the minor mode
format-all-mode.  Please see the documentation for that function
for instructions.

Supported languages:

- Angular/Vue (prettier)
- Assembly (asmfmt)
- ATS (atsfmt)
- Awk (gawk)
- Bazel Starlark (buildifier)
- BibTeX (Emacs)
- C/C++/Objective-C (clang-format, astyle)
- C# (clang-format, astyle)
- Cabal (cabal-fmt)
- Clojure/ClojureScript (node-cljfmt)
- CMake (cmake-format)
- Crystal (crystal tool format)
- CSS/Less/SCSS (prettier)
- Cuda (clang-format)
- D (dfmt)
- Dart (dartfmt, dart-format)
- Dhall (dhall format)
- Dockerfile (dockfmt)
- Elixir (mix format)
- Elm (elm-format)
- Emacs Lisp (Emacs)
- F# (fantomas)
- Fish Shell (fish_indent)
- Fortran Free Form (fprettify)
- Gleam (gleam format)
- GLSL (clang-format)
- Go (gofmt, goimports)
- GraphQL (prettier)
- Haskell (brittany, fourmolu, hindent, ormolu, stylish-haskell)
- HTML/XHTML/XML (tidy)
- Java (clang-format, astyle)
- JavaScript/JSON/JSX (prettier, standard)
- Jsonnet (jsonnetfmt)
- Kotlin (ktlint)
- LaTeX (latexindent, auctex)
- Ledger (ledger-mode)
- Lua (lua-fmt, prettier plugin)
- Markdown (prettier)
- Nix (nixpkgs-fmt, nixfmt)
- OCaml (ocp-indent)
- Perl (perltidy)\n- PHP (prettier plugin)
- Protocol Buffers (clang-format)
- PureScript (purty, purs-tidy)
- Python (black, yapf)
- R (styler)
- Racket (raco-fmt)
- Reason (bsrefmt)
- ReScript (rescript)
- Ruby (rubocop, rufo, standardrb)
- Rust (rustfmt)
- Scala (scalafmt)
- Shell script (beautysh, shfmt)
- Snakemake (snakefmt)
- Solidity (prettier plugin)
- SQL (pgformatter, sqlformat)
- Svelte (prettier plugin)
- Swift (swiftformat)
- Terraform (terraform fmt)
- TOML (prettier plugin)
- TypeScript/TSX (prettier)
- V (v fmt)
- Verilog (iStyle)
- YAML (prettier)

You will need to install external programs to do the formatting.
If `format-all-buffer` can't find the right program, it will try to
tell you how to install it.

Many of the external formatters support configuration files in the
source code directory to control their formatting.  Please see the
documentation for each formatter.

New external formatters can be added easily if they can read code
from standard input and format it to standard output.  Feel free to
submit a pull request or ask for help in GitHub issues.
")
    (license #f)))

(define-public emacs-go-guru
  (package
    (name "emacs-go-guru")
    (version "20181012.330")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/go-guru-"
               version
               ".el"))
        (sha256
          (base32 "0106p9jr3mcca1gbvw3gyc6hjsx9i9nsgs0p79jkzvqcvrvppa9w"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-go-mode" ,emacs-go-mode)))
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
    (version "20210930.1225")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/consult-lsp-"
               version
               ".tar"))
        (sha256
          (base32 "1ly8ki9lxa15li3wazs3zzsaidkb5nn4dc2gf5d44szv15f0qa5h"))))
    (build-system emacs-build-system)
    (propagated-inputs
      `(("emacs-lsp-mode" ,emacs-lsp-mode)
        ("emacs-consult" ,emacs-consult)
	("emacs-marginalia" ,emacs-marginalia)
        ("emacs-f" ,emacs-f)))
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
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/nix-update-"
               version
               ".el"))
        (sha256
          (base32 "19rns6kbhsd3wjxcg7v8w6iggriz76slfhvyfd6fdaz06y41xh45"))))
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
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/poetry-"
               version
               ".el"))
        (sha256
          (base32 "1mx02slnidd1j3fph9l2z87a6s6pihz3za27kvn7hdqxxwdd3xk3"))))
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
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/lsp-python-ms-"
               version
               ".el"))
        (sha256
          (base32 "11468wdnfknk93sim6bghzjz86h2q1l7s2wmqr2hssrci74yspv1"))))
    (build-system emacs-build-system)
    (propagated-inputs `(("emacs-lsp-mode" ,emacs-lsp-mode)))
    (home-page "https://github.com/emacs-lsp/lsp-python-ms")
    (synopsis "The lsp-mode client for Microsoft python-language-server")
    (description
      "from https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/
")
    (license #f)))

(define-public emacs-python-mode
  (package
    (name "emacs-python-mode")
    (version "20211013.1620")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://melpa.org/packages/python-mode-"
               version
               ".tar"))
        (sha256
          (base32 "19bi04dmbgmmfdnjn411085dx4z4p5yfsy1a9hk2x1kxq71bw39g"))))
    (build-system emacs-build-system)
    (home-page "https://gitlab.com/groups/python-mode-devs")
    (synopsis "Python major mode")
    (description
      "Includes a minor mode for handling a Python/IPython shell, and can
take advantage of Pymacs when installed.

See documentation in README.org, README.DEVEL.org

Please report bugs at
https://gitlab.com/python-mode-devs/python-mode/issues

available commands are documented in directory \"doc\" as
commands-python-mode.org

As for `py-add-abbrev':
Similar to `add-mode-abbrev', but uses
`py-partial-expression' before point for expansion to
store, not `word'.  Also provides a proposal for new
abbrevs.

Proposal for an abbrev is composed from the downcased
initials of expansion - provided they are of char-class
[:alpha:]

For example code below would be recognised as a
`py-expression' composed by three
py-partial-expressions.

OrderedDict.popitem(last=True)

Putting the curser at the EOL, M-3 M-x py-add-abbrev

would prompt \"op\" for an abbrev to store, as first
`py-partial-expression' beginns with a \"(\", which is
not taken as proposal.
")
    (license #f)))

(define-public emacs-zoom
  (package
    (name "emacs-zoom")
    (version "20201215.1913")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://melpa.org/packages/zoom-" version ".el"))
        (sha256
          (base32 "1br02b3g3fjbl0wzyimqv2zdpv34xmrj92cwqdkxwk61xv4ml0m9"))))
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

