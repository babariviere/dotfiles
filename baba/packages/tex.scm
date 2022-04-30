(define-module (baba packages tex)
  #:use-module (gnu packages tex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define simple-texlive-package (@@ (gnu packages tex) simple-texlive-package))

(define-public texlive-latex-minted
  (let ((template
         (simple-texlive-package
          "texlive-latex-minted"
          (list "doc/latex/minted/" "source/latex/minted/" "tex/latex/minted/")
          (base32 "13cjsjb3b04n9arwp46ayk8fcicylxq5g1864cpxl1lxjxh1yi0l"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ #t)
          "latex/minted")))
      (home-page "https://ctan.org/macros/latex/contrib/minted")
      (synopsis "Highlighted source code for LaTeX")
      (description
       "The package that facilitates expressive syntax highlighting in LaTeX using the
powerful Pygments library.  The package also provides options to customize the
highlighted source code output using fancyvrb.")
      (license license:lppl1.3+))))

(define-public texlive-latex-fvextra
  (let ((template
         (simple-texlive-package
          "texlive-latex-fvextra"
          (list "doc/latex/fvextra/" "source/latex/fvextra/" "tex/latex/fvextra/")
          (base32 "0nawx1fh55yhqspy5jgss2qmwpqmikfrg7628smk931rph9nq0aa"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ #t)
          "latex/fvextra")))
      (home-page "https://ctan.org/macros/latex/contrib/fvextra")
      (synopsis "Extensions and patches for fancyvrb")
      (description
       "fvextra provides several extensions to fancyvrb, including automatic line
breaking and improved math mode.  It also patches some fancyvrb internals.
Parts of fvextra were originally developed as part of pythontex and minted.")
      (license license:lppl1.3+))))

(define-public texlive-latex-lineno
  (let ((template
         (simple-texlive-package
          "texlive-latex-lineno"
          (list "doc/latex/lineno/" "source/latex/lineno/" "tex/latex/lineno/")
          (base32 "1xf8ljgcj411yqmng89wc49rqfz19j95yqqpnb35dj3qc1chvm2a"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ #t)
          "latex/lineno")))
      (home-page "https://ctan.org/macros/latex/contrib/lineno")
      (synopsis "Line numbers on paragraphs")
      (description
       "Adds line numbers to selected paragraphs with reference possible through the
LaTeX \\ref and \\pageref cross reference mechanism.  Line numbering may be
extended to footnote lines, using the fnlineno package.")
      (license license:lppl))))

(define-public texlive-latex-catchfile
  (let ((template
         (simple-texlive-package
          "texlive-latex-catchfile"
          (list "doc/latex/catchfile/"
                "source/latex/catchfile/"
                "tex/generic/catchfile/")
          (base32 "1dpxy64hs0bjp8d2dmikflc995vazf7fi6z92w51fnj2fidgl8gx"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ #t)
          "latex/catchfile")))
      (home-page "https://ctan.org/macros/latex/contrib/catchfile")
      (synopsis "Catch an external file into a macro")
      (description
       "This package catches the contents of a file and puts it in a macro.  It requires
e-TeX.  Both LaTeX and plain TeX are supported.")
      (license license:lppl1.3+))))

(define-public texlive-generic-xstring
  (package
    (inherit
     (simple-texlive-package
      "texlive-generic-xstring"
      (list "doc/generic/xstring/" "tex/generic/xstring/")
      (base32 "1azpq855kq1l4686bjp8haxim5c8wycz1b6lcg5q7x8kb4g9sppn")
      #:trivial?
      #t))
    (home-page "https://ctan.org/macros/generic/xstring")
    (synopsis "String manipulation for (La)TeX")
    (description
     "The package provides macros for manipulating strings -- testing a string's
contents, extracting substrings, substitution of substrings and providing
numbers such as string length, position of, or number of recurrences of, a
substring.  The package works equally in Plain TeX and LaTeX (though e-TeX is
always required).  The strings to be processed may contain (expandable) macros.")
    (license license:lppl1.3c)))

(define-public texlive-lm
  (package
    (inherit
     (simple-texlive-package
      "texlive-lm"
      (list "doc/fonts/lm/"
            "fonts/afm/public/lm/"
            "fonts/enc/dvips/lm/"
            "fonts/map/dvipdfm/lm/"
            "fonts/map/dvips/lm/"
            "fonts/opentype/public/lm/"
            "fonts/tfm/public/lm/"
            "fonts/type1/public/lm/"
            "tex/latex/lm/")
      (base32 "0yyk0dr4yms82mwy4dc03zf5igyhgcb65icdah042rk23rlpxygv")
      #:trivial?
      #t))
    (home-page "https://ctan.org/fonts/lm")
    (synopsis "Latin modern fonts in outline formats")
    (description
     "The Latin Modern family of fonts consists of 72 text fonts and 20 mathematics
fonts, and is based on the Computer Modern fonts released into public domain by
AMS (copyright (c) 1997 AMS).  The lm font set contains a lot of additional
characters, mainly accented ones, but not exclusively.  There is one set of
fonts, available both in Adobe Type 1 format (*.pfb) and in OpenType format
(*.otf).  There are five sets of TeX Font Metric files, corresponding to: Cork
encoding (cork-*.tfm); QX encoding (qx-*.tfm); TeX'n'ANSI aka LY1 encoding
(texnansi-*.tfm); T5 (Vietnamese) encoding (t5-*.tfm); and Text Companion for EC
fonts aka TS1 (ts1-*.tfm).")
    (license license:gfl1.0)))

(define-public texlive-latex-hyphenat
  (let ((template
         (simple-texlive-package
          "texlive-latex-hyphenat"
          (list "doc/latex/hyphenat/"
                "source/latex/hyphenat/"
                "tex/latex/hyphenat/")
          (base32 "0gm7s7bidp9b4sfgylvwydban8jylfcskmqrf0sxlwxsqxqq5fy5"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ #t)
          "latex/hyphenat")))
      (home-page "https://ctan.org/macros/latex/contrib/hyphenat")
      (synopsis "Disable/enable hypenation")
      (description
       "This package can disable all hyphenation or enable hyphenation of
non-alphabetics or monospaced fonts.  The package can also enable hyphenation
within 'words' that contain non-alphabetic characters (e.g., that include
underscores), and hyphenation of text typeset in monospaced (e.g., cmtt) fonts.")
      (license license:lppl1.3+))))

(define-public texlive-latex-ragged2e
  (let ((template
         (simple-texlive-package
          "texlive-latex-ragged2e"
          (list "doc/latex/ragged2e/"
                "source/latex/ragged2e/"
                "tex/latex/ragged2e/")
          (base32 "1cxj5jdgvr3xk1inrb3yzpm3l386jjawgpqiwsz53k6yshb6yfml"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ #t)
          "latex/ragged2e")))
      (home-page "https://ctan.org/macros/latex/contrib/ragged2e")
      (synopsis "Alternative versions of \"ragged\"-type commands")
      (description
       "The package defines new commands \\Centering, \\RaggedLeft, and \\RaggedRight and
new environments Center, FlushLeft, and FlushRight, which set ragged text and
are easily configurable to allow hyphenation (the corresponding commands in
LaTeX, all of whose names are lower-case, prevent hyphenation altogether).")
      (license license:lppl1.3c))))

(define-public texlive-latex-everysel
  (let ((template
         (simple-texlive-package
          "texlive-latex-everysel"
          (list "doc/latex/everysel/"
                "source/latex/everysel/"
                "tex/latex/everysel/")
          (base32 "0skzm2qsk5vpjxgslclp4pvbbcrrnm1w3df8xfvfq252dyd7w8s5"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ #t)
          "latex/everysel")))
      (home-page "https://ctan.org/macros/latex/contrib/everysel")
      (synopsis "Provides hooks into \\selectfont")
      (description
       "The package provided hooks whose arguments are executed just after LaTeX has
loaded a new font by means of \\selectfont.  It has become obsolete with LaTeX
versions 2021/01/05 or newer, since LaTeX now provides its own hooks to fulfill
this task.  For newer versions of LaTeX everysel only provides macros using
LaTeX's hook management due to compatibility reasons.  See lthooks-doc.pdf for
instructions how to use lthooks instead of everysel.")
      (license license:lppl1.3c))))

(define-public texlive-latex-subfig
  (let ((template
         (simple-texlive-package
          "texlive-latex-subfig"
          (list "doc/latex/subfig/" "source/latex/subfig/" "tex/latex/subfig/")
          (base32 "0bq1328pb1ak91j7q8n1kh2fncr742lvff7apgf8kkxzxjfg2z9r"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ #t)
          "latex/subfig")))
      (home-page "https://ctan.org/macros/latex/contrib/subfig")
      (synopsis "Figures broken into subfigures")
      (description
       "The package provides support for the manipulation and reference of small or
'sub' figures and tables within a single figure or table environment.  It is
convenient to use this package when your subfigures are to be separately
captioned, referenced, or are to be included in the List-of-Figures.  A new
\\subfigure command is introduced which can be used inside a figure environment
for each subfigure.  An optional first argument is used as the caption for that
subfigure.  This package supersedes the subfigure package (which is no longer
maintained).  The name was changed since the package is not completely backward
compatible with the older package The major advantage of the new package is that
the user interface is keyword/value driven and easier to use.  To ease the
transition from the subfigure package, the distribution includes a configuration
file (subfig.cfg) which nearly emulates the subfigure package.  The
functionality of the package is provided by the (more recent still) subcaption
package.")
      (license license:lppl))))
