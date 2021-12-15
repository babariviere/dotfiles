(define-module (baba packages qmk)
  #:use-module (gnu packages)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public python-qmk-dotty-dict
  (package
    (name "python-qmk-dotty-dict")
    (version "1.3.0.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "qmk_dotty_dict" version))
       (sha256
        (base32 "18kyzk9a00xbxjsph2a9p03zx05f9dw993n66mlamgv06qwiwq9v"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs `(("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/pawelzny/dotty_dict")
    (synopsis "Dictionary wrapper for quick access to deeply nested keys.")
    (description "Dictionary wrapper for quick access to deeply nested keys.")
    (license license:expat)))

(define-public python-hjson
  (package
    (name "python-hjson")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "hjson" version))
        (sha256
          (base32 "1mgwmim5k3pnlcxr63cz110jr28r78zrbkkfa6j9x0z501rgsf18"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "http://github.com/hjson/hjson-py")
    (synopsis "Hjson, a user interface for JSON.")
    (description "Hjson, a user interface for JSON.")
    (license license:expat)))

(define-public python-backports.shutil-get-terminal-size
  (package
    (name "python-backports.shutil-get-terminal-size")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "backports.shutil_get_terminal_size" version))
        (sha256
          (base32 "107cmn7g3jnbkp826zlj8rrj19fam301qvaqf0f3905f5217lgki"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (home-page "https://github.com/chrippa/backports.shutil_get_terminal_size")
    (synopsis
      "A backport of the get_terminal_size function from Python 3.3's shutil.")
    (description
      "A backport of the get_terminal_size function from Python 3.3's shutil.")
    (license license:expat)))

(define-public python-spinners
  (package
    (name "python-spinners")
    (version "0.0.24")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "spinners" version))
        (sha256
          (base32 "0zz2z6dpdjdq5z8m8w8dfi8by0ih1zrdq0caxm1anwhxg2saxdhy"))))
    (build-system python-build-system)
    (propagated-inputs (list python2-enum34))
    (arguments `(#:tests? #f))
    (home-page "https://github.com/manrajgrover/py-spinners")
    (synopsis "Spinners for terminals")
    (description "Spinners for terminals")
    (license license:expat)))

(define-public python-log-symbols
  (package
    (name "python-log-symbols")
    (version "0.0.14")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "log-symbols" version))
        (sha256
          (base32 "0mh5d0igw33libfmbsr1ri1p1y644p36nwaa2w6kzrd8w5pvq2yg"))))
    (build-system python-build-system)
    (propagated-inputs
      (list python-colorama python2-enum34))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/manrajgrover/py-log-symbols")
    (synopsis "Colored symbols for various log levels for Python")
    (description "Colored symbols for various log levels for Python")
    (license license:expat)))

(define-public python-halo
  (package
    (name "python-halo")
    (version "0.0.31")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "halo" version))
        (sha256
          (base32 "1mn97h370ggbc9vi6x8r6akd5q8i512y6kid2nvm67g93r9a6rvv"))))
    (build-system python-build-system)
    (propagated-inputs
      (list python-backports.shutil-get-terminal-size
            python-colorama
            python-log-symbols
            python-six
            python-spinners
            python-termcolor))
    (arguments `(#:tests? #f))
    (home-page "https://github.com/manrajgrover/halo")
    (synopsis "Beautiful terminal spinners in Python")
    (description "Beautiful terminal spinners in Python")
    (license license:expat)))

(define-public python-milc
  (package
    (name "python-milc")
    (version "1.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "milc" version))
        (sha256
          (base32 "0bbqgyzzcq7hpp8xypd188ga49zf9kv7qljbd29ms9kvl45d9j5j"))))
    (build-system python-build-system)
    (propagated-inputs
      (list python-appdirs python-argcomplete python-colorama python-halo
            python-spinners))
    (home-page "https://milc.clueboard.co/")
    (synopsis "Opinionated Batteries-Included Python 3 CLI Framework.")
    (description "Opinionated Batteries-Included Python 3 CLI Framework.")
    (license license:expat)))

(define-public python-hid
  (package
    (name "python-hid")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "hid" version))
        (sha256
          (base32 "1h9zi0kyicy3na1azfsgb57ywxa8p62bq146pb44ncvsyf1066zn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (delete 'sanity-check))))
    (home-page "https://github.com/apmorton/pyhidapi")
    (synopsis "ctypes bindings for hidapi")
    (description "ctypes bindings for hidapi")
    (license license:expat)))

(define-public qmk
  (package
    (name "qmk")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "qmk" version))
        (sha256
          (base32 "1jpr22k539yc1rhn69igvh0s7hrd40vkkgmrn0vwqj257k3ywqns"))))
    (build-system python-build-system)
    (propagated-inputs
      (list python-hid
            python-hjson
            python-jsonschema
            python-milc
            python-pygments
            python-pyusb
            python-qmk-dotty-dict
            python-setuptools))
    (inputs
     `(("setup.py" ,(plain-file "setup.py"
				(string-join
				 '("from setuptools import setup"
				   "setup()")
				 "\n")))))
    (arguments
     `(#:tests?
       #f
       #:phases
       (modify-phases %standard-phases
	 (add-before 'build 'add-setup-py
	   (lambda* (#:key inputs #:allow-other-keys)
	     (let ((setup-py (assoc-ref inputs "setup.py")))
	       (copy-file setup-py "setup.py")))))))
    (home-page "")
    (synopsis "A program to help users work with QMK Firmware.")
    (description "A program to help users work with QMK Firmware.")
    (license license:expat)))
