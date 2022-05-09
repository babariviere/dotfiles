(define-module (baba packages dotnet)
  #:use-module (ice-9 match)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages instrumentation)
  #:use-module (gnu packages tls))

(define-public dotnet-lts
  (let ((dotnet-sdk-version "6.0.4"))
    (package
      (name "dotnet")
      (version "6.0.202")
      (source
       (origin
         (method url-fetch/tarbomb)
         (uri
          (string-append "https://dotnetcli.azureedge.net/dotnet/Sdk/"
                         version "/dotnet-sdk-"
                         version "-linux-x64.tar.gz"))
         (sha256
          (base32
           "05dz8h37f9ia7dk8fmwa1bbk5v2lrflj5x4n7w9caf2jv3q0hbj5"))))
      (build-system binary-build-system)
      (arguments
       `(#:patchelf-plan
         `(("dotnet"
            ("gcc:lib" "zlib"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version
                             "/libSystem.Net.Security.Native.so")
            ("mit-krb5"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version
                             "/libSystem.Security.Cryptography.Native.OpenSsl.so")
            ("openssl"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version
                             "/libSystem.IO.Compression.Native.so")
            ("zlib"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version
                             "/libcoreclrtraceptprovider.so")
            ("gcc:lib" "lttng-ust"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version "/createdump")
            ("gcc:lib"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version "/libclrjit.so")
            ("gcc:lib"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version "/libcoreclr.so")
            ("gcc:lib" "icu4c"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version "/libdbgshim.so")
            ("gcc:lib"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version "/libhostpolicy.so")
            ("gcc:lib"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version "/libmscordaccore.so")
            ("gcc:lib"))
           (,,(string-append "shared/Microsoft.NETCore.App/"
                             dotnet-sdk-version "/libmscordbi.so")
            ("gcc:lib"))
           (,,(string-append "packs/Microsoft.NETCore.App.Host.linux-x64/"
                             dotnet-sdk-version
                             "/runtimes/linux-x64/native/singlefilehost")
            ("gcc:lib" "openssl" "mit-krb5" "zlib" "icu4c"))
           (,,(string-append "packs/Microsoft.NETCore.App.Host.linux-x64/"
                             dotnet-sdk-version
                             "/runtimes/linux-x64/native/apphost")
            ("gcc:lib"))
           (,,(string-append "packs/Microsoft.NETCore.App.Host.linux-x64/"
                             dotnet-sdk-version
                             "/runtimes/linux-x64/native/libnethost.so")
            ("gcc:lib"))
           (,,(string-append "sdk/" version "/AppHostTemplate/apphost")
            ("gcc:lib"))
           (,,(string-append "host/fxr/" dotnet-sdk-version "/libhostfxr.so")
            ("gcc:lib")))
         #:install-plan
         `(("." "share/dotnet/"))
         #:phases
         (modify-phases %standard-phases
           (add-before 'patchelf 'patchelf-writable
             (lambda _
               (for-each make-file-writable (find-files "."))))
           (add-after 'install 'install-wrapper
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin-dir (string-append out "/bin"))
                      (dotnet-target (string-append out "/share/dotnet/dotnet"))
                      (dotnet-dest (string-append bin-dir "/dotnet")))
                 (mkdir-p bin-dir)
                 (symlink dotnet-target dotnet-dest)
                 ;; First symlink, then wrap-program: dotnet cannot run when renamed
                 (wrap-program dotnet-dest
                   ;; Ensure the `dotnet' program does not phone home to share telemetry
                   `("DOTNET_CLI_TELEMETRY_OPTOUT" = ("1")))))))))
      (native-search-paths
       (list (search-path-specification
              (variable "DOTNET_ROOT")
              (separator #f)
              (files '("share/dotnet")))))
      (inputs
       `(("gcc:lib" ,gcc "lib")
         ("icu4c" ,icu4c)
         ("lttng-ust" ,lttng-ust)
         ("mit-krb5" ,mit-krb5)
         ("openssl" ,openssl)
         ("zlib" ,zlib)))
      (home-page "https://docs.microsoft.com/en-us/dotnet/")
      (supported-systems '("x86_64-linux"))
      (synopsis "Binary build of the @code{.NET} SDK and runtime")
      (description "@code{.NET} is a cross-platform developer platform for
building different types of applications.")
      (license license:expat))))
