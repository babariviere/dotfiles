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

(define-public omnisharp
  (package
    (name "omnisharp")
    (version "1.38.2")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri
        (string-append "https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v"
                       version "/omnisharp-linux-x64.tar.gz"))
       (sha256
        (base32
         "0yv1sqzdpdxjywvvb5i2ik42gazwy0nsjhqqvypdm2aldvnqaby8"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan
       `(("bin/mono"
          ("gcc:lib" "zlib"))
         ("lib/libmono-native.so"
          ("mit-krb5")))
       #:install-plan
       `(("run" "bin/omnisharp")
         ("bin" "share/omnisharp/")
         ("etc" "share/omnisharp/")
         ("lib" "share/omnisharp/")
         ("omnisharp" "share/omnisharp/"))
       #:modules ((nonguix build binary-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (ice-9 regex)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-wrapper
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "run"
               (("base_dir=.*")
                (string-append "base_dir="
                               (assoc-ref outputs "out") "/share/omnisharp\n"))
               (("chmod.*") ""))))
         (add-before 'patchelf 'patchelf-writable
           (lambda _
             (for-each make-file-writable
                       '("bin/mono" "lib/libmono-native.so"))))
         (add-after 'install 'dotnet-sdk
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dotnet (assoc-ref inputs "dotnet")))
               (wrap-program (string-append out "/bin/omnisharp")
                 `("DOTNET_ROOT" = (,dotnet))
                 `("MSBuildSDKsPath" = (,(string-append dotnet "/share/dotnet/sdk/" ,(package-version dotnet-lts) "/Sdks")))
                 `("PATH" prefix (,(string-append dotnet "/bin")))
                 `("MSBuildEnableWorkloadResolver" = ("false")))))))))
    (inputs
     `(("dotnet" ,dotnet-lts)
       ("gcc:lib" ,gcc "lib")
       ("mit-krb5" ,mit-krb5)
       ("zlib" ,zlib)))
    (home-page "https://github.com/OmniSharp/omnisharp-roslyn")
    (supported-systems '("x86_64-linux"))
    (synopsis "Implementation of Language Server Protocol based on Roslyn workspaces")
    (description "OmniSharp is a @code{.NET} development platform based on
Roslyn workspaces.  It provides project dependencies and C# language services to
various IDEs and plugins.")
    (license license:expat)))
