(define-module (baba packages compton)
  #:use-module (gnu packages compton)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public picom-ibhagwan
  (let ((commit "c4107bb6cc17773fdc6c48bb2e475ef957513c7a"))
    (package
     (inherit picom)
     (name "picom-ibhagwan")
     (version commit)
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ibhagwan/picom")
             (commit commit)))
       (sha256
        (base32 "035fbvb678zvpm072bzzpk8h63npmg5shkrzv4gfj89qd824a5fn"))
       (file-name (git-file-name name version)))))))

(define-public picom-next
  (package
   (inherit picom)
   (name "picom")
   (version "next")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/yshui/picom")
           (commit "1e8fcd00a9fd47ea511058907f64e58e57965e77")))
     (sha256
      (base32 "0zyqsaqprrkx8y1i9a3lnv8m8j1ckhc50aika9iqfhcjqqhpxnjh"))
     (file-name (git-file-name name version))))))
