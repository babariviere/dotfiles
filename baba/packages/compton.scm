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
           (commit "5a2115033ee3d79b67a4d45ab33ce91785e9d904")))
     (sha256
      (base32 "0lh3p3lkafkb2f0vqd5d99xr4wi47sgb57x65wa2cika8pz5sikv"))
     (file-name (git-file-name name version))))))
