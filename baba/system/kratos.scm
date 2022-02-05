(define-module (baba system kratos)
  #:use-module (baba)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services base)
  #:use-module (gnu services certbot)
  #:use-module (gnu services cuirass)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services web)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (guix gexp)
  #:export (%system/kratos))


(define %letsencrypt-dir "/etc/letsencrypt/live")
(define %letsencrypt-acme-challenge (nginx-location-configuration
				     (uri "/.well-known")
				     (body '("root /srv/http;"))))

(define %cuirass-port 32800)
(define %publish-port 32900)

(define %cuirass-specs
  #~(list
     (specification
	  (name "emacs")
	  (build '(channels emacs))
	  (channels
	   (cons*
	    (channel
	     (name 'emacs)
	     (url "https://github.com/babariviere/guix-emacs"))
	    %default-channels)))
     (specification
      (name "baba")
      (build '(channels baba))
      (channels
       (cons*
        (channel
         (name 'baba)
         (branch "guix")
         (url "https://github.com/babariviere/dotfiles"))
        %default-channels)))
	 (specification
	  (name "flat")
	  (build '(channels flat))
	  (channels
	   (cons*
	    (channel
	     (name 'flat)
	     (url "https://github.com/flatwhatson/guix-channel.git"))
	    %default-channels)))))


(define (publish-locations url)
  "Return the nginx location blocks for 'guix publish' running on URL."
  (list (nginx-location-configuration
         (uri "/nix-cache-info")
         (body
          (list
           (string-append
            "proxy_pass " url "/nix-cache-info;")
           ;; Cache this file since that's always the first thing we ask
           ;; for.
           "proxy_cache static;"
           "proxy_cache_valid 200 100d;"     ; cache hits for a looong time.
           "proxy_cache_valid any 5m;"       ; cache misses/others for 5 min.
           "proxy_ignore_client_abort on;"

           ;; We need to hide and ignore the Set-Cookie header to enable
           ;; caching.
           "proxy_hide_header    Set-Cookie;"
           "proxy_ignore_headers Set-Cookie;")))

        (nginx-location-configuration
         (uri "/nar/")
         (body
          (list
           (string-append "proxy_pass " url ";")
           "client_body_buffer_size 256k;"

           ;; Be more tolerant of delays when fetching a nar.
           "proxy_read_timeout 60s;"
           "proxy_send_timeout 60s;"

           ;; Enable caching for nar files, to avoid reconstructing and
           ;; recompressing archives.
           "proxy_cache nar;"
           "proxy_cache_valid 200 30d;"           ; cache hits for 1 month
           "proxy_cache_valid 504 3m;" ; timeout, when hydra.gnu.org is overloaded
           "proxy_cache_valid any 1h;" ; cache misses/others for 1h.

           "proxy_ignore_client_abort on;"

           ;; Nars are already compressed.
           "gzip off;"

           ;; We need to hide and ignore the Set-Cookie header to enable
           ;; caching.
           "proxy_hide_header    Set-Cookie;"
           "proxy_ignore_headers Set-Cookie;"

           ;; Provide a 'content-length' header so that 'guix
           ;; substitute-binary' knows upfront how much it is downloading.
           ;; "add_header Content-Length $body_bytes_sent;"
           )))

        (nginx-location-configuration
         (uri "~ \\.narinfo$")
         (body
          (list
           ;; Since 'guix publish' has its own caching, and since it relies
           ;; on the atime of cached narinfos to determine whether a
           ;; narinfo can be removed from the cache, don't do any caching
           ;; here.
           (string-append "proxy_pass " url ";")

           ;; For HTTP pipelining.  This has a dramatic impact on
           ;; performance.
           "client_body_buffer_size 128k;"

           ;; Narinfos requests are short, serve many of them on a
           ;; connection.
           "keepalive_requests 600;"

           ;; Do not tolerate slowness of hydra.gnu.org when fetching
           ;; narinfos: better return 504 quickly than wait forever.
           "proxy_connect_timeout 2s;"
           "proxy_read_timeout 2s;"
           "proxy_send_timeout 2s;"

           ;; 'guix publish --ttl' produces a 'Cache-Control' header for
           ;; use by 'guix substitute'.  Let it through rather than use
           ;; nginx's "expire" directive since the expiration time defined
           ;; by 'guix publish' is the right one.
           "proxy_pass_header Cache-Control;"

           "proxy_ignore_client_abort on;"

           ;; We need to hide and ignore the Set-Cookie header to enable
           ;; caching.
           "proxy_hide_header    Set-Cookie;"
           "proxy_ignore_headers Set-Cookie;")))

        (nginx-location-configuration
         (uri "/log/")
         (body
          (list
           (string-append "proxy_pass " url ";")

           ;; Enable caching for build logs.
           "proxy_cache logs;"
           "proxy_cache_valid 200 60d;"           ; cache hits.
           "proxy_cache_valid 504 3m;" ; timeout, when hydra.gnu.org is overloaded
           "proxy_cache_valid any 1h;" ; cache misses/others.

           "proxy_ignore_client_abort on;"

           ;; We need to hide and ignore the Set-Cookie header to enable
           ;; caching.
           "proxy_hide_header    Set-Cookie;"
           "proxy_ignore_headers Set-Cookie;")))

        ;; Content-addressed files served by 'guix publish'.
        (nginx-location-configuration
         (uri "/file/")
         (body
          (list
           (string-append "proxy_pass " url ";")

           "proxy_cache cas;"
           "proxy_cache_valid 200 200d;"          ; cache hits
           "proxy_cache_valid any 5m;"            ; cache misses/others

           "proxy_ignore_client_abort on;")))))



(define (cuirass-locations cuirass-url publish-url)
  (append (publish-locations publish-url)
	  (list
	   %letsencrypt-acme-challenge
	   (nginx-location-configuration
	    (uri "/")
	    (body (list (string-append "proxy_pass " cuirass-url ";"))))
	   (nginx-location-configuration
	    (uri "/admin")
	    (body (list "deny all;")))


           (nginx-location-configuration
            (uri "/static")
            (body
             (list
              (string-append "proxy_pass " cuirass-url ";")
              ;; Let browsers cache this for a while.
              "expires 10d;"
              ;; Cache quite aggressively.
              "proxy_cache static;"
              "proxy_cache_valid 200 5d;"
              "proxy_cache_valid any 10m;"
              "proxy_ignore_client_abort on;"))))))

(define %cuirass-extra-content
  (list
    "default_type application/octet-stream;"
    "sendfile on;"
    "sendfile_max_chunk 1m;"
    "keepalive_timeout  65;"
    "proxy_http_version 1.1;"

    ;; cache for nar files
    "proxy_cache_path /var/cache/nginx/nar"
    "     levels=2"
    "     inactive=8d"       ; inactive keys removed after 8d
    "     keys_zone=nar:4m"  ; nar cache meta data: ~32K keys
    "     max_size=10g;"     ; total cache data size max

    ;; cache for content-addressed-files
    "proxy_cache_path /var/cache/nginx/cas"
    "     levels=2"
    "     inactive=180d"     ; inactive keys removed after 180d
    "     keys_zone=cas:8m"  ; nar cache meta data: ~64K keys
    "     max_size=50g;"         ; total cache data size max

    ;; cache for build logs
    "proxy_cache_path /var/cache/nginx/logs"
    "     levels=2"
    "     inactive=60d"          ; inactive keys removed after 60d
    "     keys_zone=logs:8m"     ; narinfo meta data: ~64K keys
    "     max_size=4g;"          ; total cache data size max

    ;; cache for static data
    "proxy_cache_path /var/cache/nginx/static"
    "     levels=1"
    "     inactive=10d"         ; inactive keys removed after 10d
    "     keys_zone=static:1m"   ; nar cache meta data: ~8K keys
    "     max_size=200m;"        ; total cache data size max

    ;; Cache timeouts for a little while to avoid increasing pressure.
    "proxy_cache_valid 504 30s;"))


(define %cuirass-server
  (nginx-server-configuration
   (server-name '("ci.babariviere.com"))
   (listen '("443 ssl http2" "[::]:443 ssl http2"))
   (ssl-certificate (string-append %letsencrypt-dir "/babariviere.com/fullchain.pem"))
   (ssl-certificate-key (string-append %letsencrypt-dir "/babariviere.com/privkey.pem"))
   (locations (cuirass-locations (string-append "http://127.0.0.1:" (number->string %cuirass-port))
				 (string-append "http://127.0.0.1:" (number->string %publish-port))))))

(define %nginx-configuration
  (nginx-configuration
   (extra-content (string-join %cuirass-extra-content "\n"))
   (server-blocks
    (list
     (nginx-server-configuration
      (server-name '("babariviere.com" "www.babariviere.com"))
      (listen '("443 ssl http2" "[::]:443 ssl http2"))
      (ssl-certificate (string-append %letsencrypt-dir "/babariviere.com/fullchain.pem"))
      (ssl-certificate-key (string-append %letsencrypt-dir "/babariviere.com/privkey.pem"))
      (locations (list
		  %letsencrypt-acme-challenge)))
     %cuirass-server))))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define %system/kratos
  (operating-system
    (host-name "kratos")
    (timezone "Europe/Paris")
    (bootloader (bootloader-configuration
		 (bootloader grub-bootloader)
		 (targets '("/dev/sda"))
		 (terminal-outputs '(console))))
    (file-systems (cons* (file-system
			   (mount-point "/")
			   (device (file-system-label "root"))
			   (type "btrfs")
			   (options "subvol=system,compress=zstd"))
			 %base-file-systems))
    (packages (append (list
		       htop
		       btrfs-progs
		       ;; for HTTPS access
		       nss-certs
		       )
		      %base-packages))
    (services
     (append (list (service dhcp-client-service-type)
		   (service openssh-service-type
			    (openssh-configuration
			     (permit-root-login 'prohibit-password)
			     (allow-empty-passwords? #f)
			     ;; TODO: better system for ssh keys
			     (authorized-keys `(("root" ,(local-file (string-append %channel-root "/etc/ssh/gaia.pub")))))))
		   (service cuirass-service-type
			    (cuirass-configuration
			     (specifications %cuirass-specs)
			     (port %cuirass-port)
			     (use-substitutes? #t)))
		   (service guix-publish-service-type
			    (guix-publish-configuration
			     (compression '(("lzip" 3) ("gzip" 3)))
			     (port %publish-port)))
		   (service certbot-service-type
			    (certbot-configuration
			     (email "bot@babariviere.com")
			     (webroot "/srv/http")
			     (certificates
			      (list
			       (certificate-configuration
				(name "babariviere.com")
				(domains '("babariviere.com" "www.babariviere.com" "ci.babariviere.com"))
				(deploy-hook %nginx-deploy-hook))))))
		   (service nginx-service-type
			    %nginx-configuration)

		   (service unattended-upgrade-service-type))
	     (modify-services
		 %base-services
	       (guix-service-type config =>
				  (guix-configuration
				   (inherit config)
				   (discover? #f)
				   (authorized-keys (append
						     %default-authorized-guix-keys
						     (list (local-file (string-append %channel-root "/etc/keys/gaia.pub"))))))))))))

%system/kratos
