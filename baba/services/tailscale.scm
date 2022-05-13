(define-module (baba services tailscale)
  #:use-module (baba packages tailscale)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:export (tailscale-service-type))

(define (tailscaled-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Run the tailscaled daemon.")
    (provision '(tailscale))
    (requirement '(networking))
    (start #~(make-forkexec-constructor
              (list (string-append #$tailscale "/bin/tailscaled"))
              #:environment-variables
              (list (string-append
                     "PATH=/run/current-system/profile/bin:"
                     "/run/current-system/profile/sbin"))))
    (stop #~(make-kill-destructor)))))

(define tailscale-service-type
  (service-type
   (name 'tailscale)
   (extensions
    (list (service-extension profile-service-type
                             (const (list tailscale iptables)))
          (service-extension shepherd-root-service-type
                             tailscaled-shepherd-service)))
   (default-value #f)))
