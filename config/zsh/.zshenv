for file in $XDG_CONFIG_HOME/zsh/rc.d/env.*.zsh(N); do
  source $file
done

# Add flutter to path
export PATH=$PATH:$HOME/flutter/bin
export DART_VM_OPTIONS=--root-certs-file=/etc/ssl/certs/ca-certificates.crt
