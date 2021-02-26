export PATH="/usr/local/opt/qt/bin:/usr/local/opt/ruby/bin:/usr/local/sbin:$HOME/Library/Python/3.9/bin:$HOME/.cargo/bin:/usr/local/bin:$PATH"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export PKG_CONFIG_PATH="/usr/local/opt/qt/lib/pkgconfig:/usr/local/opt/libpq/lib/pkgconfig"

# GraalVM
export PATH="/Library/Java/JavaVirtualMachines/graalvm-ce-java8-21.0.0/Contents/Home/bin:$PATH"
export GRAALVM_HOME="$(jabba which graalvm-ce-java11@21.0.0)/Contents/Home"
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/graalvm-ce-java8-21.0.0/Contents/Home
if [ -e /Users/bastienriviere/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/bastienriviere/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
