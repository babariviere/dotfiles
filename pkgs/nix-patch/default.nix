{ writeShellScriptBin, patchelf, glibc }:

writeShellScriptBin "nix-patch" ''
  if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <dir>"
    exit 1
  fi

  # These next two functions need to be in scope for `patch-shebangs.sh` to work
  isScript() {
    local fn="$1"
    local fd
    local magic
    exec {fd}< "$fn"
    read -r -n 2 -u "$fd" magic
    exec {fd}<&-
    if [[ "$magic" =~ \#! ]]; then return 0; else return 1; fi
  }
  stopNest() { true; }
  source ${<nixpkgs/pkgs/build-support/setup-hooks/patch-shebangs.sh>}
  patchShebangs --build $1
  find $1 -executable -type f -exec ${patchelf}/bin/patchelf --set-interpreter ${glibc}/lib/ld-linux-x86-64.so.2 {} \;
''
