{ writeShellScriptBin, patchelf, glibc }:

writeShellScriptBin "nix-patch" ''
  if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <dir>"
    exit 1
  fi

  # Taken from https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/setup-hooks/patch-shebangs.sh
  patchShebangs() {
      local pathName

      if [ "$1" = "--host" ]; then
          pathName=HOST_PATH
          shift
      elif [ "$1" = "--build" ]; then
          pathName=PATH
          shift
      fi

      echo "patching script interpreter paths in $@"
      local f
      local oldPath
      local newPath
      local arg0
      local args
      local oldInterpreterLine
      local newInterpreterLine

      if [ $# -eq 0 ]; then
          echo "No arguments supplied to patchShebangs" >&2
          return 0
      fi

      local f
      while IFS= read -r -d $'\0' f; do
          isScript "$f" || continue

          oldInterpreterLine=$(head -1 "$f" | tail -c+3)
          read -r oldPath arg0 args <<< "$oldInterpreterLine"

          if [ -z "$pathName" ]; then
              if [ -n "$strictDeps" ] && [[ "$f" = "$NIX_STORE"* ]]; then
                  pathName=HOST_PATH
              else
                  pathName=PATH
              fi
          fi

          if $(echo "$oldPath" | grep -q "/bin/env$"); then
              # Check for unsupported 'env' functionality:
              # - options: something starting with a '-'
              # - environment variables: foo=bar
              if $(echo "$arg0" | grep -q -- "^-.*\|.*=.*"); then
                  echo "$f: unsupported interpreter directive \"$oldInterpreterLine\" (set dontPatchShebangs=1 and handle shebang patching yourself)" >&2
                  exit 1
              fi

              newPath="$(PATH="''${!pathName}" command -v "$arg0" || true)"
          else
              if [ "$oldPath" = "" ]; then
                  # If no interpreter is specified linux will use /bin/sh. Set
                  # oldpath="/bin/sh" so that we get /nix/store/.../sh.
                  oldPath="/bin/sh"
              fi

              newPath="$(PATH="''${!pathName}" command -v "$(basename "$oldPath")" || true)"

              args="$arg0 $args"
          fi

          # Strip trailing whitespace introduced when no arguments are present
          newInterpreterLine="$(echo "$newPath $args" | sed 's/[[:space:]]*$//')"

          if [ -n "$oldPath" -a "''${oldPath:0:''${#NIX_STORE}}" != "$NIX_STORE" ]; then
              if [ -n "$newPath" -a "$newPath" != "$oldPath" ]; then
                  echo "$f: interpreter directive changed from \"$oldInterpreterLine\" to \"$newInterpreterLine\""
                  # escape the escape chars so that sed doesn't interpret them
                  escapedInterpreterLine=$(echo "$newInterpreterLine" | sed 's|\\|\\\\|g')
                  # Preserve times, see: https://github.com/NixOS/nixpkgs/pull/33281
                  timestamp=$(mktemp)
                  touch -r "$f" "$timestamp"
                  sed -i -e "1 s|.*|#\!$escapedInterpreterLine|" "$f"
                  touch -r "$timestamp" "$f"
                  rm "$timestamp"
              fi
          fi
      done < <(find "$@" -type f -perm -0100 -print0)

      stopNest
  }

  patchShebangsAuto () {
      if [ -z "''${dontPatchShebangs-}" -a -e "$prefix" ]; then

          # Dev output will end up being run on the build platform. An
          # example case of this is sdl2-config. Otherwise, we can just
          # use the runtime path (--host).
          if [ "$output" != out ] && [ "$output" = "$outputDev" ]; then
              patchShebangs --build "$prefix"
          else
              patchShebangs --host "$prefix"
          fi
      fi
  }

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
  patchShebangs --build $1
  find $1 -executable -type f -exec ${patchelf}/bin/patchelf --set-interpreter ${glibc}/lib/ld-linux-x86-64.so.2 {} \;
''
