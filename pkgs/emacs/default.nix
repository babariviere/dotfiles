{ emacsGit, ... }:

builtins.foldl' (drv: fn: fn drv) emacsGit [
  (drv: drv.override ({ nativeComp = true; }))
  (drv:
    drv.overrideAttrs (attrs: {
      name = "emacs-osx-${attrs.version}";

      CFLAGS = "-DMAC_OS_X_VERSION_MAX_ALLOWED=110200 -g -O2";
    }))
]
