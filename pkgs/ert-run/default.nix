{ writeShellScriptBin, emacs }:

writeShellScriptBin "ert-run" ''
  ${emacs}/bin/emacs -batch -l ert -l $1 -f ert-run-tests-batch-and-exit
''
