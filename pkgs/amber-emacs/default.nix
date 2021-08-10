{ emacs, emacsPackagesFor }:

let emacsWithPackages = (emacsPackagesFor emacs).emacsWithPackages;
in emacsWithPackages (epkgs: with epkgs; [ doom-modeline ivy use-package ])
