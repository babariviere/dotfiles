{ emacs, emacsPackagesFor }:

let emacsWithPackages = (emacsPackagesFor emacs).emacsWithPackages;
in emacsWithPackages (epkgs:
  with epkgs; [
    counsel
    doom-modeline
    helpful
    ivy
    ivy-rich
    swiper
    use-package
    which-key

    evil
    evil-collection
    general

    # Themes
    doom-themes
    kaolin-themes
  ])
