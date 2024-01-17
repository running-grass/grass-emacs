{ pkgs, ... }:
let
  emacsBase =
    if pkgs.stdenv.isDarwin then pkgs.emacs-macport else pkgs.emacs-pgtk;
  emacsWrap = (pkgs.emacsWithPackagesFromUsePackage {
    package = emacsBase;

    config = ./README.org;

    # defaultInitFile = ./init.el;
    defaultInitFile = pkgs.substituteAll {
      name = "default.el";
      src = ./init.el;
    };
    #              defaultInitFile = false;
    alwaysTangle = true;

    extraEmacsPackages = epkgs:
      with epkgs;
      [ treesit-grammars.with-all-grammars ];
  });

in emacsWrap
