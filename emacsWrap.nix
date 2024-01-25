{ pkgs, ... }:
let
  emacsBase = if pkgs.stdenv.isDarwin then pkgs.emacs-macport else pkgs.emacs-pgtk;
  emacsWrap = (pkgs.emacsWithPackagesFromUsePackage {
    package = emacsBase;

    config = ./README.org;

    defaultInitFile = pkgs.substituteAll {
      name = "default.el";
      src = ./init.el;
    };

    alwaysTangle = true;

    extraEmacsPackages = epkgs:
      with epkgs;
      [ treesit-grammars.with-all-grammars ];
  });

in emacsWrap
