{ pkgs, ... }:

with pkgs;
((emacsPackagesFor emacs-pgtk).emacsWithPackages
  (epkgs: with epkgs; [ mu4e treesit-grammars.with-all-grammars ]))
