{ pkgs, ... }:

with pkgs;
((emacsPackagesFor emacs29-pgtk).emacsWithPackages
  (epkgs: with epkgs; [ mu4e treesit-grammars.with-all-grammars ]))
