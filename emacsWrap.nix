{ pkgs, ... }:

with pkgs;
((emacsPackagesFor emacs).emacsWithPackages
  (epkgs: with epkgs; [ mu4e treesit-grammars.with-all-grammars ]))
