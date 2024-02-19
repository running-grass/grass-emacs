{ pkgs, ... }:

with pkgs;
((emacsPackagesFor emacs-pgtk).emacsWithPackages (epkgs: [ epkgs.mu4e ]))
