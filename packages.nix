{ pkgs, ... }:
let python = import ./python.nix { inherit pkgs; };
in with pkgs;
[
  # lsp
  rnix-lsp # nix
  phpactor # php
  vscode-langservers-extracted
  nodePackages.typescript-language-server # ts
  nodePackages.volar # vue
  yaml-language-server

  # mardown
  multimarkdown

  # email
  mu
  offlineimap

  # org
  pandoc

  # check and format
  shellcheck
  html-tidy
  nixfmt
  nodePackages.prettier

  # plantuml
  # plantuml
  # jdk
  # graphviz-nox

  # eaf
  pkg-config
  libinput
  libevdev

  git
  nodejs
  wmctrl
  xdotool
  # eaf-browser
  aria
  # eaf-file-manager
  fd

] ++ [ python ]
