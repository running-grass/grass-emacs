{ pkgs, ... }:
let
  python = import ./python.nix { inherit pkgs; };

  onlyLinux = if pkgs.stdenv.isLinux then
    (with pkgs; [
      # eaf
      pkg-config
      libinput
      libevdev
      xdotool
    ])
  else
    [ ];
  onlyDarwin = if pkgs.stdenv.isDarwin then [ ] else [ ];

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

  wakatime

  git
  nodejs
  wmctrl
  # eaf-browser
  aria
  # eaf-file-manager
  fd

  # hugo
  hugo
] ++ [ python ] ++ onlyDarwin ++ onlyLinux
