{
  description = "我的 Emacs 配置";

  nixConfig = {
    extra-substituters = [ "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    };

  };
  outputs = { self, nixpkgs, emacs-overlay, flake-utils, ... }:
    let
      getEmacs = system:
        (

          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ (import emacs-overlay) ];
            };
            emacs = if pkgs.stdenv.isDarwin then
              pkgs.emacs-macport
            else
              pkgs.emacs-pgtk;
            emacsWrap = (pkgs.emacsWithPackagesFromUsePackage {
              package = emacs;

              config = ./README.org;

              # defaultInitFile = ./init.el;
              defaultInitFile = pkgs.substituteAll {
                name = "default.el";
                src = ./init.el;
              };
              #              defaultInitFile = false;
              alwaysTangle = true;
            });
            my-python-packages = python-packages:
              with python-packages; [
                pandas
                requests
                sexpdata
                tld
                pyqt6
                pyqt6-sip
                pyqt6-webengine
                epc
                lxml # for eaf
                qrcode # eaf-file-browser
                pysocks # eaf-browser
                pymupdf # eaf-pdf-viewer
                pypinyin # eaf-file-manager
                psutil # eaf-system-monitor
                retry # eaf-markdown-previewer
                markdown
              ];
            python-with-my-packages =
              pkgs.python3.withPackages my-python-packages;
          in {
            inherit pkgs emacsWrap;
            packages = with pkgs; [
              # lsp
              rnix-lsp # nix
              phpactor # php
              nodePackages.typescript-language-server # ts
              nodePackages.volar # vue

              # fmt
              nixfmt

              # mardown
              multimarkdown

              # email
              mu
              offlineimap

              # eaf
              pkg-config
              libinput
              libevdev

              git
              nodejs
              wmctrl
              xdotool
              python-with-my-packages
              # eaf-browser
              aria
              # eaf-file-manager
              fd
            ];
            env-vars = {
              # eaf
              QT_QPA_PLATFORM_PLUGIN_PATH =
                "${pkgs.qt6.qtbase.outPath}/lib/qt-6/plugins";
            };
          });

    in flake-utils.lib.eachDefaultSystem (system:
      let
        vars = { project_root = builtins.getEnv "PWD"; };
        inherit (getEmacs system) pkgs emacsWrap packages env-vars;
      in {
        devShells.default =
          import ./shell.nix { inherit pkgs vars packages emacsWrap env-vars; };
      })
    # Nixos 使用的模块
    // (let
      system = "x86_64-linux";
      inherit (getEmacs system) emacsWrap packages env-vars;
    in {
      nixosModules.default = { config, ... }: {
        options = { };
        config = {
          environment.systemPackages = packages ++ [ emacsWrap ];
          environment.variables = env-vars;
          # services.emacs = {
          #   enable = true;
          #   package = emacsWrap;
          #   defaultEditor = true;
          # };
        };
      };
    })
    # Macos 使用的模块
    // (let
      system = "x86_64-darwin";
      inherit (getEmacs system) emacsWrap packages env-vars;
    in {
      darwinModules.default = { config, ... }: {
        options = { };
        config = {
          environment.systemPackages = packages ++ [ emacsWrap ];
          environment.variables = env-vars;
          # services.emacs = {
          #   enable = true;
          #   package = emacsWrap;
          # };
        };
      };
    });
}
