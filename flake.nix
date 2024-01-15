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
            ];
          });

    in flake-utils.lib.eachDefaultSystem (system:
      let
        vars = { project_root = builtins.getEnv "PWD"; };
        inherit (getEmacs system) pkgs emacsWrap packages;
      in {
        devShells.default =
          import ./shell.nix { inherit pkgs vars packages emacsWrap; };
        nixosModules.emacs = { config, ... }: {
          options = { };
          config = {
            environment.systemPackages = packages;
            services.emacs = {
              enable = true;
              package = emacsWrap;
              defaultEditor = true;
            };
          };
        };

      })
    # Nixos 使用的模块
    // (let
      system = "x86_64-linux";
      inherit (getEmacs system) emacsWrap packages;
    in {
      nixosModules.default = { config, ... }: {
        options = { };
        config = {
          environment.systemPackages = packages;
          services.emacs = {
            enable = true;
            package = emacsWrap;
            defaultEditor = true;
          };
        };
      };
    })
    # Macos 使用的模块
    // (let
      system = "x86_64-darwin";
      inherit (getEmacs system) emacsWrap packages;
    in {
      darwinModules.default = { config, ... }: {
        options = { };
        config = {
          environment.systemPackages = packages;
          services.emacs = {
            enable = true;
            package = emacsWrap;
          };
        };
      };
    });
}
