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
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system nixpkgs;
          overlays = [ (import emacs-overlay) ];
        };
        vars = { PROJECT_ROOT = builtins.getEnv "PWD"; };
        emacs = if pkgs.stdenv.isDarwin then pkgs.emacs-macport else pkgs.emacs;
        emacsWrap = (pkgs.emacsWithPackagesFromUsePackage {
          package = emacs;

          config = ./README.org;

          defaultInitFile = false;
          alwaysTangle = true;
        });

        packages = with pkgs; [ rnix-lsp ];
      in {
        devShells.default =
          import ./shell.nix { inherit pkgs vars packages emacsWrap; };

      }) // (let
        system = "x86_64-linux";
        pkgs = import nixpkgs {
          inherit system nixpkgs;
          overlays = [ (import emacs-overlay) ];
        };
        vars = { PROJECT_ROOT = builtins.getEnv "PWD"; };
        emacs = if pkgs.stdenv.isDarwin then pkgs.emacs-macport else pkgs.emacs;
        emacsWrap = (pkgs.emacsWithPackagesFromUsePackage {
          package = emacs;

          config = ./README.org;

          defaultInitFile = false;
          alwaysTangle = true;
        });

        packages = with pkgs; [ rnix-lsp ];

      in {
        nixosModules.default = { config }: {
          options = { };
          config = {
            environment.systemPackages = packages;
            programs.emacs = {
              enable = true;
              package = emacsWrap;
            };
            services.emacs = {
              enable = true;
              package = emacsWrap;
              defaultEditor = true;
            };
          };
        };
      });
}
