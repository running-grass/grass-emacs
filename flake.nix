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

    # emacs-overlay = {
    #   url = "github:nix-community/emacs-overlay";
    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.nixpkgs-stable.follows = "nixpkgs-stable";
    # };
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      getEmacs = system: rec {
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          # overlays = [ (import emacs-overlay) ];
        };
        emacsWrap = import ./emacsWrap.nix { inherit pkgs; };
        fontPackages = import ./font-packages.nix { inherit pkgs; };
        env-vars = {
          # eaf
          QT_QPA_PLATFORM_PLUGIN_PATH =
            "${pkgs.qt6.qtbase.outPath}/lib/qt-6/plugins";
          GRASS_EMACS_ENV = "manaual";
        };
      };

    in { }
    # Nixos 使用的模块
    // (let
      system = "x86_64-linux";
      inherit (getEmacs system) emacsWrap fontPackages env-vars;
    in {
      nixosModules.default = { config, ... }: {
        options = { };
        config = {
          environment.systemPackages = [ emacsWrap ];
          environment.variables =
            (env-vars // { GRASS_EMACS_ENV = "nix-module"; });
          fonts = {
            enableDefaultPackages = true;
            fontDir.enable = true;
            packages = fontPackages;
          };

          services.emacs = {
            enable = false;
            package = emacsWrap;
            defaultEditor = true;
          };
        };
      };
    })
    # Macos 使用的模块
    // (let
      system = "x86_64-darwin";
      inherit (getEmacs system) emacsWrap  fontPackages env-vars pkgs;
    in {
      darwinModules.default = { config, ... }: {
        options = { };
        config = {
          fonts = {
            fontDir.enable = true;
            fonts = fontPackages;
          };

          homebrew = {
            enable = true;
            taps = [ "d12frosted/emacs-plus" ];

            brews = [ "emacs-plus@30" ];
          };
        };
      };
    })
    # 为每个系统提供
    // flake-utils.lib.eachDefaultSystem (system:
      let
        vars = { project_root = builtins.getEnv "PWD"; };
        inherit (getEmacs system) pkgs emacsWrap packages env-vars;
      in {
        # devShells
        devShells.default =
          import ./shell.nix { inherit pkgs vars packages emacsWrap env-vars; };
      })

  ;
}
