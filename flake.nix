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
      getEmacs = system: rec {
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [ (import emacs-overlay) ];
        };
        emacsWrap = import ./emacsWrap.nix { inherit pkgs; };
        packages = import ./packages.nix { inherit pkgs; };
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
      inherit (getEmacs system) emacsWrap packages env-vars;
    in {
      nixosModules.default = { config, ... }: {
        options = { };
        config = {
          environment.systemPackages = packages ++ [ emacsWrap ];
          environment.variables =
            (env-vars // { GRASS_EMACS_ENV = "nix-module"; });

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
          environment.variables =
            (env-vars // { GRASS_EMACS_ENV = "nix-module"; });
          home-manager.users.grass = {
    
            programs.zsh.shellAliases = {
              emacs = "${emacsWrap}/Applications/Emacs.app/Contents/MacOS/Emacs";
            };
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
