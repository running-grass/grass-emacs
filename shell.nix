{ pkgs, vars, packages, emacsWrap, env-vars, ... }:
let
  dev-emacs = pkgs.writeShellScriptBin "dev-emacs" "${emacsWrap}/bin/emacs";
in
pkgs.mkShell {
  name = "Emacs debug shell";

  "EMACS_DEBUG_DIR" = vars.project_root;
  "GRASS_EMACS_ENV" = "debug-shell";

  # vars
  inherit (env-vars) QT_QPA_PLATFORM_PLUGIN_PATH;

  buildInputs = packages ++ [ emacsWrap dev-emacs ];

  shellHook = "";
}
