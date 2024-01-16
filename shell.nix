{ pkgs, vars, packages, emacsWrap, env-vars, ... }:

pkgs.mkShell {
  name = "Emacs debug shell";

  "EMACS_DEBUG_DIR" = vars.project_root;

  # vars
  inherit (env-vars) QT_QPA_PLATFORM_PLUGIN_PATH;

  buildInputs = [ emacsWrap packages ];

  shellHook = "";
}
