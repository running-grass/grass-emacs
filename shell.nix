{ pkgs, vars, packages, emacsWrap, ... }:

pkgs.mkShell {
  name = "Emacs debug shell";

  "EMACS_DEBUG_DIR" = vars.project_root;

  buildInputs = [ emacsWrap packages ];

  shellHook = "";
}
