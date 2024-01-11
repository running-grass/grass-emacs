{ pkgs, vars, packages, emacsWrap, ... }:
pkgs.mkShell {
  buildInputs = packages ++ [ emacsWrap ];

  shellHook = ''
  '';
}
