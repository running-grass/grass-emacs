{ pkgs, vars, packages, emacsWrap, ... }:

pkgs.mkShell {
  name = "调试 Emacs 配置使用的 Shell";
  buildInputs = [
    emacsWrap
    packages
  ];

  shellHook = ''
  '';
}
