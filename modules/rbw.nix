{ pkgs, ... }: {
  home.packages = with pkgs; [ pinentry ];
  programs.rbw = rec {
    enable = true;
    settings = {
      base_url = "https://vw.mugeda.com";
      email = "liushihua@mugeda.com";
      pinentry = "tty";
    };
  };
}
