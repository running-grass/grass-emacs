{ pkgs, ... }:
let
  python-packages = python-packages:
    with python-packages; [
      pandas
      requests
      sexpdata
      tld
      pyqt6
      pyqt6-sip
      pyqt6-webengine
      epc
      lxml # for eaf
      qrcode # eaf-file-browser
      pysocks # eaf-browser
      pymupdf # eaf-pdf-viewer
      pypinyin # eaf-file-manager
      psutil # eaf-system-monitor
      retry # eaf-markdown-previewer
      markdown
    ];
in pkgs.python3.withPackages python-packages
