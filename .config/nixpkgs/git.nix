{ pkgs, ... }:

{
  programs.git = {
    enable = true;
    userName = "Antoine R. Dumont (@ardumont)";
    userEmail = "antoine.romain.dumont@gmail.com";
    signing = {
      key = "0D10C3B8";
      signByDefault = true;
    };
    aliases = {
      co = "checkout";
      br = "branch";
      ci = "commit";
      st = "status";
      unstage = "reset HEAD~ --";
      last = "log -1 HEAD";
      r	= "reset HEAD~ --soft";
      rh = "reset HEAD~ --hard";
    };

  ignores = [ "*.com" "*.class" "*.dll" "*.exe" "*.o" "*.so" "*.7z" "*.dmg" "*.gz" "*.iso" "*.jar" "*.rar" "*.tar" "*.zip" "*.log" "*.sqlite" ".DS_Store" ".DS_Store?" "._*" ".Spotlight-V100" ".Trashes" "ehthumbs.db" "Thumbs.db" ".#*" "*~" "TAGS" "# -*- mode: gitignore; -*-" "*~" "\#*\#" "/.emacs.desktop" "/.emacs.desktop.lock" "*.elc" "auto-save-list" "tramp" ".\#*" ".org-id-locations" "*_archive" "*_flymake.*" "/eshell/history" "/eshell/lastdir" "/elpa/" ".ensime" ".ensime_lucene/" ];
  };

  # FIXME: port this, somehow that won't work
  # extraConfig = {
  #   core = {
  #     editor = "${pkgs.emacs}/bin/emacsclient -nw";
  #     whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
  #     excludesfile = "~/.gitignore_global";
  #     autocrlf = "input";
  #   };
  #   web = { browser = "${pkgs.qutebrowser}/bin/qutebrowser"; };
  #   push = { default = "matching"; };
  #   help = { autocorrect = 1; };
  #   credential = { helper = "cache --timeout 3600"; };
  #   diff = { compactionHeuristic = true; };
  #   # "url git@forge.softwareheritage.org" = {
  #   #     pushInsteadOf = "https://forge.softwareheritage.org";
  #   # };
  # };
}
