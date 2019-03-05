{ config, pkgs, ... }:

{
  nix.trustedBinaryCaches = [
    http://hydra.nixos.org
    http://cache.nixos.org
    http://hydra.nixos.org
  ];

  # Do not activate if not needed
  # nixpkgs.config = {
  #   allowUnfree = true;
  #   allowBroken = true;
  # };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    vim nano
    pass
    pmount mount
    sqlite
    python3 sshfsFuse
    python36Packages.powerline
    powerline-fonts powerline-rs
    stack
    hexedit
    bc
    html2text
    pciutils # lspci, etc...
    psmisc
    wmname
    xsane
    jq
    which
    peco
    # pinta
    scrot
    youtube-dl
    vlc x264 mplayer mpv
    gnome3.zenity gnome3.eog
    audacious
    evince fbreader mcomix
    git gitAndTools.tig gitg meld
    gnupg pinentry
    cryptsetup
    file
    wget curl tree
    gcc gnumake
    trayer
    networkmanagerapplet networkmanager
    x11 xlibs.xmessage xlibs.xmodmap xdotool x11_ssh_askpass xscreensaver xlibs.xbacklight xlibs.xdpyinfo xlibs.xkill xlibs.xhost
    xorg.libX11
    libxml2
    most
    xclip xsel pass pwgen
    htop ncdu
    tmux bind rxvt_unicode-with-plugins urxvt_perls xterm
    bash zsh
    ruby bundler
    conkeror qutebrowser
    graphviz
    coreutils lshw lsof
    nmap netcat wireshark
    zlib libzip p7zip zip unzip pigz gnutar pv
    # unrar # non-free
    acpi acpid acpitool
    gparted testdisk
    binutils
    pmutils
    autojump
    inotifyTools
    unetbootin
    alsaUtils
    # darcs
    rlwrap
    fortune cowsay
    ffmpeg
    simplescreenrecorder
    keymon
    # imagemagick
    ### printing
    # cups
    # samba
    telnet
    ncurses
    ### javascript stack
    # rhino
    # nodejs nodePackages.npm nodePackages.jshint nodePackages.grunt-cli nodePackages.npm2nix nodePackages.bower2nix
    ### communication
    # x11vnc tightvnc
    ### common-lisp
    sbcl
  ];
}
