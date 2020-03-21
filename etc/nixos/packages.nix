{ pkgs, ... }:

{
  # Do not activate if not needed
  # nixpkgs.config = {
  #   allowUnfree = true;
  #   allowBroken = true;
  # };

  # List packages installed in system profile. To search by name, run:
  # nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    git which peco file wget curl tree
    most htop ncdu tmux bind xterm
    bash zsh coreutils lshw lsof dmidecode
    nmap netcat ngrep tcpdump
    zlib libzip p7zip zip unzip pigz gnutar pv
    acpi acpid acpitool
    parted testdisk
    binutils
    pmutils
    autojump
    rlwrap
    mg
    sshfs
  ];
}
