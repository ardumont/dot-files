{ config, pkgs, ... }:

let defaultUser = "tony";
in {
  programs = {
    zsh.enable = true;
    bash.enableCompletion = true; # for nix-shell
  };

  users = {
    defaultUserShell = "${pkgs.zsh}/bin/zsh";

    # Define a user account. Don't forget to set a password with ‘passwd’.
    # we could also set `mutableUsers = false;` and add a `password = pass;` entry (but then git out this file)
    extraUsers = [{
      description = "Antoine R. Dumont";
      name = "${defaultUser}";
      group = "users";
      uid = 1000;
      createHome = true;
      home = "/home/${defaultUser}";
      password = "dummy"; # for the first time, make sure to not be locked out passwordless then change it manually
      extraGroups = [ "wheel" "audio" "video" "vboxusers" "docker" "networkmanager" "dialout" ];
      useDefaultShell = true;
    }];
  };

  # Make sure I can use openvpn as a user
  security = {
    sudo.extraConfig = ''
      ${defaultUser} localhost = (root) NOPASSWD: ${pkgs.openvpn}/bin/openvpn
     '';
     setuidPrograms = [ "pmount" "pumount" "mount" "umount" ];
   };

}
