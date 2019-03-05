{ config, pkgs, ... }:

let defaultUser = "tony"; defaultPassword = "dummy";
in {
  programs = {
    zsh.enable = true;
    bash.enableCompletion = true; # for nix-shell
  };

  users = {
    defaultUserShell = "${pkgs.zsh}/bin/zsh";

    # Define a user account. # `dummy` password by default
    extraUsers = [
    { description = "Antoine R. Dumont";
      name = "${defaultUser}";
      group = "users";
      uid = 1000;
      createHome = true;
      home = "/home/${defaultUser}";
      password = "${defaultPassword}";  # for the first time, make sure to not
                                        # be locked out passwordless then
					# change it manually
      extraGroups = [ "wheel" "audio" "video" "vboxusers" "docker"
                      "networkmanager" "dialout" ];
      useDefaultShell = true; }
    { description = "Christelle Héritier";
      name = "chris";
      group = "users";
      uid = 1001;
      createHome = true;
      home = "/home/chris";
      password = "${defaultPassword}";  # for the first time, make sure to not
                                        # be locked out passwordless then
					# change it manually
      extraGroups = [ "audio" "video" "networkmanager" ];
      useDefaultShell = true; }
    ];
  };

  # Make sure I can use openvpn as a user
  security = {
    sudo.extraConfig = ''
      ${defaultUser} localhost = (root) NOPASSWD: ${pkgs.openvpn}/bin/openvpn
     '';
  };
}
