{ config, pkgs, ... }:

{
  users = {
    defaultUserShell = "/var/run/current-system/sw/bin/zsh";

    # Define a user account. Don't forget to set a password with ‘passwd’.
    # we could also set `mutableUsers = false;` and add a `password = pass;` entry (but then git out this file)
    extraUsers = [{
      description = "Antoine R. Dumont";
      name = "tony";
      group = "users";
      uid = 1000;
      createHome = true;
      home = "/home/tony";
      extraGroups = [ "users" "wheel" "audio" "video" "vboxusers" "docker" "networkmanager" "dialout" ];
      useDefaultShell = true;
    }];
  };
}
