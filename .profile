# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# for non nixos-machine using nix
[ -f ~/.nix-profile/etc/profile.d/nix.sh ] && source ~/.nix-profile/etc/profile.d/nix.sh

# from man gpg-agent
gpg-connect-agent /bye
GPG_TTY=$(tty)
export GPG_TTY

unset SSH_AUTH_SOCK
export SSH_AUTH_SOCK="/run/user/$(id -u ${USER})/gnupg/S.gpg-agent.ssh"

# Make the user units aware of his/her environment
systemctl --user import-environment GPG_AGENT_INFO SSH_AUTH_SOCK PATH DISPLAY XAUTHORITY
