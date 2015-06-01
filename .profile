# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

if [ -f $HOME/.gpg-agent-info ]; then

    . $HOME/.gpg-agent-info
    export GPG_AGENT_INFO
    export SSH_AUTH_INFO
    export GPG_TTY=$(tty)

    if [ -f /etc/NIXOS ]; then
        # To work around some issue about ssh (cf. man gpg-agent) in my nixos env
        gpg-connect-agent updatestartuptty /bye 2>&1 >/dev/null
        gpg-connect-agent /bye 2>&1 >/dev/null
    fi

elif [ ! -f /etc/NIXOS ]; then # only on non-nixos environment

    # trigger the gpg agent (and it deals with ssh support too!)
    gpg-agent --daemon --enable-ssh-support \
      --write-env-file "${HOME}/.gpg-agent-info"

fi
