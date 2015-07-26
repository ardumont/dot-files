# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

pidof gpg-agent 2>&1 > /dev/null
if [ ! $? = 0 ]; then # clean up the mess left behind
    rm -f $HOME/.gpg-agent-info
fi

if [ -f $HOME/.gnupg/.gpg-agent-info ]; then

    . $HOME/.gnupg/.gpg-agent-info
    export GPG_AGENT_INFO
    #export SSH_AUTH_INFO # typo? Need to check on NixOS
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID

    if [ -f /etc/NIXOS ]; then
        # To work around some issue about ssh (cf. man gpg-agent) in my nixos env
        gpg-connect-agent updatestartuptty /bye 2>&1 >/dev/null
        gpg-connect-agent /bye 2>&1 >/dev/null
    fi

else

    # trigger the gpg agent (and it deals with ssh support too!)
    gpg-agent --daemon --enable-ssh-support \
      --write-env-file "${HOME}/.gnupg/.gpg-agent-info"

fi
