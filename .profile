# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

GPG_AGENT_INFO_PATH=$HOME/.gnupg/.gpg-agent-info

pidof gpg-agent 2>&1 > /dev/null
if [ ! $? = 0 ]; then # clean up the mess left behind
    rm -f $GPG_AGENT_INFO_PATH
fi

if [ -f $GPG_AGENT_INFO_PATH ]; then
    . $GPG_AGENT_INFO_PATH
    export GPG_AGENT_INFO
    #export SSH_AUTH_INFO # typo? Need to check on NixOS
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
else
    # trigger the gpg agent (and it deals with ssh support too!)
    gpg-agent --daemon \
              --enable-ssh-support \
              --write-env-file $GPG_AGENT_INFO_PATH
fi

gpg-connect-agent updatestartuptty /bye 2>&1 >/dev/null
gpg-connect-agent /bye 2>&1 >/dev/null

# Make the user units aware of his/her environment
systemctl --user import-environment GPG_AGENT_INFO SSH_AUTH_SOCK SSH_AGENT_PID PATH DISPLAY XAUTHORITY
