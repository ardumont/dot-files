# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias lla='ls -lA'
alias l='ls -CF'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias rgrep='rgrep --color=auto'

# emacs for the win
alias em='emacsclient -nw'
alias emc='emacsclient -c'

# vagrant
alias v='vagrant'

#### Alias for changing directory

# cd ..
alias cd..='cd ..'
alias cd.='cd .'

# cd ~/org
alias cdo='cd $HOME/org'

# cd ~/bin
alias cdb='cd $HOME/bin'

# cd ~/work/bin
alias cdwb='cd $HOME/work/bin'

# cd ~/.m2
alias cdm2='cd $HOME/.m2'

# cd ~/.ssh
alias cdssh='cd $HOME/.ssh'

# cd ~/repo/perso
alias cdp='cd $HOME/repo/perso'

# cd ~/repo/perso/lazyposse
alias cdl='cd $HOME/repo/perso/lazyposse'

# cd ~/repo/pro/wikeo
alias cdw='cd $HOME/repo/pro/wikeo'

################ Git aliases

# gitk --all
alias gitka='gitk --all'

# git status
alias gst='git status'

# git commit -v
alias gci='git commit -v'

# git diff
alias gdf='git diff'

# pretty log
alias gl='git log --oneline --graph --remotes --decorate --all'

# git fetch
alias gf='git fetch'
alias gfa='git fetch --all'
