export HISTFILE=~/.zsh_history
export HISTSIZE=1000
export SAVEHIST=1000
eval `dircolors -b`

autoload -Uz compinit && compinit
setopt appendhistory extendedglob
unsetopt beep nomatch notify
bindkey -e

zstyle :compinstall filename '/home/austin/.zshrc'

autoload -U colors && colors

export PS1="[$PR_BLUE%n$PR_WHITE@$PR_GREEN%U%m%u$PR_NO_COLOR:$PR_RED%2c$PR_NO_COLOR]%(!.#.$) "
export RPS1="$PR_LIGHT_YELLOW(%D{%m-%d %H:%M})$PR_NO_COLOR"
export EDITOR="vim"

alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

alias vi="vim"
