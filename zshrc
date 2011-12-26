# define XDG dirs for definiteness
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_CONFIG_DIRS=/etc/xdg/
export XDG_CACHE_HOME=$HOME/.cache
#export XDG_RUNTIME_DIR

export HISTFILE=$XDG_CACHE_HOME/zsh/zsh_history
export HISTSIZE=1000
export SAVEHIST=1000
export EDITOR='vim'

eval `dircolors -b`

autoload -Uz compinit && compinit
setopt appendhistory extendedglob
unsetopt beep nomatch notify
bindkey -e

zstyle :compinstall filename '/home/austin/.zshrc'

autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
  colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
  eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
  eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
  (( count = $count + 1 ))
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"

export PS1="[$PR_BLUE%n$PR_WHITE@$PR_GREEN%U%m%u$PR_NO_COLOR:$PR_RED%2c$PR_NO_COLOR]%(!.#.$) "
export RPS1="$PR_LIGHT_YELLOW(%D{%m-%d %H:%M})$PR_NO_COLOR"

alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

alias vi="vim"
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'

export LESSHISTFILE=$XDG_CACHE_HOME/lesshist
