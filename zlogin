export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_CONFIG_DIRS=/etc/xdg/
export XDG_CACHE_HOME=$HOME/.cache
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export MPD_HOST=/home/austin/.cache/mpd/socket

export BROWSER='qutebrowser'
export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export LESS='-F -g -i -M -R -S -w -X -z-4'
export LANG='en_US.UTF-8'

# export CDPATH="."
export HISTFILE=$XDG_CACHE_HOME/zsh/zsh_history
export LESSHISTFILE=$XDG_CACHE_HOME/lesshist
export MAIL='~/.mail'

export WORDCHARS=$(echo $WORDCHARS | tr -d '/')

mkdir -p -m 700 $(readlink $XDG_CACHE_HOME || echo $XDG_CACHE_HOME)
mkdir -p $XDG_CACHE_HOME/zsh

# vim: ft=sh
