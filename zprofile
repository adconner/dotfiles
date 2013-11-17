# sourced on login before .zshrc

export PATH=~/bin:~/.local/bin:~/.cabal/bin:$PATH

# define XDG dirs for definiteness
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_CONFIG_DIRS=/etc/xdg/
export XDG_CACHE_HOME=$HOME/.cache
#export XDG_RUNTIME_DIR

#set up cache dir
# cachetmp=/tmp/austin-`date +%s`
# mkdir $cachetmp 
# rm -r $XDG_CACHE_HOME
# ln -sf $cachetmp $XDG_CACHE_HOME
# mkdir $XDG_CACHE_HOME/zsh

