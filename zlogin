#sourced on login after .zshrc

export PATH=~/bin:~/.local/bin:~/.cabal/bin:$PATH

export EDITOR='vim'
export BROWSER='luakit'
export PAGER='less -R'

# define XDG dirs for definiteness
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_CONFIG_DIRS=/etc/xdg/
export XDG_CACHE_HOME=$HOME/.cache
#export XDG_RUNTIME_DIR

export HISTFILE=$XDG_CACHE_HOME/zsh/zsh_history
export LESSHISTFILE=$XDG_CACHE_HOME/lesshist
export MAIL='~/.mail'

export WORDCHARS=$(echo $WORDCHARS | tr -d '/')
# export RANGER_LOAD_DEFAULT_RC='FALSE'

#set up cache dir
cachetmp=/tmp/austin-`date +%s`
mkdir $cachetmp 
rm -r $XDG_CACHE_HOME
ln -sf $cachetmp $XDG_CACHE_HOME
mkdir $XDG_CACHE_HOME/{zsh,vim}

echo
fortune
