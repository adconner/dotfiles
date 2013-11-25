export PATH="~/bin:~/.local/bin:~/.cabal/bin:$PATH"

# define XDG dirs for definiteness
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_CONFIG_DIRS=/etc/xdg/
export XDG_CACHE_HOME=$HOME/.cache
#export XDG_RUNTIME_DIR

# set up non-script environment vars
export BROWSER='luakit'
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

# Execute code that does not affect the current session in the background.
{
  cachedir=/tmp/$(whoami)
  if [[ ! -d $cachedir ]]; then
    mkdir -p -m 700 "$cachedir"
  fi

  if [[ ! -a $XDG_CACHE_HOME ]]; then
    ln -s $cachedir $XDG_CACHE_HOME
    mkdir -p $XDG_CACHE_HOME/{zsh,vim}
  fi

  # # Compile the completion dump to increase startup speed.
  # zcompdump="$XDG_CACHE_HOME/zsh/zcompdump"
  # if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
  #   zcompile "$zcompdump"
  # fi
} &!

if (( $+commands[fortune] )); then
  fortune -a
  print
fi

