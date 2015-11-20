# set up non-script environment vars

export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.cabal/bin:$PATH"

export BROWSER='qutebrowser'
export EDITOR='nvim'
export VISUAL='nvim'
export PAGER='less'
export LESS='-F -g -i -M -R -S -w -X -z-4'
export LANG='en_US.UTF-8'

# export CDPATH="."
export HISTFILE=$XDG_CACHE_HOME/zsh/zsh_history
export LESSHISTFILE=$XDG_CACHE_HOME/lesshist
export MAIL='~/.mail'

export WORDCHARS=$(echo $WORDCHARS | tr -d '/')

# Execute code that does not affect the current session in the background.
# {
  # # Compile the completion dump to increase startup speed.
  # zcompdump="$XDG_CACHE_HOME/zsh/zcompdump"
  # if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
  #   zcompile "$zcompdump"
  # fi
# } &!

# cachedir=/tmp/$(whoami)
# if [[ ! -d $cachedir ]]; then
#   mkdir -p -m 700 "$cachedir"
# fi

# if [[ ! -a $XDG_CACHE_HOME ]]; then
#   ln -s $cachedir $XDG_CACHE_HOME
#   mkdir -p $XDG_CACHE_HOME/{zsh,vim}
# fi

mkdir -p -m 700 $(readlink $XDG_CACHE_HOME || echo $XDG_CACHE_HOME)

if (( $+commands[fortune] )); then
  # fortune -a
  # print
  startx
fi

