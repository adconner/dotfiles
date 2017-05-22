export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_CONFIG_DIRS=/etc/xdg/
export XDG_CACHE_HOME=$HOME/.cache
export MPD_HOST=/run/mpd/socket
export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.cabal/bin:$PATH"

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

cd ~
/usr/bin/xrdb -load ~/.Xresources &
/usr/bin/setxkbmap dvorak && /usr/bin/xmodmap ~/.Xmodmap &
xset +fp /usr/share/fonts/local && xset fp rehash &

sh ~/.fehbg &
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
# unclutter -jitter 200 -grab -idle 10 &
unclutter -grab -idle 10 &
# syndaemon -k -t -i 10 -R -d &
# syndaemon -k -i 5 -R -d &
# syndaemon -k -i 1 -R -d &

renice -n -10 $$

# This must be last
exec xmonad

# vim: ft=sh
