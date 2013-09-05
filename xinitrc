cd ~
/usr/bin/xrdb -load ~/.Xresources &
/usr/bin/setxkbmap dvorak && /usr/bin/xmodmap ~/.Xmodmap &
xset +fp /usr/share/fonts/local && xset fp rehash &

sh ~/.fehbg &
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
unclutter -jitter 200 -grab -idle 10 &
# syndaemon -k -t -i 10 -R -d &
# syndaemon -k -i 5 -R -d &
# syndaemon -k -i 1 -R -d &

# This must be last
exec xmonad

# vim: ft=sh
