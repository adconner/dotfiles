source ~/.zlogin

/usr/bin/xrdb -merge ~/.Xresources &
# /usr/bin/xrdb -load ~/.Xresources &
/usr/bin/setxkbmap dvorak && /usr/bin/xmodmap ~/.Xmodmap &
xset +fp /usr/share/fonts/local && xset fp rehash &

# xinput --list-props [id] to see options 
# enable tapping
xinput set-prop "04CA00B1:00 04CA:00B1 Touchpad" 340 1
# natural scrolling
xinput set-prop "04CA00B1:00 04CA:00B1 Touchpad" 320 1

sh ~/bin/fehbg &
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
# unclutter -jitter 200 -grab -idle 10 &
unclutter -grab -idle 10 &
redshift -l 47:9 &
# syndaemon -k -t -i 10 -R -d &
# syndaemon -k -i 5 -R -d &
# syndaemon -k -i 1 -R -d &

# This must be last
exec xmonad

# vim: ft=sh
