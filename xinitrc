if [ -f $HOME/.Xmodmap ]; then
  /usr/bin/xmodmap $HOME/.Xmodmap
fi
/usr/bin/xrdb -load $HOME/.Xresources
