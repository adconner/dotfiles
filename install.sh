#!/bin/sh
for i in $(ls | grep -v 'install.sh\|config'); do
  ln -svTir $i ~/.$i
done
cd config
for i in $(ls); do
  ln -svTir $i ~/.config/$i
done
cd -
