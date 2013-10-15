#!/bin/sh
dotf=`pwd`
cd ~
for i in $(ls $dotf | grep -v 'install.sh\|config'); do
  ln -svTi $dotf/$i ~/.$i
done
for i in $(ls $dotf/config); do
  ln -svTi $dotf/config/$i ~/.config/$i
done
cd -
