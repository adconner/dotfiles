#!/bin/sh
for i in $(ls | grep -v install.sh); do
  ln -svTir $i ~/.$i
done
