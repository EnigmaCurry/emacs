#!/bin/bash

#CEDET 
(cd cedet && make)

#Jabber
pushd emacs-jabber
autoconf
autoreconf
automake --add-missing
./configure
make
popd

#Pymacs
easy_install Pymacs-0.23