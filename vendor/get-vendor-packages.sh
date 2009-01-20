#!/bin/bash

#BBDB 2.35
rm -rf bbdb
mkdir bbdb
wget -c "http://downloads.sourceforge.net/bbdb/bbdb-2.35.tar.gz?modtime=1170198722&big_mirror=0"
tar xfvz bbdb-2.35.tar.gz
rm bbdb-2.35.tar.gz
cp bbdb-2.35/lisp/*.el bbdb
rm -rf bbdb-2.35

#ECB 2.32
wget -c "http://superb-east.dl.sourceforge.net/sourceforge/ecb/ecb-2.32.tar.gz"
tar xfvz ecb-2.32.tar.gz
rm ecb-2.32.tar.gz

#NXHTML 
wget -c "http://ourcomments.org/Emacs/DL/elisp/nxhtml/zip/nxhtml-1.75-090112.zip"
unzip nxhtml-1.75-090112.zip
rm nxhtml-1.75-090112.zip

#NXML
wget -c "http://www.thaiopensource.com/download/nxml-mode-20041004.tar.gz"
tar xfvz nxml-mode-20041004.tar.gz
rm nxml-mode-20041004.tar.gz
ln -s nxml-mode-20041004 nxml-mode

#Jabber
#Needs to be compiled
pushd emacs-jabber
autoconf
autoreconf
automake --add-missing
./configure
make
popd
