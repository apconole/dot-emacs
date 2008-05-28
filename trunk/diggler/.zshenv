#!/bin/zsh
#-*-mode:shell-script-*-
# Time-stamp: <2008-05-11 11:18:16 (djcb)>
#
# Copyright (C) 1996-2008  Dirk-Jan C. Binnema.
# URL: http://www.djcbsoftware.nl/dot-zsh.html

# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# .zshenv for Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
# works for zsh >= 4.x; this file should be bash-compatible as well.

# NOTE: we source this file in ~/.gnomerc, so its contents are available for
# all X-clients in our Xsession

export CVS_RSH="ssh"

export LANG=C
export LANGUAGE="$LANG"
export LC_ALL="$LANG"

export DEVHELP_SEARCH_PATH="/usr/local/share/gtk-doc/html:$DEVHELP_SEARCH_PATH"

export EDITOR=emacsclient
export ALTERNATE_EDITOR=emacs
export VISUAL=emacsclient

# the default, can be overridden below
export MACHINE="`hostname`"

# machine-specific stuff
case $MACHINE in
    
    diggler)   
	email="djcb@djcbsoftware.nl";;
    
    evergrey)  # work, linux workstation
	email="dirk-jan.binnema@nokia.com";
	export http_proxy="http://172.16.42.133:8080"
	;;

    mindcrime)  # main machine @ home
	export NNTPSERVER="news.kolumbus.fi"
	export MAILDIR=$HOME/.Maildir/todo
	email="djcb@djcbsoftware.nl";;

   
    wintersun) # work, linux laptop
	email="dirk-jan.binnema@nokia.com";
	export http_proxy="http://172.16.42.133:8080"
	export NNTPSERVER="nntpcache.nokia.com"
	;;

    www.artolive.com)	
	email="djcb@artolive.com";
	MACHINE="artolive" # override
	;;
esac

export EMAIL=$email
export NAME=$fullname
export DEBFULLNAME=$fullname
export DEBEMAIL=$email

#
# the end.
#
