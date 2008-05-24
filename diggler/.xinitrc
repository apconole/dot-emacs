# -*-mode:shell-script-*-; Time-stamp: <2008-05-17 12:46:59 (djcb)>
#
# Copyright (C) 1996-2008  Dirk-Jan C. Binnema.

# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# i'm sourcing my env variables here, so they are available even for apps
# started through menus etc.

# my .zshenv should be understandable by bash as well...
if [ -f $HOME/.zshenv ]; then
    . $HOME/.zshenv
fi

if [ -e $HOME/.xbindkeysrc -a -x /usr/bin/xbindkeys ]; then
    xbindkeys &
fi

if [ -e $HOME/.conkyrc -a -x /usr/bin/conky ]; then
	/usr/bin/conky &
fi
