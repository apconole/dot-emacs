#!/bin/zsh
#-*-mode:shell-script-*-
# Time-stamp: <2008-05-09 09:29:53 (djcb)>
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

# .zshrc for Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
# works for zsh >= 4.x

################################################################################
#  environment / paths

# search path for cd
typeset -u cdpathc
cdpath=(.. ~ ~/src ~/Documents)

# the normal PATH
typeset -u path
path=($HOME/bin $HOME/scripts /usr/local/bin /bin /usr/bin /usr/X11R6/bin /usr/games /sbin)
# for root add sbin dirs to path
if (( EUID == 0 )); then
        path=(/usr/sbin /usr/local/sbin $path)
fi
export path

# man path
typeset -u manpath
manpath=/(usr/local/man /usr/man /usr/share/man \
    /usr/local/share/man /usr/X11R6/man/)
export manpath
################################################################################


################################################################################
# some one-time setup things

# create ~/.tmp if it's not there yet
test -e $HOME/.tmp || mkdir $HOME/.tmp

# secure some stuff a little bit...
chmod -R o-r $HOME/.zshrc $HOME/.zshenv $HOME/.mutt/
################################################################################





################################################################################
# zsh options
setopt autopushd pushdignoredups  # auto add dirs to dirstack, but no dups
setopt autoparamslash             # add / to dir names in completion
setopt completealiases            # autocomplete aliases as well
setopt aliases                    # expand aliases

setopt printexitvalue             # print exit val when != 0
setopt correct correctall         # correct my crappy typing + all args 

setopt appendhistory              # append (not replace) history with session
setopt histignorealldups          # no dups in the history
setopt histnostore                # don't put 'history' in history

bindkey -e                        # use emacs-style keybindings
################################################################################




################################################################################
# aliases
alias mv='nocorrect mv'       # no spelling correction on mv
alias cp='nocorrect cp'       # no spelling correction on cp
alias mkdir='nocorrect mkdir' # no spelling correction on mkdir
alias ls='ls --color=auto'
alias man='LC_CTYPE=C man'
alias perldoc='LC_CTYPE=C perldoc'

# links that point to directories
alias lsa='ls -ld .*'
alias lsmods="find /lib/modules/`uname -r` -name '*.ko'"

# imagemagick installed?
if test -x /usr/bin/mogrify; then
    alias resize300="mogrify -resize 300x300"
    alias resize512="mogrify -resize 512x512"
    alias resize1024="mogrify -resize 1024x1024"
    alias resize1400="mogrify -resize 1400x1400"
fi

alias dos2unix='recode ibmpc:lat1'
alias unix2dos='recode la1:imbpc'

# vim installed?
if test -x /usr/share/vim/vimcurrent/macros/less.sh; then
    alias less="/usr/share/vim/vimcurrent/macros/less.sh"
fi

alias svn="EDITOR=\"vim\" svn"
alias svnr="svn info | grep '^Revision:' | sed 's/Revision: //'"
alias svni="svn diff | grep Index"

# apt get
alias sagu="sudo apt-get update"
alias sagdu="sudo apt-get dist-upgrade"
alias sagi="sudo apt-get install"
alias sagr="sudo apt-get remove"



################################################################################






################################################################################
# completion
autoload -U compinit
compinit

# list of completers to use; first we try to expand, complete; if that does
# not work, were checking if it's in the 'ignore' list (we don't want
# to autocomplete rm arguments etc.), and then try to approximate
zstyle ':completion:*' menu select=1 completer _expand \
    _complete _ignored _approximate
#zstyle ':completion:*' menu select=1  _complete _ignored _approximate


# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# don't autocomplete what's already on the line
zstyle ':completion:*:*' ignore-line yes

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*~' '#*' 

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'

# for process (kill etc.)
zstyle ':completion:*:processes' command 'ps -o pid,s,nice,stime,args'


# use caching for the completions
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.tmp/zsh-cache
################################################################################




################################################################################
# prompt
autoload colors zsh/terminfo
colors
setopt prompt_subst
# # Declare an associative array, "_pr_ompt _c_omponents"
typeset -A prc

# # Define common and useful things to put in a prompt
# (MACHINE is set in ~/.zshenv)
prc[abbrevpath]='%{${fg[red]}%}%B%45<...<%~%<<%b%{${fg[default]}%}'
prc[lastresult]='(%{${fg[yellow]}%}%?%{${fg[default]}%}%)'
prc[promptchar]='%(!.#.$)'
prc[timestamp]='%B%{${fg[blue]}%}[%T]%{${fg[default]}%}%b'
prc[user]='%{${fg[red]}%}%n%}%{${fg[default]}%}'
prc[machine]='%{${fg[green]}%}${MACHINE}%}%{${fg[default]}%}'
prc[userspec]='%B%(!.%{${fg[red]}%}.%{${fg[green]}%})%n@%m%{${fg[default]}%}%b'

# # String them together in a readable format
#PROMPT="${prc[timestamp]} ${prc[user]}@${prc[machine]} %# "
PROMPT="${prc[user]}@${prc[machine]} %# "
RPROMPT="${prc[abbrevpath]}"
# # Unclutter the namespace
unset prc
################################################################################







################################################################################
# and finally... ($MACHINE is set in ~/.zshenv)
echo "Welcome to $MACHINE, $USER. Local time is `date +%c`"
echo
#################################### FIN #######################################
