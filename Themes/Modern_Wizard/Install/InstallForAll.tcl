## $Id$
##
## BEGIN LICENSE BLOCK
##
## Copyright (C) 2002  Damon Courtney
## 
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## version 2 as published by the Free Software Foundation.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License version 2 for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the
##     Free Software Foundation, Inc.
##     51 Franklin Street, Fifth Floor
##     Boston, MA  02110-1301, USA.
##
## END LICENSE BLOCK

proc CreateWindow.InstallForAll { wizard id } {
    CreateWindow.CustomBlankPane2 $wizard $id

    set base [$id widget get ClientArea]

    grid rowconfigure    $base 0 -weight 1
    grid columnconfigure $base 0 -weight 1

    set frame [ttk::frame $base.frame]
    grid $frame -row 0 -column 0 -sticky new -padx 10 -pady 10

    grid rowconfigure    $frame 0 -weight 1
    grid columnconfigure $frame 0 -weight 1

    ttk::radiobutton $frame.radio1 -variable ::info(InstallForAllUsers) -value 0
    grid $frame.radio1 -row 0 -column 0 -sticky nw -pady 4
    $id widget set CurrentUserText -widget $frame.radio1 -type text

    ttk::radiobutton $frame.radio2 -variable ::info(InstallForAllUsers) -value 1
    grid $frame.radio2 -row 1 -column 0 -sticky nw -pady 4
    $id widget set AllUsersText -widget $frame.radio2 -type text
}
