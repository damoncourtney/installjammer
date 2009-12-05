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

if {[info exists ::InstallJammer]} { return }

set len [llength $argv]
for {set i 0} {$i < $len} {incr i} {
    set opt [lindex $argv $i]
    if {[string match "--*" $opt]} {
        set _argv($opt) [lindex $argv [incr i]]
    } else {
        lappend args $opt
    }
}

unset -nocomplain _args
installkit::ParseWrapArgs _args $args

set pwd         [file dirname $_args(executable)]
set conf(pwd)   [file dirname [info script]]
set conf(stop)  [file join $pwd .stop]
set conf(pause) [file join $pwd .pause]

file delete -force $conf(stop) $conf(pause)

if {[info exists ::parentThread]} {
    proc echo { string } {
        thread::send $::parentThread [list ::InstallJammer::BuildOutput $string]
        return
    }
} else {
    lappend ::auto_path [file dirname [info nameofexecutable]]

    proc echo { string } {
        puts  stdout $string
        flush stdout
    }

    foreach file {common.tcl} {
        set file [file join $conf(pwd) $file]
        if {[catch { source $file } error]} {
            echo $::errorInfo
        }
    }
}

proc CheckForBuildStop {} {
    global conf

    while {[file exists $conf(pause)]} {
        if {[file exists $conf(stop)]} { Exit }
        after 500
    }
    return 1
}

proc Progress { file in out } {
    CheckForBuildStop

    if {$file ne $::lastfile} {
        echo [list :FILE $file]
        set ::lastin    0.0
        set ::lastfile  $file
        set ::filetotal 0.0
    }

    iincr ::total     [expr {$in - $::lastin}]
    iincr ::filetotal [expr {$in - $::lastin}]
    set ::lastin $in

    set x [expr {round( ($::filetotal * 100.0) / $::sizes($file) )}]
    if {$x != $::lastfiletotal} {
        set ::lastfiletotal $x
        echo [list :FILEPERCENT $x]
    }

    set x [expr {round( ($::total * 100.0) / $::totalSize )}]
    if {$x != $::lasttotal} {
        set ::lasttotal $x
        echo [list :PERCENT $x]
    }
}

proc Progress { file } {
    CheckForBuildStop

    if {[string length $::lastfile]} {
        iincr ::total $::sizes($::lastfile)
        set x [expr {round( ($::total * 100.0) / $::totalSize )}]
        echo [list :PERCENT $x]
    }
    echo [list :FILE $file]
    set ::lastfile $file
}

proc Exit {} {
    if {![info exists ::parentThread]} {
        exit
    } else {
        thread::release [thread::id]
    }
}

catch { 
    set total         0
    set lastfile      ""
    set lasttotal     0
    set totalSize     0
    set lastfiletotal 0

    if {[info exists _args(wrapFiles)] && [llength $_args(wrapFiles)]} {
        unset -nocomplain sizes
        foreach file $_args(wrapFiles) {
            set sizes($file) [file size $file].0
            iincr totalSize $sizes($file)
        }
    }

    if {[info exists _argv(--archive-manifest)]} {
        set outputDir $_argv(--output)

        echo [list :ECHO "Building archives..."]

        set i 0
        file mkdir $outputDir
        set manifest [read_file $_argv(--archive-manifest)]
        foreach {id file group size mtime method} $manifest {
            set sizes($file) $size
            iincr totalSize $size
            lappend files($group) [list $file -name $id -method $method]
        }

        set opts [list -flatheaders 1]
        if {[info exists _args(password)]} {
            lappend opts -password $_args(password)
        }
        foreach group [array names files] {
            set archive [file join $outputDir setup[incr i].ijc]
            set fp [miniarc::open crap $archive w {*}$opts]
            miniarc::addfilelist $fp $files($group) -progress Progress
            miniarc::close $fp
        }
    }

    echo [list :ECHO "Building install executable..."]
    if {[catch {eval ::installkit::wrap -command ::Progress $args}]} {
        echo $::errorInfo
    }
} error

if {$error ne ""} { echo $::errorInfo }

echo ":DONE"

Exit
