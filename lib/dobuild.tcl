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

unset -nocomplain tmp
installkit::ParseWrapArgs tmp $args

set pwd         [file dirname $tmp(executable)]
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
    proc echo { string } {
        puts  stdout $string
        flush stdout
    }

    foreach file {common.tcl installkit.tcl} {
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

    ## FIXME: Remove this once the SHA1 code is fixed for large installers.
    catch {rename sha1 ""}

    if {[info exists tmp(wrapFiles)] && [llength $tmp(wrapFiles)]} {
        unset -nocomplain sizes
        foreach file $tmp(wrapFiles) {
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
        }

        foreach {id file group size mtime method} $manifest {
            if {![info exists fp($group)]} {
                set archive [file join $outputDir setup[incr i].ijc]
                set fp($group) [miniarc::open crap $archive w]
            }
            Progress $file
            miniarc::addfile $fp($group) $file -name $id -method $method
        }

        foreach f [array names fp] {
            miniarc::close $fp($f)
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
