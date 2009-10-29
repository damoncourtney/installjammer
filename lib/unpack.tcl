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

if {[threaded]} {
    proc output { line } {
        thread::send $::parentThread [list ::InstallJammer::UnpackOutput $line]
    }
} else {
    proc output {string} {
        global conf

        catch { puts $conf(runlogFp) $string }
        puts stdout $string

        catch { flush $conf(runlogFp) }
        flush stdout
    }
}

proc ::InstallJammer::InstallFiles {} {
    global conf
    global info
    global files
    global groups

    set conf(pct)  0
    set conf(done) 0

    ::InstallJammer::CreateDir $info(InstallDir)

    foreach group $groups {
        output [list :GROUP [$group name] [$group directory]]

        $group install

        ## The group may not have any actual files.
        if {![info exists files($group)]} { continue }

        foreach file $files($group) {
            output [list :FILE [$file destfile] [$file version]]
            if {![$file install]} { return }
        }
    }
}

proc ::InstallJammer::unpack { src dest {permissions "0666"} } {
    global conf
    global info

    if {![::InstallJammer::PauseCheck]} { return }

    if {$conf(rollback) && [file exists $dest]} {
        output [list :ROLLBACK $dest]
        ::InstallJammer::SaveForRollback $dest
    }

    # Extract the file and copy it to its location.
    set ifp [open $src r]
    if {[catch {open $dest w $permissions} ofp]} {
	close $ifp
	return -code error $ofp
    }

    fconfigure $ifp -translation binary -encoding identity
    fconfigure $ofp -translation binary -encoding identity

    if {[info exists conf(eol,[file extension $dest])]} {
        set trans $conf(eol,[file extension $dest])
        if {[llength $trans] == 2} {
            fconfigure $ifp -translation [lindex $trans 0]
            fconfigure $ofp -translation [lindex $trans 1]
        } else {
            fconfigure $ofp -translation [lindex $trans 0]
        }
    }

    set i     0
    set pct   0
    set chunk 4096
    set total $info(TotalSize)

    set break 0
    while {1} {
        if {[set x [read $ifp $chunk]] ne ""} {
            incr conf(done) [string length $x]
            puts -nonewline $ofp $x
        } else {
            set break 1
        }

        if {$break || [incr i] >= 10} {
            set i 0

            if {![::InstallJammer::PauseCheck]} { break }

            if {$total > 0} {
                set pct [expr {round(($conf(done) * wide(100.0)) / $total)}]
                if {$pct != $conf(pct)} {
                    set conf(pct) $pct
                    output ":PERCENT $pct"
                }
            }
        }

        if {$break} { break }
    }

    catch { close $ifp }
    catch { close $ofp }

    return $dest
}

proc ::InstallJammer::InstallLog { string } {
    output [list :LOG $string]
}

proc ::InstallJammer::exit {} {
    global conf

    if {![threaded]} { catch { close $conf(runlogFp) } }

    output ":PERCENT 100"
    output ":DONE"
}

proc ::InstallJammer::UnpackMain {} {
    global conf
    global info

    catch { wm withdraw . }

    ::InstallJammer::CommonInit

    set conf(pwd) [file dirname [info nameofexe]]

    if {![threaded]} {
        set info(Temp) $conf(pwd)
        uplevel #0 [list source [file join $conf(pwd) unpack.ini]]
    }

    set conf(stop)     [::InstallJammer::TmpDir .stop]
    set conf(pause)    [::InstallJammer::TmpDir .pause]
    set conf(rollback) [string match "*Rollback*" $info(CancelledInstallAction)]

    ::InstallJammer::InitSetup
    ::InstallJammer::InitFiles
    ::InstallJammer::UpdateFiles

    if {![threaded]} {
        set conf(vfs) /installkitunpackvfs
        ::InstallJammer::Mount $info(installer) $conf(vfs)
        set conf(runlogFp) [open [TmpDir run.log] w]

        if {$info(InstallHasSolidArchives)} {
            foreach file [glob -nocomplain -dir [TmpDir] solid.*] {
                ::InstallJammer::Mount $file $conf(vfs)
            }
        }

        ::InstallJammer::MountSetupArchives
    }

    if {$conf(Wow64Disabled)} {
        installkit::Windows::disableWow64FsRedirection
    }

    ::InstallJammer::InstallFiles

    ::InstallJammer::exit
}

::InstallJammer::UnpackMain
