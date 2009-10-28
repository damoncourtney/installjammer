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

proc ::InstallJammer::UpdateUninstallProgress {} {
    global conf
    global info

    incr0 conf(num)
    set pct [expr ($conf(num) * 100) / $conf(total)]
    if {$pct != $conf(lastPercent)} {
        set info(UninstallPercentComplete) $pct

        if {$info(GuiMode)} {
            ::InstallJammer::UpdateSelectedWidgets
            update
        } elseif {$info(ConsoleMode) && $conf(ShowConsoleProgress)} {
            ::InstallJammer::ConsoleProgressBar $pct
        }
        set conf(lastPercent) $pct
    }
}

proc ::InstallJammer::GetUninstallInfo {} {
    global conf
    global info
    global uninstall

    ## We check first to see if our .info files may be stored
    ## inside the uninstaller itself.  If not, we check for
    ## an InstallJammer registry that we can use.
    set dir   $::installkit::root
    set files [glob -nocomplain -dir $dir *.info]
    set conf(LogsInUninstaller) 1

    if {![llength $files]} {
        set conf(LogsInUninstaller) 0
        set dir   [::InstallJammer::GetInstallInfoDir]
        set files [glob -nocomplain -dir $dir *.info]
    }

    set installdir  $info(InstallDir)
    set uninstaller $info(Uninstaller)

    set conf(uninstall)        $uninstaller
    set conf(UninstallRemoved) 0

    set sort [list]
    foreach file $files {
        ## If there is a .dead file alongside this .info file,
        ## it's our way of marking a file as deleted.
        if {[file exists [file root $file].dead]} { continue }

        set id [file root [file tail $file]]
        ::InstallJammer::ReadPropertyFile $file tmp

        if {![info exists tmp(Uninstaller)]} {
            lappend sort [list $tmp(Date) $id]
            continue
        }

        if {[patheq $installdir $tmp(Dir)]
            || [patheq $uninstaller $tmp(Uninstaller)]} {
            lappend sort [list $tmp(Date) $id]
        }
    }

    set data ""
    foreach list [lsort -integer -index 0 $sort] {
        set id [lindex $list 1]

        lappend conf(UninstallIDs) $id

        set file [file join $dir $id.log]
        if {[file exists $file]} {
            append data [read_file $file]
        }
    }

    set uninstall(:DIR)      {}
    set uninstall(:FILE)     {}
    set uninstall(:REGISTRY) {}
    foreach line [split [string trim $data] \n] {
        if {[info exists done($line)]} { continue }
        set done($line) 1
        lappend uninstall([lindex $line 0]) [lrange $line 1 end]
    }
}

proc ::InstallJammer::CleanupInstallInfoDirs {} {
    global info
    global conf

    if {[string is true -strict $info(Testing)]} { return }

    if {![info exists conf(UninstallIDs)]} { return }
    if {[file exists $info(Uninstaller)]
        && ![file writable $info(Uninstaller)]} { return }
    if {[file exists $info(InstallInfoDir)]
        && ![file writable $info(InstallInfoDir)]} { return }

    debug "Cleaning up install registry..."

    set info(Status) "Cleaning up install registry..."
    ::InstallJammer::UpdateWidgets -update 1

    if {!$conf(UninstallRemoved)} {
        ## There was a problem deleting files, so the uninstall
        ## was not fully removed from the system.  We want to
        ## take any leftover bits from the uninstall and store
        ## them inside the uninstaller.

	debug "Uninstaller was not removed."

        if {$conf(LogsInUninstaller)} {
	    debug "Found logs in uninstaller.  Moving them  to new uninstaller."

            ## We already have logs saved in our uninstall, so
            ## we want to find the first .info file and use it
            ## for our future uninstalls.
            foreach id $conf(UninstallIDs) {
                set file [file join $::installkit::root $id.info]

                if {![info exists found]} {
                    ## Take the first .info file we find and use it.
                    set found $id
                    file copy -force $file [::InstallJammer::TmpDir]
                } else {
                    ## Mark all other info files as dead since we
                    ## have no real way of deleting them from the
                    ## uninstaller.
                    close [open [::InstallJammer::TmpDir $id.dead] w]
                }
            }

            set id $found
        } else {
	    debug "Storing install IDs in uninstaller."

            foreach id $conf(UninstallIDs) {
                set file [file join $info(InstallInfoDir) $id.info]
                if {[file exists $file]} {
                    file copy -force $file [::InstallJammer::TmpDir]
                    break
                }
            }
        }

        set fp [open [::InstallJammer::TmpDir $id.log] w]

        foreach var [array names ::leftovers] {
            foreach list [lreverse $::leftovers($var)] {
                puts $fp [concat $var $list]
            }
        }

        close $fp

        ::InstallJammer::StoreLogsInUninstall
    }

    foreach id $conf(UninstallIDs) {
        foreach ext {.log .ver .info} {
	    set file [file join $info(InstallInfoDir) $id$ext]
	    if {[file exists $file]} {
	    	debug "Deleting $file"
		file delete $file
	    }
        }
    }

    ## If this ApplicationID has no more installations, we can
    ## remove its directory entirely.
    if {[::InstallJammer::DirIsEmpty $info(InstallInfoDir)]} {
    	debug "Deleting empty registry directory $info(InstallInfoDir)."
        catch { file delete -force $info(InstallInfoDir) }
    } else {
    	debug "Will not delete non-empty directory $info(InstallInfoDir)."
    }
}

proc ::InstallJammer::InstallLog {args} {
    ## This is a dummy proc.  We don't actually want
    ## to log anything during an uninstall.
}

proc ::InstallJammer::CleanupTmpDir {} {
    global conf
    global info

    set tmpdir [::InstallJammer::TmpDir]
    if {$conf(windows)} {
        if {[auto_execok wscript] eq ""} {
            set tmp [::InstallJammer::TmpDir cleanup.tcl]
            set fp [open $tmp w]
            puts $fp "set dir [::InstallJammer::TmpDir]"
            puts $fp {
                catch {wm withdraw .}
                set i 0
                while {[file exists $dir] && [incr i] < 600} {
                    file delete -force -- $dir
                    after 100
                }
            }
            close $fp

            set installkit [::InstallJammer::GetCommonInstallkit]
            exec $::env(COMSPEC) /c start $installkit $tmp &
        } else {
            set bat ij[pid]cleanup.bat
            set vbs ij[pid]cleanup.vbs

            set fp [open [file join $info(TempRoot) $vbs] w]
            puts $fp [string map [list @BAT@ $bat @VBS@ $vbs @TMP@ $tmpdir] {
                On Error Resume Next
                WScript.Sleep(10000)
                Set fsObj = WScript.CreateObject("Scripting.FileSystemObject")
                fsObj.DeleteFile("@BAT@")
                fsObj.DeleteFile("@VBS@")
                fsObj.DeleteFolder("@TMP@")
                WScript.Quit(0)
            }]
            close $fp

            set fp [open [file join $info(TempRoot) $bat] w]
            puts $fp "@echo off"
            puts $fp "start /WAIT wscript //B //E:vbscript $vbs > nul"
            close $fp

            cd $info(TempRoot)
            exec $bat &
        }
    } else {
        set tmp [file join $info(TempRoot) cleanup.sh]
        set fp [open $tmp w]
        puts $fp "sleep 3"
        puts $fp "rm -rf $tmpdir $tmp"
        close $fp

        exec [auto_execok sh] $tmp &
    }
}

proc ::InstallJammer::exit { {prompt 0} } {
    global conf
    global info

    if {$info(WizardStarted) && !$info(WizardCancelled)} {
        ::InstallJammer::ExecuteActions "Finish Actions"
    } else {
        ::InstallJammer::ExecuteActions "Cancel Actions"
    }

    ::InstallJammer::CommonExit

    if {!$info(Debugging)} { ::InstallJammer::CleanupTmpDir }

    if {[string is integer -strict $conf(ExitCode)]} { ::exit $conf(ExitCode) }
    ::exit 0
}

proc ::InstallJammer::UninstallMain {} {
    global conf
    global info

    if {$::tcl_platform(platform) eq "unix"} {
        if {$info(RequireRoot) && !$info(UserIsRoot)} {
            ::InstallJammer::Message -title "Root Required" -message \
                "You must be root to run uninstall this application."
            exit 1
        }
    }

    if {$info(SilentMode)} {
        after 1000
        ::InstallJammer::ExecuteActions "Startup Actions"
        ::InstallJammer::ExecuteActions Silent
    } elseif {$info(ConsoleMode)} {
        ::InstallJammer::ExecuteActions "Startup Actions"
        ::InstallJammer::ExecuteActions Console
    } else {
        ::InstallJammer::ExecuteActions "Startup Actions"

        set info(WizardStarted) 1
        ::InstallJammer::CenterWindow $info(Wizard)
        ::InstallJammer::Wizard next
    }
}

proc ::InstallJammer::InitUninstall {} {
    global conf
    global info
    global argv

    catch { wm withdraw . }

    SourceCachedFile common.tcl

    ## Check and load the TWAPI extension.
    ::InstallJammer::LoadTwapi

    unset -nocomplain info(Temp)
    unset -nocomplain info(TempRoot)

    cd [::InstallJammer::TmpDir]

    set info(RunningInstaller)   0
    set info(RunningUninstaller) 1

    ::InstallJammer::CommonInit

    ::InstallJammer::ReadMessageCatalog messages

    set conf(mode)  "UninstallMode"
    set conf(stop)  [::InstallJammer::TmpDir .stop]
    set conf(pause) [::InstallJammer::TmpDir .pause]
    set conf(lastPercent) 0

    set conf(modes) "Standard Silent"
    if {!$conf(windows)} { lappend conf(modes) "Console" }

    array set info {
        ErrorsOccurred            0
        RunningUninstaller        1
        FileBeingUninstalled      ""
        GroupBeingUninstalled     ""
        UninstallPercentComplete  0
    }

    set info(Status) "Preparing to uninstall..."

    SafeArraySet info {
        FileBeingUninstalledText  "Removing <%FileBeingUninstalled%>"
        GroupBeingUninstalledText "Removing <%GroupBeingUninstalled%>"
    }

    ::InstallJammer::ParseCommandLineArguments $::argv

    if {$info(GuiMode)} {
        SourceCachedFile gui.tcl
        InitGui
    }

    ::InstallJammer::CommonPostInit

    ::InstallJammer::ConfigureBidiFonts
}

::InstallJammer::InitUninstall
