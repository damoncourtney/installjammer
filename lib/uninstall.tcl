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

    set uninstaller $info(Uninstaller)

    if {$conf(windows)} {
        set uninstaller [string tolower [Normalize $uninstaller]]
    }

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

        set tmpuninstaller $tmp(Uninstaller)
        if {$conf(windows)} {
            set tmpuninstaller [string tolower [Normalize $tmpuninstaller]]
        }

        if {$uninstaller eq $tmpuninstaller} {
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

proc ::InstallJammer::exit { {prompt 0} } {
    global conf
    global info

    if {$info(WizardStarted) && !$info(WizardCancelled)} {
        ::InstallJammer::ExecuteActions "Finish Actions"
    } else {
        ::InstallJammer::ExecuteActions "Cancel Actions"
    }

    ::InstallJammer::CommonExit

    if {$conf(windows) && !$conf(windows98)} {
	## We can't delete a file in-use on Windows, so we'll
	## write out a .bat file to do the cleanup for us.
	## We'll wait a few seconds before deleting the directory
	## to give the uninstaller time to exit.

	set tmp [file dirname [::InstallJammer::TmpDir]]
	set bat [file join $tmp [pid]cleanup.bat]
	set vbs [file join $tmp [pid]sleep.vbs]

	set fp [open $vbs w]
	puts $fp "WScript.Sleep(3000)"
	close $fp

	set fp [open $bat w]
        puts $fp "@echo off"
	puts $fp "start /WAIT wscript //B //E:vbscript [file tail $vbs] > nul"
	puts $fp "rmdir /S /Q \"[::InstallJammer::TmpDir]\" > nul"
	puts $fp "start /B del [file tail $bat] [file tail $vbs] > nul"
	close $fp
	cd $tmp
	exec $bat &
    }

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
        if {$conf(windows)} {
            ## If we're in Windows and we've made it this far,
            ## we're being started from a temp directory.  If
            ## we're in Silent mode, we need to give the original
            ## uninstall a chance to exit.
            after 1000
        }

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

    unset -nocomplain info(Temp)
    unset -nocomplain info(TempRoot)

    set info(RunningInstaller)   0
    set info(RunningUninstaller) 1

    ::InstallJammer::CommonInit

    ## If we're on Windows, we can't delete ourselves, Windows won't allow us.
    ## So, we need to copy ourselves to a temp directory and do the uninstall
    ## from there.
    if {$conf(windows)} {
	set uninstall $conf(exe)
	set test [file join [file dirname $uninstall] .uninstall]
	if {![file exists $test]} {
            ## We're in our original location.  Copy ourselves to
            ## a temporary directory, startup the copy and then exit.
	    set new [::InstallJammer::TmpDir [file tail $uninstall]]
	    file copy $uninstall $new
	    close [open [::InstallJammer::TmpDir .uninstall] w]
	    eval exec [list $new] $argv &
	    ::exit
	}

        ## We're already in a temp directory, so we just set
        ## our temp directory to our own to keep the clutter down.
	set info(Temp) [file dirname $uninstall]
	cd $info(Temp)
    }

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

    ## Check and load the TWAPI extension.
    ::InstallJammer::LoadTwapi

    if {$info(GuiMode)} {
        SourceCachedFile gui.tcl
        InitGui
    }

    ::InstallJammer::ConfigureBidiFonts
}

::InstallJammer::InitUninstall
