## installtool.tcl
##
## Code in this file is used at install time to generate a new installtool
## binary that can then be used to query information about the installed
## application as well as check for and install updates.
##
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

namespace eval ::installtool {}

proc ::installtool::check {} {
    global info
    global remote

    set force $info(ForceUpdate)

    if {![array exists remote]} { ::installtool::GetUpdateInfo }

    if {![info exists info(UpdateVersion)]} {
        set info(UpdateVersion) [lindex $info(UpdateVersions) end]
    }

    array set _info $remote($info(UpdateVersion))
    set info(UpdateDisplayVersion) $_info(DisplayVersion)

    if {$info(UpdateVersion) ni [array names remote]} { return 0 }
    if {!$force && [vercmp $info(Version) $info(UpdateVersion)] >= 0} {
        return 0
    }
    return 1
}

proc ::installtool::get {} {
    global info
    global remote

    if {![::installtool::check]} { return 0 }

    array set _info $remote($info(UpdateVersion))

    if {[info exists _info($info(Platform))]} {
        set url $_info($info(Platform))
    } elseif {[info exists _info(All)]} {
        set url $_info(All)
    } else {
        return 0
    }

    if {![regexp {(http|https)://} $url]} {
        if {![info exists _info(URL)]} { return 0 }
        set url [string trimright $_info(URL) /]/$url
    }

    set info(UpdateDownloadURL) $url

    ::installtool::DownloadUpdate $url

    return 1
}

proc ::installtool::update {} {
    global info

    if {![::installtool::check]} { return 0 }

    set force $info(ForceUpdate)
    if {!$force && [vercmp $info(Version) $info(UpdateVersion)] >= 0} {
        return 0
    }

    if {![::installtool::get]
        || ![info exists info(UpdateInstaller)]
        || ![file exists $info(UpdateInstaller)]} { return 0 }

    set ::status   "Installing update..."
    set ::progress 0

    exec $info(UpdateInstaller)
    file delete $info(UpdateInstaller) $info(Temp)
    return 1
}

proc ::installtool::version {} {
    global info
    global local

    set dir  [::InstallJammer::GetInstallInfoDir]
    set sort {}
    foreach file [glob -nocomplain -type f -dir $dir *.info] {
        unset -nocomplain tmp
        ::InstallJammer::ReadPropertyFile $file tmp

        if {![info exists tmp(Version)]} { continue }
        if {![info exists tmp(VersionString)]} { continue }

        set mtime [file mtime $file]
        if {[info exists tmp(Date)]} { set mtime $tmp(Date) }
        lappend sort [list $mtime $tmp(Version) $tmp(VersionString)]
        set local($tmp(Version)) [array get tmp]
    }

    if {![llength $sort]} { return 0 }

    set list [lindex [lsort -integer -index 0 $sort] end]
    set last [lindex $list 1]
    
    unset -nocomplain tmp
    array set tmp $local($last)

    foreach {var val} $local($last) {
        set info(Previous${var}) $val
    }

    if {![info exists info(Version)]} { set info(Version) $last }
    if {![info exists info(VersionString)]} {
        set info(VersionString) $info(Version)
        if {[info exists tmp(VersionString)]} {
            set info(VersionString) $tmp(VersionString)
        } 
    }

    return 1
}

proc ::installtool::GetUpdateInfo {} {
    global info
    global remote

    if {[array exists remote]} { return }

    package require http

    set url $info(UpdateURL)

    set tok    [http::geturl $url]
    set data   [http::data $tok]
    set status [http::status $tok]
    http::cleanup $tok

    if {$status ne "ok"} { return }

    ::installtool::ParseUpdateInfo $data
    return $data
}

proc ::installtool::ParseUpdateInfo {data} {
    global info
    global remote

    foreach line [split $data \n] {
        if {[string index [string trimleft $line] 0] eq "#"} { continue }
        lappend lines $line
    }
    set data [join $lines \n]

    foreach {appid body} $data {
        if {$appid ne $info(ApplicationID)} { continue }

        set vars(Summary)      "<%AppName%> version <%VersionString%> released."
        set vars(Description)  ""
        set vars(ReleaseNotes) ""
        array set vars $body

        if {![info exists vars(Version)]} { continue }
        if {![info exists vars(DisplayVersion)]} {
            set vars(DisplayVersion) $vars(Version)
        }

        lappend info(UpdateVersions) $vars(Version)
        set remote($vars(Version)) $body
        set remote($vars(DisplayVersion)) $body
    }

    set info(UpdateVersions) [lsort -dict -unique $info(UpdateVersions)]
}

proc ::installtool::DownloadProgress {tok total current} {
    if {$total == 0} { return }
    set ::progress [expr {($current * 100.0) / $total}]
    if {$::info(ConsoleMode)} {
        ::InstallJammer::ConsoleProgressBar $::progress
    }
}

proc ::installtool::DownloadUpdate {url} {
    global info

    package require http

    if {$info(GuiMode)} {
        set top .update
        destroy $top

        toplevel    $top
        wm withdraw $top
        wm title    $top "Downloading Update"
        wm protocol $top WM_DELETE_WINDOW "#"
        wm geometry $top 300x100
        ::InstallJammer::PlaceWindow $top -width 300 -height 100 -anchor center

        grid rowconfigure $top    1 -weight 1
        grid columnconfigure $top 0 -weight 1

        set ::status "Downloading update..."
        ttk::label $top.l -text "Downloading update..." -textvariable ::status
        grid $top.l -row 0 -column 0 -padx 10 -sticky w

        ttk::progressbar $top.p -variable ::progress
        grid $top.p -row 1 -column 0 -padx 10 -pady 5 -sticky ew

        wm deiconify $top
        ::update
    }

    set tok  [http::geturl $url -validate 1]
    set code [http::ncode $tok]
    http::cleanup $tok

    if {$code != 200} { return 0 }

    set file [file join $info(OutputDir) [file tail $url]]
    set fp [open $file w]

    set tok [http::geturl $url -binary 1 -channel $fp \
        -progress ::installtool::DownloadProgress]
    set code   [http::ncode $tok]
    set status [http::status $tok]
    http::cleanup $tok
    close $fp

    if {$info(GuiMode)} { wm withdraw .update }

    if {$code != 200 || $status ne "ok"} {
        file delete $file
        return
    }

    set info(UpdateInstaller) $file

    return $file
}

proc ::installtool::usage {} {
    ::installtool::message "Invalid Arguments" {
installtool <command> ?option ...?

options:
    --appid           <app id>     application ID to use
    --current-version <version>    current version of the given application
    --force                        force update
    --http-proxy      <host:port>  host and port of HTTP proxy server
    --output          <directory>  directory to downloaded installer to
    --url             <url>        URL to fetch the update information from
    --version         <version>    specific version to update to

commands:
    check       check to see if a newer version is available
    get         get the update installer but do not run it
    update      get and update the given application if an update exists
    version     output the current version of the given application
}
    exit 1
}

proc ::installtool::message {title message} {
    global conf

    if {$conf(windows)} {
        tk_messageBox -title $title -message $message
    } else {
        puts $message
    }
}

proc installtool {args} {
    global argv
    global info

    if {[catch {package require Tk} error]} {
        set info(GuiMode)     0
        set info(ConsoleMode) 1
    } else {
        set info(GuiMode)     1
        set info(ConsoleMode) 0
        wm withdraw .
    }

    ::InstallJammer::CommonInit

    if {![llength $args]} { ::installtool::usage }

    set info(OutputDir)   [::InstallJammer::TmpDir]
    set info(ForceUpdate) 0

    set command [string tolower [lindex $args 0]]

    set len [llength $args]
    for {set i 1} {$i < $len} {incr i} {
        set arg [lindex $args $i]

        switch -- $arg {
            "--appid" {
                set info(ApplicationID) [lindex $args [incr i]]
            }

            "--current-version" {
                set info(Version) [lindex $args [incr i]]
            }

            "--force" {
                set info(ForceUpdate) 1
            }

            "--http-proxy" {
                set info(HTTPProxy)     [lindex $args [incr i]]
            }

            "--output" {
                set info(OutputDir)     [lindex $args [incr i]]
            }

            "--url" {
                set info(UpdateURL)     [lindex $args [incr i]]
            }

            "--version" {
                set info(UpdateVersion) [lindex $args [incr i]]
            }

            "--" {
                set info(CommandLine) [lrange $args [incr i] end]
            }

            default {
                if {$command ne "query"} { usage }
                lappend info(QueryProperties) $arg
            }
        }
    }

    if {![info exists info(ApplicationID)]} {
        ::installtool::message "Installtool Error" "No application ID specified"
        exit 1
    }

    if {$command in {check get update} && ![info exists info(UpdateURL)]} {
        ::installtool::message "Installtool Error" "No update URL specified"
        exit 1
    }

    ## Read local version info.
    ::installtool::version

    set result 0
    switch -- $command {
        "check" {
            ## Check for an updated version.
            set result [::installtool::check]
        }

        "get" {
            ## Get the latest installer but don't install.
            set result [::installtool::get]
        }

        "query" {
            ## Query information about the application.
            set result 1
            if {![info exists info(QueryProperties)]} { usage }
            set list {}
            foreach prop $info(QueryProperties) {
                if {$prop eq "InstallDir"} { set prop Dir }
                if {[info exists info(Previous${prop})]} {
                    lappend list $info(Previous${prop})
                }
            }
            set output [join $list \n]
        }

        "update" {
            ## Get the latest installer and run it.
            set result [::installtool::update]
        }

        "version" {
            ## Output the current version.
            set result [::installtool::version]
            set output "$info(Version)\n$info(VersionString)"
        }

        default {
            ::installtool::usage
        }
    }

    if {$result} {
        if {![info exists output]} {
            set output "$info(Version)\n$info(VersionString)"
            if {[info exists info(UpdateVersion)]} {
                set output "$info(UpdateVersion)\n$info(UpdateDisplayVersion)"
                if {[info exists info(UpdateInstaller)]} {
                    append output "\n[file normalize $info(UpdateInstaller)]"
                }
            }
        }
        puts $output
        exit 0
    }
    exit 1
}

if {!$tcl_interactive} { installtool {*}$::argv }
