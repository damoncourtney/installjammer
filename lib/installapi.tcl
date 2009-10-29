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

namespace eval ::InstallAPI {}

proc ::InstallAPI::AddExitScript { args } {
    global conf

    ::InstallAPI::ParseArgs _args $args {
        -script  { string 1 }
    }

    set script [string trim $_args(-script)]
    if {[lsearch -exact $conf(ExitScripts) $script] < 0} {
        lappend conf(ExitScripts) $script
    }
}

proc ::InstallAPI::AddInstallInfo { args } {
    ::InstallAPI::ParseArgs _args $args {
        -key   { string 1 }
        -value { string 1 }
    }

    global conf
    lappend conf(APPLOG) $_args(-key) $_args(-value)
}

proc ::InstallAPI::AddLanguage { args } {
    ::InstallAPI::ParseArgs _args $args {
        -language     { string 1 }
        -languagecode { string 1 }
    }

    variable ::InstallJammer::languages
    variable ::InstallJammer::languagecodes
    set languages($_args(-language)) $_args(-languagecode)
    set languagecodes($_args(-languagecode)) $_args(-language)
}

proc ::InstallAPI::CommandLineAPI { args } {
    ::InstallAPI::ParseArgs _args $args {
        -do         { choice  1 "" {check exists}}
        -option     { string  1 }
    }

    variable ::InstallJammer::CommandLineOptions
    variable ::InstallJammer::PassedCommandLineOptions

    set opt [string tolower [string trimleft $_args(-option) -/]]
    if {$_args(-do) eq "exists"} {
        return [info exists CommandLineOptions($opt)]
    }

    if {$_args(-do) eq "check"} {
        return [info exists PassedCommandLineOptions($opt)]
    }
}

proc ::InstallAPI::ComponentAPI { args } {
    ::InstallAPI::ParseArgs _args $args {
        -components { string  1 }
        -active     { boolean 0 }
        -updateinfo { boolean 0 1 }
    }

    global conf
    variable ::InstallJammer::Components

    set components $_args(-components)
    if {![llength $components]} { return }

    set allComponents [Components children recursive]
    if {$components eq "all"} { set components $allComponents }

    foreach component $components {
        set component [string trim $component]
        set id [::InstallAPI::FindComponent -component $component]
        if {$id eq ""} {
            return -code error "invalid component \"$component\""
        }

        set id [$id id]
        lappend componentIds $id
        if {[info exists _args(-active)]} {
            if {$_args(-active)} {
                debug "Activating component $component"
            } else {
                debug "Deactivating component $component"
            }
            $id active $_args(-active)

            set include [$id get IncludeComponents]
            if {$include ne ""} {
                foreach comp [split $include \;] {
                    ## A ! applied to the component means that it should
                    ## be tightly-coupled with the primary component and
                    ## should be deactivated as the primary is deactivated.
                    ## Any other component will be activated when the primary
                    ## is activated, but its state will not change if the
                    ## primary is deactivated.
                    set comp [string trim $comp]
                    set hard [expr {[string index $comp 0] eq "+"}]
                    set realhard [expr {[string index $comp 0] eq "!"}]
                    if {$hard || $realhard} {
                        set comp [string range $comp 1 end]
                    }
                    set comp [::InstallAPI::FindComponent -component $comp]
                    if {$comp eq ""} { continue }
                    if {$_args(-active)} {
                        debug "Activating included component $comp"
                        lappend componentIds $comp
                        $comp active $_args(-active)
                        if {$realhard && [info exists conf(ComponentTree)]} {
                            set tree $conf(ComponentTree)
                            if {[$tree exists $comp]} {
                                $tree itemconfigure $comp -state disabled
                            }
                        }
                    } elseif {$hard || $realhard} {
                        debug "Deactivating included component $comp"
                        lappend componentIds $comp
                        $comp active $_args(-active)
                        if {$realhard && [info exists conf(ComponentTree)]} {
                            set tree $conf(ComponentTree)
                            if {[$tree exists $comp]} {
                                $tree itemconfigure $comp -state normal
                            }
                        }
                    }
                }
            }
        }
    }

    foreach component $componentIds {
        set group [$component get ComponentGroup]

        set var $component
        if {$group ne ""} { set var $group }

        if {![info exists Components($var)] || $Components($var) eq ""} {
            set checked [$component get Checked]
            if {[$component get RequiredComponent]} { set checked 1 }

            if {$group eq ""} {
                set Components($var) [string is true $checked]
            } elseif {$checked} {
                set Components($var) $component
            }
        } else {
            if {$group eq ""} {
                set Components($var) [string is true [$component active]]
            } elseif {[$component active]} {
                set Components($var) $component
            }
        }

        if {[$component active] && $group ne ""} {
            ## If this component is active and part of a group, we need
            ## to walk all of the other components and disable any that
            ## share the same group.
            set grouped {}
            foreach obj $allComponents {
                if {$obj eq $component} { continue }
                if {$group eq [$obj get ComponentGroup]} {
                    lappend grouped $obj
                }
            }
            ::InstallAPI::ComponentAPI -components $grouped -active 0 
        }
    }

    if {$_args(-updateinfo)} { ::InstallJammer::UpdateInstallInfo }
}

proc ::InstallAPI::ConfigAPI { args } {
    ::InstallAPI::ParseArgs _args $args {
        -usenativefiledialog      { boolean 0 }
        -usenativemessagebox      { boolean 0 }
        -usenativedirectorydialog { boolean 0 }
    }

    global conf

    if {$conf(unix) && !$conf(osx)} {
        ## UNIX platforms have no native dialog, so no matter
        ## what we're told, always use our own dialogs.
        foreach arg [array names _args -usenative*] {
            set _args($arg) 0
        }
    }

    if {[info exists _args(-usenativefiledialog)]} {
        set conf(NativeChooseFile) $_args(-usenativefiledialog)
    }

    if {[info exists _args(-usenativemessagebox)]} {
        set conf(NativeMessageBox) $_args(-usenativemessagebox)
    }

    if {[info exists _args(-usenativedirectorydialog)]} {
        set conf(NativeChooseDirectory) $_args(-usenativedirectorydialog)
    }
}

proc ::InstallAPI::CopyObject { args } {
    ::InstallAPI::ParseArgs _args $args {
        -object    { string 1 }
        -newobject { string 0 }
    }

    set obj [::InstallJammer::ID $_args(-object)]
    if {![::InstallJammer::ObjExists $obj]} {
        return -code error "object \"$obj\" does not exist"
    }

    if {[info exists _args(-newobject)]} {
        set new ::$_args(-newobject)
        if {[::InstallJammer::ObjExists $new]} {
            return -code error "object \"$new\" already exists"
        }
    } else {
        set new ::[::InstallJammer::uuid]
    }

    set opts {}
    foreach {opt val} [$obj serialize] {
        if {$opt eq "-id"} { continue }
        if {$opt eq "-alias"} { continue }
        lappend opts $opt $val
    }

    eval [list [$obj class] $new] $opts

    $obj properties props

    set opts {}
    foreach {prop val} [array get props] {
        if {$prop eq "Alias"} { continue }
        lappend opts $prop $val
    }
    $new set $opts

    set map [list $obj, [namespace tail $new],]
    foreach lang [::InstallJammer::GetLanguageCodes] {
        upvar #0 ::msgcat::Msgs_$lang msgs
        foreach {msg string} [array get msgs $obj,*] {
            set msgs([string map $map $msg]) $string
        }
    }

    return $new
}

proc ::InstallAPI::DestroyWidget { args } {
    if {![::InstallJammer::InGuiMode]} { return }

    ::InstallAPI::ParseArgs _args $args {
        -widgets { string 1 }
    }

    foreach w $_args(-widgets) {
        set widget [::InstallAPI::GetWidgetPath -frame 1 -widget $w]
        if {$widget ne ""} { destroy $widget }
    }
}

proc ::InstallAPI::EncodeURL { args } {
    ::InstallAPI::ParseArgs _args $args {
        -url { string 1 }
    }

    global conf

    variable urlEncodeMap
    if {![info exists urlEncodeMap]} {
        for {set i 0} {$i < 256} {incr i} {
            set c [format %c $i]
            if {$c eq "?" || $c eq "=" || $c eq "&"} { continue }
            if {![string match {[a-zA-Z0-9]} $c]} {
                lappend urlEncodeMap $c %[format %02x $i]
            }
        }
    }

    lassign [split $_args(-url) ?] url query
    if {$query ne ""} {
        set query [string map $urlEncodeMap $query]
        append url "?$query"
    }

    if {[file exists $_args(-url)]} {
        if {$conf(windows)} {
            return "file:///$url"
        } else {
            return "file://$url"
        }
    }

    return $url
}

proc ::InstallAPI::ErrorMessage { args } {
    ::InstallAPI::ParseArgs _args $args {
        -title   { string 0 "Install Error" }
        -message { string 1 }
    }

    ::InstallJammer::Message -icon "error" \
        -title $_args(-title) -message $_args(-message)
}

proc ::InstallAPI::EnvironmentVariableExists { args } {
    ::InstallAPI::ParseArgs _args $args {
        -variable { string 1 }
    }

    return [info exists ::env($_args(-variable))]
}

proc ::InstallAPI::ExecuteAction { args } {
    ::InstallAPI::ParseArgs _args $args {
        -action          { string  1 }
        -parent          { string  0 "" }
        -checkconditions { boolean 0 1 }
    }

    ::InstallJammer::ExecuteActions $_args(-action) \
        -parent $_args(-parent) -conditions $_args(-checkconditions)
}

proc ::InstallAPI::Exit { args } {
    global info

    ::InstallAPI::ParseArgs _args $args {
        -exittype { choice 0 "finish" {cancel finish} }
    }

    if {$_args(-exittype) eq "cancel"} {
        set info(WizardCancelled) 1
    }

    foreach command {::Exit ::InstallJammer::exit ::exit} {
        if {[::InstallJammer::CommandExists $command]} {
            $command
        }
    }
}

proc ::InstallAPI::FetchURL { args } {
    ::InstallAPI::ParseArgs _args $args {
        -blocksize           { string 0 "8192" }
        -file                { string 0 "" }
        -progressvirtualtext { string 0 }
        -proxyhost           { string 0 "" }
        -proxyport           { string 0 "" }
        -statusvirtualtext   { string 0 "Status" }
        -timeout             { string 0 }
        -url                 { string 1 }
        -variable            { string 0 "" }
        -virtualtext         { string 0 "" }
    }

    package require http

    set urls  [split $_args(-url) \;]
    set len   [llength $urls]
    set total 0

    set files        $_args(-file)
    set variables    $_args(-variable)
    set virtualtexts $_args(-virtualtext)

    if {$len > 1} {
        set files        [split $_args(-files) \;]
        set variables    [split $_args(-variable) \;]
        set virtualtexts [split $_args(-virtualtext) \;]
    }

    http::config -proxyhost $_args(-proxyhost)
    http::config -proxyport $_args(-proxyport)

    if {[info exists _args(-progressvirtualtext)]} {
        ::InstallAPI::SetVirtualText \
            -virtualtext $_args(-progressvirtualtext) -value 0 -autoupdate 1

        foreach url $urls {
            if {[catch { http::geturl $url -validate 1 } tok]} {
                if {$_args(-returnerror)} {
                    return -code error $tok
                }

                set error $tok
                return 0
            }

            upvar #0 $tok state
            incr total $state(totalsize)
            http::cleanup $tok
        }

        ::InstallJammer::StartProgress \
            ::info($_args(-progressvirtualtext)) $total
    }

    set i 0
    foreach url $urls file $files var $variables virtualtext $virtualtexts {
        set file        [string trim $file]
        set virtualtext [string trim $virtualtext]

        set opts [list -blocksize $_args(-blocksize)]

        if {$file ne ""} {
            set fp [open $file w]
            fconfigure $fp -translation binary
            lappend opts -channel $fp
        }

        if {$total > 0} {
            ::InstallJammer::ResetProgress
            lappend opts -progress ::InstallJammer::UpdateProgress
        }

        if {$_args(-statusvirtualtext) ne ""} {
            set varName ::info($_args(-statusvirtualtext))

            if {$len == 1} {
                set $varName "<%DownloadingFilesText%>"
            } else {
                set $varName "<%DownloadingFilesText%> ([incr i]/$len)"
            }
        }

        if {[catch { eval http::geturl [list $url] $opts } token]} {
            if {[info exists fp]} {
                close $fp
                file delete -force $file
            }

            if {$_args(-returnerror)} {
                return -code error $token
            }

            set error $token
            return 0
        }

        upvar #0 $token state

        if {[info exists fp]} { close $fp }

        if {$state(status) ne "ok" && $file ne ""} {
            file delete -force $file
        }

        if {$state(status) eq "ok" && [http::ncode $token] == 302} {
            ## This URL is a redirect.  We need to fetch the redirect URL.

            array set meta $state(meta)
            http::cleanup $token

            if {![info exists meta(Location)]} {
                return -code error "302 Redirect without new Location"
            }

            set _args(-url) $meta(Location)
            debug "URL Redirected to $_args(-url)."

            eval ::InstallAPI::FetchURL [array get _args]

            continue
        }

        if {$var ne ""} {
            upvar 1 $var body
            set body $state(body)
        }

        if {$virtualtext ne ""} {
            set ::info($virtualtext) $state(body)
        }

        http::cleanup $token
    }

    return 1
}

proc ::InstallAPI::FindApplication { args } {
    ::InstallAPI::ParseArgs _args $args {
        -all            { boolean 0 0 }
        -prompt         { boolean 0 0 }
        -separator      { string  0 }
        -searchpath     { string  1 }
        -expression     { string  0 }
        -promptmessage  { string  0 ""}
    }

    global info 

    if {[info exists _args(-versionvar)]} {
        upvar 1 $_args(-versionvar) versions
        set versions {}
    }

    set dirs {}
    foreach path [split [::InstallJammer::SubstText $_args(-searchpath)] \;] {
        set path [string trim $path]
        if {$path eq ""} { continue }

        if {[string match "HKEY_*" $path]} {
            set list [split $path \\]
            set key  [join [lrange $list 0 end-1] \\]
            set val  [lindex $list end]

            set keys [list $key]
            if {[string first "*" $key] > -1 || [string first "|" $key] > -1} {
                set keys [::InstallAPI::GetWindowsRegistryKeys -key $key]
            }

            foreach key $keys {
                if {[catch { registry get $key $val } dir]} { continue }
                if {[file exists $dir]} { lappend dirs $dir }
            }
        } elseif {[string match "ENV *" $path]} {
            set var [lindex $path 1]
            set sep [lindex $path 2]
            if {$sep eq ""} { set sep $info(PathSeparator) }
            if {$sep eq "colon"} { set sep : }
            if {$sep eq "semicolon"} { set sep \; }
            if {[info exists ::env($var)]} {
                eval lappend dirs [split $::env($var) $sep]
            }
        } elseif {[file exists $path]} {
            lappend dirs $path
        } else {
            foreach dir [glob -nocomplain -type d $path] {
                lappend dirs $dir
            }
        }
    }

    set args {}
    if {[info exists _args(-separator)]} {
        lappend args -separator $_args(-separator)
    }
    if {[info exists _args(-expression)]} {
        lappend args -expression $_args(-expression)
    }
    
    set return {}
    foreach dir $dirs {
        set path [file normalize $dir]
        if {[info exists done($path)]} { continue }
        set done($path) 1

        set vargs [concat [list -path $path] $args]
        if {[eval ::InstallAPI::ValidateApplication $vargs]} {
            lappend return $dir
            if {!$_args(-all)} { return $return }
        }
    }

    if {[llength $return]} { return $return }

    if {$_args(-prompt)} {
        set msg $_args(-promptmessage)
        set dir [::InstallAPI::PromptForDirectory -message $msg]

        set vargs [concat [list -path $dir] $args]
        if {[eval ::InstallAPI::ValidateApplication $vargs]} {
            return [list $dir]
        }
    }
}

proc ::InstallAPI::FindComponent { args } {
    ::InstallAPI::ParseArgs _args $args {
        -component { string 1 }
    }

    set comp [string trim $_args(-component)]
    set obj [::InstallAPI::FindObjects -alias $comp]
    if {$obj eq ""} {
        set obj [::InstallAPI::FindObjects -type component -name $comp]
    }
    return $obj
}

proc ::InstallAPI::FindObjects { args } {
    ::InstallAPI::ParseArgs _args $args {
        -active    { boolean 0 }
        -alias     { string  0 }
        -component { string  0 }
        -glob      { boolean 0 }
        -name      { string  0 }
        -parent    { string  0 }
        -type      { string  0 }
    }

    if {[info exists _args(-alias)]} {
        set id [::InstallJammer::ID $_args(-alias)]
        if {[::InstallJammer::ObjExists $id]} { return $id }
        return
    }

    set check 0
    foreach x {type name active parent component} {
        set chk${x} 0
        if {[info exists _args(-$x)]} {
            incr check
            set chk${x} 1
            set $x $_args(-$x)
        }
    }

    if {!$chktype} {
        set type "all"
        set objects [::obj::object instances]
    } else {
        set type [string tolower $type]
        incr check -1
        set chktype 0
        switch -- $type {
            "file" { set class File }
            "pane" { set class Pane }
            "action" { set class Action }
            "filegroup" { set class FileGroup }
            "component" { set class Component }
            "setuptype" { set class SetupType }
            "condition" { set class Condition }
            "actiongroup" { set class ActionGroup }

            default {
                return -code error [BWidget::badOptionString type $_args(-type)\
                    {action actiongroup pane file filegroup component
                        setuptype condition}]
            }
        }
        set objects [::obj::object instances $class]
        if {$type eq "file"} {
            if {$chkcomponent} {
                ## Files don't have a component.
                incr check -1
                set chkcomponent 0
            }
        } else {
            ## All other types besides files have a root
            ## parent object that we don't want to show up.
            set list {}
            foreach obj $objects {
                if {[$obj parent] eq ""} { continue }
                lappend list $obj
            }
            set objects $list
        }
    }

    if {$chkparent} {
        set check   0
        set parent  [::InstallJammer::ID $parent]
        set objects {}
        if {[::InstallJammer::ObjExists $parent]} {
            set objects [$parent children]
        }
    }

    if {!$check} { return $objects }

    set glob 0
    if {$chkname && [info exists _args(-glob)]} { set glob 1 }

    set found {}
    foreach obj $objects {
        if {$chktype && [$obj type] ne $type} { continue }
        if {$chkparent && [$obj parent] ne $parent} { continue }
        if {$chkcomponent && [$obj component] ne $component} { continue }
        if {$chkactive} {
            set state [$obj active]
            if {($state && !$active) || (!$state && $active)} { continue }
        }
        if {$chkname} {
            set objname [$obj name]
            if {$type eq "file"} { set objname [file tail $objname] }
            if {($glob && ![string match $name $objname])
                || (!$glob && $objname ne $name)} { continue }
        }
        lappend found $obj
    }
    return $found
}

proc ::InstallAPI::FindProcesses { args } {
    global conf

    ::InstallAPI::ParseArgs _args $args {
        -pid   { string  0 }
        -glob  { boolean 0 0}
        -name  { string  0 }
        -user  { string  0 }
        -group { string  0 }
    }

    if {$conf(windows)} {
        if {[info exists _args(-pid)]} {
            set pid $_args(-pid)
            return [expr {[twapi::process_exists $pid] ? $pid : ""}]
        }

        set opts {}

        if {$_args(-glob)} {
            lappend opts -glob
        }

        if {[info exists _args(-name)]} {
            lappend opts -name $_args(-name)
        }

        if {[info exists _args(-user)]} {
            lappend opts -user $_args(-user)
        }

        return [eval twapi::get_process_ids $opts]
    } else {
        if {![info exists conf(OldPS)]} {
            set conf(OldPS) [catch { exec ps -C ps }]
        }

        set ps     [list ps]
        set all    0
        set format "pid"

        if {[info exists _args(-pid)]} {
            lappend ps -p $_args(-pid)
        }

        if {[info exists _args(-name)]} {
            if {$conf(OldPS)} {
                set all 1
                lappend format comm
            } else {
                lappend ps -C $_args(-name)
            }
        }

        if {[info exists _args(-user)]} {
            set all 0
            lappend ps -U $_args(-user)
        }

        if {[info exists _args(-group)]} {
            set all 0
            lappend ps -G $_args(-group)
        }

        if {$all} { lappend ps -ae }

        lappend ps -o $format

        if {![catch { eval exec $ps } result]} {
            set pids [list]
            foreach list [lrange [split [string trim $result] \n] 1 end] {
                if {[llength $list] > 1} {
                    set command [file tail [lindex $list 1]]
                    if {$_args(-name) ne $command} { continue }
                }
                lappend pids [lindex $list 0]
            }
            return $pids
        }
    }
}

proc ::InstallAPI::GetComponentsForSetupType { args } {
    ::InstallAPI::ParseArgs _args $args {
        -setuptype  { string  1 }
    }

    if {[::InstallJammer::ObjExists $_args(-setuptype)]} {
        set obj  $_args(-setuptype)
        set name [$obj name]

        if {![$obj is setuptype]} {
            return -code error "$obj is not a valid Setup Type object"
        }
    } else {
        set name $_args(-setuptype)

        set setups [SetupTypes children]
        set obj [::InstallJammer::FindObjByName $name [SetupTypes children]]
        if {$obj eq ""} {
            return -code error "Could not find Setup Type '$name'"
        }
    }

    return [$obj get Components]
}

proc ::InstallAPI::GetInstallSize { args } {
    ::InstallAPI::ParseArgs _args $args {
        -object     { string  0 "" }
        -activeonly { boolean 0 1  }
    }

    set total      0
    set object     [::InstallJammer::ID $_args(-object)]
    set activeonly $_args(-activeonly)

    if {$object eq ""} {
        global info
        set setups [SetupTypes children]
        set object [::InstallJammer::FindObjByName $info(InstallType) $setups]
        if {$object eq ""} { return -1 }
    }

    set filegroups {}
    switch -- [$object type] {
        "setuptype" {
            foreach component [$object get Components] {
                if {$activeonly && ![$component active]} { continue }
                set size [$component get Size]
                if {$size ne ""} {
                    incr total $size
                } else {
                    eval lappend filegroups [$component get FileGroups]
                }
            }
        }

        "component" {
            foreach component [concat $object [$object children recursive]] {
                if {$activeonly && ![$component active]} { continue }
                set size [$component get Size]
                if {$size ne ""} {
                    incr total $size
                } else {
                    eval lappend filegroups [$component get FileGroups]
                }
            }
        }

        "filegroup" {
            set filegroups [list $object]
        }
    }

    foreach filegroup $filegroups {
        if {$activeonly && ![$filegroup active]} { continue }
        set size [$filegroup get Size]
        if {$size eq ""} { set size [$filegroup get FileSize] }
        incr total $size
    }

    return $total
}

proc ::InstallAPI::GetSelectedFiles { args } {
    global info

    ::InstallAPI::ParseArgs _args $args {
        -fileids { boolean 0 0 }
    }

    set setups [SetupTypes children]
    set setup  [::InstallJammer::FindObjByName $info(InstallType) $setups]

    if {$setup eq ""} { return }

    set files [list]
    foreach component [$setup get Components] {
        if {![$component active]} { continue }

        foreach filegroup [$component get FileGroups] {
            if {![$filegroup active]} { continue }

            foreach file [$filegroup children] {
                if {![$file active]} { continue }

                if {$_args(-fileids)} {
                    lappend files $file
                } else {
                    lappend files [$file destfile]
                }
            }
        }
    }

    return $files
}

proc ::InstallAPI::GetSystemPackageManager { args } {
    ::InstallAPI::ParseArgs _args $args {}

    if {[auto_execok rpm] ne ""} { return RPM }
    if {[auto_execok dpkg] ne ""} { return DPKG }
}

proc ::InstallAPI::GetWidgetChildren { args } {
    if {![::InstallJammer::InGuiMode]} { return }

    ::InstallAPI::ParseArgs _args $args {
        -includeparent { boolean 0 0 }
        -widget        { string 1 }
        -window        { string 0 }
    }

    set widget $_args(-widget)
    if {[info exists _args(-window)]} {
        set window $_args(-window)
    } else {
        set window $info(CurrentPane)
    }

    set widget [::InstallAPI::GetWidgetPath -widget $widget -window $window]
    if {$widget eq ""} { return }

    set widgets {}
    if {$_args(-includeparent)} { lappend widgets $widget }

    set class [winfo class $widget]
    if {$class eq "Frame" || $class eq "TFrame"} {
        eval lappend widgets [winfo children $widget]
    }

    return $widgets
}

proc ::InstallAPI::GetWidgetPath { args } {
    global conf
    global info

    if {![::InstallJammer::InGuiMode]} { return }

    ::InstallAPI::ParseArgs _args $args {
        -frame  { boolean 0 0 }
        -widget { string  1 }
        -window { string  0 }
    }

    set window noop
    set widget $_args(-widget)

    if {[info exists info(CurrentPane)]} { set window $info(CurrentPane) }

    ## Look to see if the widget is in the form of PANE.WIDGET
    lassign [split $_args(-widget) .] pane wid
    if {$wid ne ""} {
        set pane [::InstallJammer::ID $pane]
        if {[::InstallJammer::ObjExists $pane]} {
            set widget $wid
            set window $pane
        }
    }

    if {[info exists _args(-window)]} {
        set obj [::InstallJammer::ID $_args(-window)]
        if {[::InstallJammer::ObjExists $obj] && [$obj ispane]} {
            set window $obj
        }
    }

    set name [join [::InstallJammer::ID $widget] ""]

    if {[lsearch -exact $conf(ButtonWidgets) $name] > -1} {
        set name [string tolower [string map {Button ""} $name]]
        set widg [$info(Wizard) widget get $name]
    } else {
        if {[::InstallJammer::IsID $name]
            && [::InstallJammer::ObjExists $name]} {
            set widg [$name window]
        } else {
            set widg [$window widget get $name]
        }

        if {![winfo exists $widg]} { return }
        if {$_args(-frame)} { return $widg }
        set class [winfo class $widg]
        if {$class eq "Frame" || $class eq "TFrame"} {
            foreach w [winfo children $widg] {
                set class [winfo class $w]
                if {$class eq "Label" || $class eq "TLabel"} { continue }
                set widg $w
                break
            }
        }
    }

    return $widg
}

proc ::InstallAPI::GetWindowsRegistryKeys { args } {
    ::InstallAPI::ParseArgs _args $args {
        -key { string 1 }
    }

    set key $_args(-key)

    if {[string first "*" $key] < 0 && [string first "|" $key] < 0} {
        if {[catch { registry keys $_args(-key) } keys]} { return }
        return $keys
    }

    set len    [llength [split $key \\]]
    set keys   [list $key]
    set return {}
    for {set i 0} {$i < [llength $keys]} {incr i} {
        set key  [lindex $keys $i]
        set list [split $key \\]

        if {[string first "*" $key] < 0 && [string first "|" $key] < 0} {
            if {[llength $list] == $len} { lappend return $key }
            continue
        }

        set key [lindex $list 0]
        for {set j 1} {$j < [llength $list]} {incr j} {
            set elem [lindex $list $j]

            if {[string first "|" $elem] > -1} {
                set list [concat {{}} [lrange $list [incr j] end]]
                foreach found [split $elem |] {
                    lappend keys $key\\$found[join $list \\]
                }
                break
            }

            if {[string first "*" $elem] > -1} {
                if {![catch {registry keys $key} foundkeys]} {
                    set list [concat {{}} [lrange $list [incr j] end]]
                    foreach found $foundkeys {
                        if {$elem eq "*" || [string match $elem $found]} {
                            lappend keys $key\\$found[join $list \\]
                        }
                    }
                }
                break
            }
            
            append key "\\$elem"
        }
    }

    return $return
}

proc ::InstallAPI::KillProcess { args } {
    global conf
    global info

    ::InstallAPI::ParseArgs _args $args {
        -pid    { string  0 }
        -glob   { boolean 0 0}
        -name   { string  0 }
        -user   { string  0 }
        -group  { string  0 }
        -signal { string  0 "TERM"}
    }

    set signal [string toupper [string trimleft $_args(-signal) -]]
    unset _args(-signal)

    foreach pid [eval ::InstallAPI::FindProcesses $args] {
        if {$conf(windows)} {
            if {$signal eq "FORCE" || $signal eq "KILL" || $signal eq "9"} {
                twapi::end_process $pid -force
            } else {
                twapi::end_process $pid
            }
        } else {
            exec kill -$signal $pid
        }
    }
}

proc ::InstallAPI::Exit { args } {
    global conf
    global info

    ::InstallAPI::ParseArgs _args $args {
        -exitcode { string 0 }
        -exittype { choice 0 "finish" {cancel finish} }
    }

    if {[info exists _args(-exitcode)]} {
        set conf(ExitCode) $_args(-exitcode)
    }

    if {$_args(-exittype) eq "cancel"} {
        set info(WizardCancelled) 1
    }

    foreach command {::Exit ::InstallJammer::exit ::exit} {
        if {[::InstallJammer::CommandExists $command]} {
            $command
        }
    }
}

proc ::InstallAPI::InstallInstallTool { args } {
    ::InstallAPI::ParseArgs _args $args {
        -file  { string  1 }
    }

    global conf
    global info

    set binary $_args(-file)

    set info(FileBeingInstalled) $binary
    set info(Status) "<%BuildFileText%>"

    set opts   [list -noinstall -o $binary -w [::InstallJammer::BaseInstallkit]]
    set script [::InstallJammer::TmpFile]

    set fp [open $script w]
    puts $fp "set info(ApplicationID) [list $info(ApplicationID)]"
    puts $fp "set info(Version) [list $info(InstallVersion)]"
    foreach file {common.tcl installtool.tcl} {
        puts $fp $::InstallJammer::files($file)
    }
    close $fp

    if {$conf(windows)} {
        lappend opts -company "InstallJammer.com"
        lappend opts -fileversion 1.0
        lappend opts -filedescription "InstallJammer Install Tool"
    }
    lappend opts $script

    ::InstallJammer::CreateDir [file dirname $binary]
    eval ::InstallJammer::Wrap $opts

    if {[file exists $binary]} {
        ::InstallJammer::SetPermissions $binary 00755
        return 1
    }
    return 0
}

proc ::InstallAPI::LanguageAPI { args } {
    ::InstallAPI::ParseArgs _args $args {
        -do       { choice 1 "" "setlanguage" }
        -language { string 0 "" }
    }

    global info

    if {$_args(-do) eq "setlanguage"} {
        set code [::InstallJammer::GetLanguageCode $_args(-language)]
        if {$code eq ""} { return 0 }
        debug "Setting language to $code"
        set info(Language) $code
        ::msgcat::mclocale ""
        ::msgcat::mclocale en $info(Language)
        ::InstallJammer::UpdateWidgets -buttons 1
    }

    return 1
}

proc ::InstallAPI::LoadMessageCatalog { args } {
    ::InstallAPI::ParseArgs _args $args {
        -data     { string 0 }
        -dir      { string 0 }
        -encoding { string 0 }
        -file     { string 0 }
        -language { string 0 }
        -object   { string 0 }
    }

    set code ""
    if {[info exists _args(-language)]} {
        set code [::InstallJammer::GetLanguageCode $_args(-language)]
        if {$code eq ""} {
            return -code error "Invalid language '$_args(-language)'"
        }
    }

    set langs {}
    set files {}
    if {[info exists _args(-dir)]} {
        if {![file exists $_args(-dir)]} {
            return -code error "Directory '$_args(-dir)' does not exist."
        }

        foreach file [recursive_glob $_args(-dir) *.msg] {
            lappend files $file
            if {$code ne ""} {
                lappend langs $code
            } else {
                lappend langs [file root [file tail $file]]
            }
        }
    }

    if {[info exists _args(-file)]} {
        if {![file exists $_args(-file)]} {
            return -code error "File '$_args(-file)' does not exist."
        }

        lappend files $_args(-file)
        if {$code ne ""} {
            lappend langs $code
        } else {
            lappend langs [file root [file tail $_args(-file)]]
        }
    }

    if {[info exists _args(-object)]} {
        set obj [::InstallJammer::ID $_args(-object)]
        if {![::InstallJammer::ObjExists $obj]} {
            return -code error "Object '$_args(-object)' does not exist."
        }
        if {[$obj type] ne "file"} {
            return -code error "Object '$_args(-object)' is not a file object."
        }

        lappend files [$obj srcfile]
        if {$code ne ""} {
            lappend langs $code
        } else {
            lappend langs [file root [file tail [$obj name]]]
        }
    }

    foreach file $files lang $langs {
        set fp [open $file]
        if {[info exists _args(-encoding)]} {
            fconfigure $fp -encoding $_args(-encoding)
        }

        set data [string trim [gets $fp]]
        if {[string index $data 0] eq "#"} {
            catch { eval [list fconfigure $fp] [string trimleft $data #] }
            set data [read $fp]
        } else {
            append data \n[read $fp]
        }

        if {[catch { ::msgcat::mcmset $lang $data } error]} {
            close $fp
            return -code error "error reading message catalog '$file': $error"
        }
        close $fp
    }

    if {[info exists _args(-data)]} {
        if {$code eq ""} {
            return -code error "You must specify -language with -data"
        }
        lappend langs $code
        ::msgcat::mcmset $code $_args(-data)
    }

    return [lsort -unique $langs]
}

proc ::InstallAPI::LocateJavaRuntime { args } {
    global conf
    global info

    ::InstallAPI::ParseArgs _args $args {
        -all           { boolean 0 0 }
        -prefix        { string  0 "Java" }
        -searchpath    { string  0 "Default Search Path" }
        -minversion    { string  0 "" }
        -maxversion    { string  0 "" }
        -requirejdk    { boolean 0 0 }
        -expression    { string  0 "" }
        -validversions { string  0 "" }
        -promptmessage { string  0 "<%LocateJavaRuntime,PromptMessage%>"}
        -statusmessage { string  0 "<%LocateJavaRuntime,StatusMessage%>"}
    }

    set info(Status) [::InstallJammer::SubstText $_args(-statusmessage)]

    set prefix $_args(-prefix)
    set info(${prefix}Found)        0
    set info(${prefix}Home)         ""
    set info(${prefix}Version)      ""
    set info(${prefix}VersionMajor) ""
    set info(${prefix}Executable)   ""
    set info(${prefix}wExecutable)  ""
    set info(${prefix}cExecutable)  ""

    set args {}
    if {$_args(-all)} { lappend args -all 1 }

    set expression {}
    lappend expression "collect _ver in variable versions"
    lappend expression "collect _path in variable foundPaths"

    if {$_args(-requirejdk)} {
        set file "bin/javac"
        if {$conf(windows)} { append file ".exe" }
        lappend expression "exists $file"
    }

    set file "bin/java"
    if {$conf(windows)} { append file ".exe" }
    lappend expression "execute $file -version"
    lappend expression {regexp {java version "([^"]+)} _match _ver}

    if {$_args(-minversion) ne ""} {
        lappend expression "version \$_ver >= $_args(-minversion)"
    }

    if {$_args(-maxversion) ne ""} {
        lappend expression "version \$_ver <= $_args(-maxversion)"
    }

    if {$_args(-validversions) ne ""} {
        lappend expression "test \$_ver in $_args(-validversions)"
    }

    if {$_args(-expression) ne ""} { append expression "\n$_args(-expression)" }

    set paths {}
    foreach path [split $_args(-searchpath) \;] {
        set path [string trim $path]

        if {$path eq "Default Search Path"} {
            foreach var {JAVA_HOME JAVAHOME JDK_HOME JRE_HOME JAVA_ROOT} {
                lappend paths "ENV $var"
            }

            set which [auto_execok java]
            if {$which ne ""} {
                ## Strip the bin/java off.
                lappend paths [file dirname [file dirname $which]]
            }

            if {$conf(windows)} {
                set    key {HKEY_LOCAL_MACHINE\Software\JavaSoft}
                append key {\Java Development Kit|Java Runtime Environment\*}
                lappend paths $key

                set prog [::InstallJammer::WindowsDir PROGRAM_FILES]
                lappend paths [file join $prog Java jdk*]
                lappend paths [file join $prog Java jre*]
            } else {
                lappend paths \
                    /usr/java/jdk \
                    /usr/lib/java \
                    /usr/lib/jvm \
                    /usr/lib/jvm/jre \
                    /usr/lib/java-1.4.0 \
                    /usr/lib/java-1.4.1 \
                    /usr/lib/java-1.4.2 \
                    /usr/lib/java-1.5.0
            }
        } elseif {$path eq "Prompt User"} {
            lappend args -prompt 1
            lappend args -promptmessage \
                [::InstallJammer::SubstText $_args(-promptmessage)]
        } else {
            lappend paths $path
        }
    }

    lappend args -expression $expression
    lappend args -searchpath [join $paths \;]

    set dirs [eval ::InstallAPI::FindApplication $args]

    if {[llength $dirs]} {
        set info(${prefix}AvailableVersions) [lsort -dict -unique $versions]

        set dir  [lindex $dirs 0]
        set ver  [lindex $versions 0]
        set java [file join $dir bin java]
        if {$conf(windows)} { append java ".exe" }

        set info(${prefix}Found)        1
        set info(${prefix}Home)         $dir
        set info(${prefix}Version)      $ver
        set info(${prefix}VersionMajor) [join [lrange [split $ver .] 0 1] .]
        set info(${prefix}Executable)   $java
        set info(${prefix}wExecutable)  $java

        set javaw [file join [file dirname $java] javaw]
        if {$conf(windows)} { append javaw .exe }

        if {[file exists $javaw]} {
            set info(${prefix}wExecutable) $javaw
        }

        set javac [file join [file dirname $java] javac]
        if {$conf(windows)} { append javac .exe }

        if {[file exists $javac]} {
            set info(${prefix}cExecutable) $javac
        }

        return 1
    }

    return 0
}

proc ::InstallAPI::ModifyWidget { args } {
    global conf
    global info
    global hidden

    if {![::InstallJammer::InGuiMode]} { return }

    ::InstallAPI::ParseArgs _args $args {
        -state   { choice 0 "" {disabled hidden normal readonly}}
        -widget  { string 1 }
        -window  { string 0 }
    }

    if {[info exists _args(-window)]} {
        set window $_args(-window)
    } else {
        set window $info(CurrentPane)
    }

    foreach widget [split $_args(-widget) \;] {
        foreach widget [::InstallAPI::GetWidgetChildren -window $window \
            -widget $widget -includeparent 1] {
            if {[info exists _args(-state)]} {
                set state $_args(-state)
                if {$state eq "hidden" && ![info exists hidden($widget)]} {
                    set manager [winfo manager $widget]
                    set options [$manager info $widget]
                    set hidden($widget) \
                        [concat $manager configure $widget $options]
                    $manager forget $widget
                } elseif {$state eq "normal" && [info exists hidden($widget)]} {
                    eval $hidden($widget)
                    unset hidden($widget)
                } else {
                    $widget configure -state $state
                }
            }
        }
    }
}

proc ::InstallAPI::ParseArgs { _arrayName _arglist optionspec } {
    if {[debugging ison] >= 2} { debug "API Call: [info level -1]" }

    upvar 1 $_arrayName array

    if {[expr {[llength $_arglist] % 2}]} {
        if {$_arglist ne "-?"} {
            set proc [lindex [info level -1] 0]
            return -code error "invalid number of arguments passed to $proc:\
                    all arguments must be key-value pairs"
        }
    }

    array set options {
        -subst       { string  0 0 }
        -errorvar    { string  0 }
        -returnerror { boolean 0 }
    }

    array set options $optionspec

    set optionlist [lsort [array names options]]

    set required [list]
    foreach option $optionlist {
        if {[lindex $options($option) 1]} {
            lappend required $option
        } elseif {[llength $options($option)] > 2} {
            set array($option) [lindex $options($option) 2]
        }
    }

    if {(![llength $_arglist] && [llength $required]) || $_arglist eq "-?"} {
        set help "Usage: [lindex [info level -1] 0] ?option value ...?"
        append help "\n\nOptions:"
        foreach option $optionlist {
            set type     [lindex $options($option) 0]
            set required [lindex $options($option) 1]
            set default  [lindex $options($option) 2]
            set choices  [lindex $options($option) 3]

            append help "\n\t"

            if {!$required} { append help ? }
            append help $option

            if {$type eq "choice"} {
                append help " [join $choices " | "]"
            } elseif {$type eq "boolean"} {
                append help " 0 | 1"
            } else {
                append help " [string range $option 1 end]"
            }

            if {$default ne ""} { append help " (Default: $default)" }

            if {!$required} { append help ? }
        }

        return -code error $help
    }

    if {[expr {[llength $_arglist] % 2}]} {
        return -code error "invalid number of arguments"
    }

    foreach {option value} $_arglist {
        set option [string tolower $option]

        if {![info exists options($option)]} {
            return -code error "invalid option $option"
        }

        if {$option eq "-do"} { set value [join [string tolower $value] ""] }
        set array($option) $value

        set type [lindex $options($option) 0]
        if {$type eq "string"} {
            if {$array(-subst)} {
                set array($option) [::InstallJammer::SubstText $value]
            }
        } elseif {$type eq "boolean"} {
            if {![string is boolean -strict $value]} {
                return -code error "invalid boolean value for $option"
            }
        } elseif {$type eq "choice"} {
            set values [lindex $options($option) 3]
            if {[lsearch -exact $values $value] < 0} {
                set x "[string range $option 1 end] value"
                return -code error [BWidget::badOptionString $x $value $values]
            }
        }
    }

    foreach option $required {
        if {![info exists array($option)]} {
            return -code error "missing required option $option"
        }
    }

    unset array(-subst)

    if {![info exists array(-returnerror)]} {
        set array(-returnerror) 1
        if {[info exists array(-errorvar)]} {
            set array(-returnerror) 0
            uplevel 1 [list upvar 1 $array(-errorvar) error]
        }
    }
}

proc ::InstallAPI::PromptForDirectory { args } {
    global conf

    ::InstallJammer::SetDialogArgs ChooseDirectory _args

    ::InstallAPI::ParseArgs _args $args {
        -title         { string 0 "<%PromptForDirectoryTitle%>"}
        -message       { string 0 "<%PromptForDirectoryMessage%>"}
        -newfoldertext { string 0 "<%PromptForDirectoryNewFolderText%>"}
        -variable      { string 0 }
        -initialdir    { string 0 }
        -normalize     { string 0 "platform" }
        -virtualtext   { string 0 }
    }

    set normalize $_args(-normalize)
    unset _args(-normalize)

    if {[info exists _args(-virtualtext)]} {
        set _args(-variable) ::info($_args(-virtualtext))
        unset _args(-virtualtext)
    }

    if {[info exists _args(-variable)]} {
        set varName $_args(-variable)
        upvar 1 $varName dir
        unset _args(-variable)
    }

    if {[info exists _args(-initialdir)]} {
        set dir $_args(-initialdir)
    } elseif {[info exists dir]} {
        set _args(-initialdir) $dir
    }

    foreach x {-title -message -newfoldertext} {
        set _args($x) [::InstallJammer::SubstText $_args($x)]
    }
    set _args(-oktext)      [::InstallJammer::SubstText "<%OK%>"]
    set _args(-canceltext)  [::InstallJammer::SubstText "<%Cancel%>"]

    if {$_args(-usenative)} {
        set res [eval ::ij_chooseDirectory [array get _args]]
    } else {
        unset -nocomplain _args(-usenative)
        if {[llength $conf(ParentWindow)] > 1} {
            wm withdraw [lindex $conf(ParentWindow) end]
            set _args(-parent) [::InstallJammer::TransientParent]
        }

        set res [eval ::ChooseDirectory .__choose_directory [array get _args]]

        if {[llength $conf(ParentWindow)] > 1} {
            wm deiconify [lindex $conf(ParentWindow) end]
        }
    }

    if {$res ne ""} {
        set dir $res
        if {$conf(windows) && [file exists $dir]} {
            set dir [file attributes $dir -longname]
        }

        set dir [::InstallJammer::Normalize $dir $normalize]

        if {[info exists varName]} { set $varName $dir }

        ::InstallJammer::UpdateWidgets

        return $dir
    }
}

proc ::InstallAPI::PromptForFile { args } {
    global conf
    
    ::InstallJammer::SetDialogArgs ChooseFile _args

    ::InstallAPI::ParseArgs _args $args {
        -type             { choice  0 "open" {open save} }
        -title            { string  0 }
        -message          { string  0 }
        -variable         { string  0 }
        -multiple         { boolean 0 }
        -normalize        { string  0 "platform" }
        -filetypes        { string  0 }
        -initialdir       { string  0 }
        -initialfile      { string  0 }
        -virtualtext      { string  0 }
        -defaultextension { string  0 }
    }

    set normalize $_args(-normalize)
    unset _args(-normalize)

    if {[info exists _args(-virtualtext)]} {
        set _args(-variable) ::info($_args(-virtualtext))
        unset _args(-virtualtext)
    }

    if {[info exists _args(-variable)]} {
        set varName $_args(-variable)
        upvar 1 $varName file
        unset _args(-variable)
    }

    if {$_args(-usenative)} {
        set type [string toupper $_args(-type) 0]
        set res  [eval [list ij_get${type}File] [array get _args]]
    } else {
        unset -nocomplain _args(-usenative)

        if {[llength $conf(ParentWindow)] > 1} {
            wm withdraw [lindex $conf(ParentWindow) end]
            set _args(-parent) [::InstallJammer::TransientParent]
        }

        set res [eval ::ChooseFile .__choose_file [array get _args]]

        if {[llength $conf(ParentWindow)] > 1} {
            wm deiconify [lindex $conf(ParentWindow) end]
        }
    }

    if {$res ne ""} {
        set file $res
        if {$conf(windows) && [file exists $file]} {
            set file [file attributes $file -longname]
        }

        set file [::InstallJammer::Normalize $file $normalize]

        if {[info exists varName]} { set $varName $file }

        ::InstallJammer::UpdateWidgets

        return $file
    }
}

proc ::InstallAPI::RestartAsRoot { args } {
    global conf
    global info

    ::InstallAPI::ParseArgs _args $args {
        -promptmessage  { string 0 "<%PromptForRootText%>" }
        -failuremessage { string 0 "<%RequireRootText%>" }
    }

    if {$conf(windows) || $info(UserIsRoot)} { return }

    set cmd [concat [list [info nameofexecutable]] $::argv]
    set msg [::InstallAPI::SubstVirtualText -virtualtext $_args(-promptmessage)]
    
    if {![::InstallJammer::ExecAsRoot $cmd -message $msg]} {
        ::InstallJammer::Message -title "Root Required" -message \
            [::InstallJammer::SubstText $_args(-failuremessage)]
        ::exit 1
    }

    ::exit 0
}

proc ::InstallAPI::RestartProcess { args } {
    global conf

    ::InstallAPI::ParseArgs _args $args {
        -pid    { string  0 }
        -glob   { boolean 0 0}
        -name   { string  0 }
        -user   { string  0 }
        -group  { string  0 }
    }

    set pids [eval ::InstallAPI::FindProcesses $args]

    foreach pid $pids {
        if {$conf(windows)} {
            set cmd [twapi::get_process_commandline $pid]
            twapi::end_process $pid -force
            after 1000 [list twapi::create_process "" -cmdline $cmd]
        } else {
            exec kill -HUP $pid
        }
    }
}

proc ::InstallAPI::ReadInstallInfo { args } {
    global info

    ::InstallAPI::ParseArgs _args $args {
        -array          { string 1 }
        -installid      { string 0 "" }
        -applicationid  { string 1 }
    }

    upvar 1 $_args(-array) array

    set pattern *.info
    if {$_args(-installid) ne ""} { set pattern $_args(-installid).info }

    ::InstallJammer::GetInstallInfoDir
    set dir [file join $info(InstallJammerRegistryDir) $_args(-applicationid)]
    foreach file [glob -nocomplain -dir $dir $pattern] {
        set id [file root [file tail $file]]

        set tmp(ID) $id
        ::InstallJammer::ReadPropertyFile $file tmp
        if {$_args(-installid) ne ""} {
            array set array [array get tmp]
            return $_args(-installid)
        }

        set mtime [file mtime $file]
        if {[info exists tmp(Date)]} { set mtime $tmp(Date) }

        lappend sort [list $mtime $id]

        foreach var [array names tmp] {
            set array($id,$var) $tmp($var)
        }
    }

    if {![info exists sort]} { return }

    set installids {}
    foreach list [lsort -integer -index 0 $sort] {
        lappend installids [lindex $list 1]
    }
    return $installids
}

proc ::InstallAPI::ResponseFileAPI { args } {
    ::InstallAPI::ParseArgs _args $args {
        -do          { choice  1 "" {add read remove write}}
        -file        { string  0 "" }
        -virtualtext { string  0 "" }
    }

    global conf
    global info

    if {$_args(-do) eq "add"} {
        foreach var $_args(-virtualtext) {
            if {[lsearch -exact $conf(SaveResponseVars) [lindex $var 0]] < 0} {
                lappend conf(SaveResponseVars) $var
            }
        }
    }

    if {$_args(-do) eq "remove"} {
        foreach var $conf(SaveResponseVars) {
            if {[lsearch -exact $_args(-virtualtext) [lindex $var 0]] < 0} {
                lappend vars $var
            }
        }
        set conf(SaveResponseVars) $vars
    }

    if {$_args(-do) eq "read" && [file exists $_args(-file)]} {
        ::InstallJammer::ReadPropertyFile $_args(-file) tmp

        foreach list $conf(SaveResponseVars) {
            lassign $list var type
            if {![info exists tmp($var)]} { continue }

            set val $tmp($var)
            if {$type eq "boolean"} {
                if {![string is boolean -strict $val]} { continue }
            } elseif {$type eq "list"} {
                set elems ""
                foreach elem [split $val ,] {
                    lappend elems [string trim $elem]
                }
                set val $elems
            }

            set info($var) $val
        }

        ::InstallJammer::SetupModeVariables
    }

    if {$_args(-do) eq "write" && $_args(-file) ne ""} {
        set fp [open $_args(-file) w]
        foreach list $conf(SaveResponseVars) {
            lassign $list var type

            set val [sub "<%$var%>"]
            if {$val eq "<%$var%>"} { continue }

            if {$type eq "boolean"} {
                if {[string is true -strict $val]} { set val "Yes" }
                if {[string is false -strict $val]} { set val "No" }
            } elseif {$type eq "list"} {
                set val [join $val ,]
            }

            puts $fp "$var: $val"
        }
        close $fp
    }

    return $conf(SaveResponseVars)
}

proc ::InstallAPI::RollbackInstall { args } {
    ::InstallJammer::CleanupCancelledInstall
}

proc ::InstallAPI::SendWindowsNotification { args } {
    global conf

    ::InstallAPI::ParseArgs _args $args {
        -force        { boolean 0 0 }
        -environment  { boolean 0 0 }
        -assocchanged { boolean 0 0 }
    }

    if {!$conf(windows)} { return }

    if {!$_args(-force)} {
        if {$_args(-environment)} {
            ::InstallAPI::AddExitScript -script {
                ::InstallAPI::SendWindowsNotification -force 1 -environment 1
            }
        }

        if {$_args(-assocchanged)} {
            ::InstallAPI::AddExitScript -script {
                ::InstallAPI::SendWindowsNotification -force 1 -assocchanged 1
            }
        }

        return
    }

    if {$_args(-environment)} {
        registry broadcast Environment -timeout 1
    }

    if {$_args(-assocchanged)} {
        package require Ffidl

        if {[info commands ffidl::SHChangeNotify] eq ""} {
            set symbol [ffidl::symbol shell32 SHChangeNotify]
            ffidl::callout ffidl::SHChangeNotify \
                {long unsigned pointer pointer} void $symbol
        }

        set SHCNE_ASSOCCHANGED 0x08000000
        set SHCNF_FLUSH        0x1000

        ffidl::SHChangeNotify $SHCNE_ASSOCCHANGED $SHCNF_FLUSH 0 0
    }
}

proc ::InstallAPI::SetActiveSetupType { args } {
    global conf
    global info

    ::InstallAPI::ParseArgs _args $args {
        -setuptype  { string  1 }
    }

    if {[::InstallJammer::ObjExists $_args(-setuptype)]} {
        set obj  $_args(-setuptype)
        set name [$obj name]

        if {![$obj is setuptype]} {
            return -code error "$obj is not a valid Setup Type object"
        }
    } else {
        set name $_args(-setuptype)
        set obj  [::InstallAPI::FindObjects -type setuptype -name $name]
        if {$obj eq ""} {
            return -code error "Could not find Setup Type '$name'"
        }
    }

    debug "Setting active setup type to $name ([$obj id])."

    set info(InstallType)   $name
    set info(InstallTypeID) [$obj id]
    unset -nocomplain conf(PopulatedSetupType)

    ::InstallJammer::SelectSetupType $info(InstallTypeID)

    foreach var [array names ::InstallJammer::Components] {
        set ::InstallJammer::Components($var) ""
    }

    set components [$info(InstallTypeID) get Components]
    foreach c [Components children recursive] {
        if {[lsearch -exact $components $c] < 0} {
            ::InstallAPI::ComponentAPI -components $c -active 0 -updateinfo 0
        } else {
            ::InstallAPI::ComponentAPI -components $c -active 1 -updateinfo 0
        }
    }

    ::InstallJammer::UpdateInstallInfo

    return $info(InstallTypeID)
}

proc ::InstallAPI::SetExitCode { args } {
    global conf

    ::InstallAPI::ParseArgs _args $args {
        -exitcode  { string 1 }
    }

    set conf(ExitCode) $_args(-exitcode)
}

proc ::InstallAPI::SetFileTypeEOL { args } {
    global conf

    ::InstallAPI::ParseArgs _args $args {
        -eol       { string 1 }
        -extension { string 1 }
    }

    set eol [string map {unix lf windows crlf} $_args(-eol)]
    foreach ext [split $_args(-extension) \;] {
        set conf(eol,$ext) $eol
    }
}

proc ::InstallAPI::SetObjectProperty { args } {
    ::InstallAPI::ParseArgs _args $args {
        -object   { string 1 }
        -property { string 1 }
        -value    { string 1 }
    }

    set id [::InstallJammer::ID $_args(-object)]

    if {![::InstallJammer::ObjExists $id]} {
        return -code error "Error in ::InstallAPI::SetObjectProperty:\
                object \"$id\" does not exist"
    }

    $id set $_args(-property) $_args(-value)
}

proc ::InstallAPI::SetUpdateWidgets { args } {
    global conf

    if {![::InstallJammer::InGuiMode]} { return }

    ::InstallAPI::ParseArgs _args $args {
        -save     { boolean 0 0 }
        -restore  { boolean 0 0 }
        -widgets  { string  0 }
    }

    if {$_args(-save)} {
        set conf(SavedUpdateWidgets) $conf(UpdateWidgets)
    }

    if {$_args(-restore) && [info exists conf(SavedUpdateWidgets)]} {
        set conf(UpdateWidgets) $conf(SavedUpdateWidgets)
        unset conf(SavedUpdateWidgets)
    }

    if {[info exists _args(-widgets)]} {
        set conf(UpdateWidgets) {}
        foreach widget $_args(-widgets) {
            lappend conf(UpdateWidgets) [join $widget ""]
        }
    }
}

proc ::InstallAPI::SetVirtualText { args } {
    variable ::InstallJammer::UpdateVarCmds
    variable ::InstallJammer::AutoUpdateVars

    ::InstallAPI::ParseArgs _args $args {
        -action      { string  0 }
        -autoupdate  { boolean 0 }
        -command     { string  0 }
        -language    { string  0 None }
        -object      { string  0 }
        -value       { string  0 }
        -virtualtext { string  1 }
    }

    set val  ""
    set var  $_args(-virtualtext)
    set lang $_args(-language)

    if {$lang eq "None"} {
        upvar #0 ::InstallJammer::UpdateVarCmds  cmds
        upvar #0 ::InstallJammer::AutoUpdateVars vars
    } else {
        if {[string equal -nocase $lang "all"]} {
            set lang  "all"
            set langs [::InstallJammer::GetLanguageCodes]
        } else {
            set langs [::InstallJammer::GetLanguageCode $lang]
        }
        if {![llength $langs]} {
            return -code error "invalid language '$lang'"
        }
        upvar #0 ::InstallJammer::UpdateTextCmds cmds
        upvar #0 ::InstallJammer::AutoUpdateText vars
    }

    if {[info exists _args(-object)]} {
        set object [::InstallJammer::ID $_args(-object)]
        set var $object,$var
    }
    
    if {[info exists _args(-value)]} {
        set val $_args(-value)

        if {$lang eq "None"} {
            set ::info($var) $val
        } elseif {$lang eq "all" && [info exists object]} {
            $object set $_args(-virtualtext) $val
        } else {
            foreach lang $langs {
                ::msgcat::mcset $lang $var $val
            }
            if {[info exists cmds($var)]} {
                uplevel #0 $cmds($var)
            }
        }
    }

    if {[info exists _args(-action)] && $_args(-action) ne ""} {
        set command [list ::InstallJammer::ExecuteActions $_args(-action)]
    }

    if {[info exists _args(-command)] && $_args(-command) ne ""} {
        set command $_args(-command)
    }

    if {[info exists command]} {
        lappend cmds($var) $command

        if {$lang eq "None"} {
            if {[info exists ::info($var)]} {
                uplevel #0 $command
            }
        } else {
            foreach lang $langs {
                if {[::msgcat::mcexists $var $lang]} {
                    uplevel #0 $command
                    break
                }
            }
        }
    }

    if {[info exists _args(-autoupdate)]} {
        set auto $_args(-autoupdate)
        if {$auto && ![info exists vars($var)]} {
            set vars($var) $val
            ::InstallJammer::UpdateWidgets -updateidletasks 1
        } elseif {!$auto && [info exists vars($var)]} {
            unset vars($var)
            ::InstallJammer::UpdateWidgets -updateidletasks 1
        }
    }
}

proc ::InstallAPI::SubstVirtualText { args } {
    ::InstallAPI::ParseArgs _args $args {
        -virtualtext { string 1 }
    }

    return [::InstallJammer::SubstText $_args(-virtualtext)]
}

proc ::InstallAPI::UnpackArchive { args } {
    global info

    ::InstallAPI::ParseArgs _args $args {
        -file                 { string  1 }
        -format               { string  0 }
        -destination          { string  1 }
        -deletearchive        { boolean 0 0 }
        -statusvirtualtext    { string  0 "PercentComplete" }
        -progressvirtualtext  { string  0 "CurrentFile" }
    }

    set file $_args(-file)
    set dest $_args(-destination)

    if {[info exists _args(-format)]} {
        set format $_args(-format)
    } else {
        set _args(-format) [string range [file extension $file] 1 end]
    }
    set format [string tolower $format]

    if {$format eq "zip"} {
        set mnt [::InstallJammer::TmpMount]
        set map [list $mnt/ ""]

        zvfs::mount $file $mnt

        set status      $_args(-statusvirtualtext)
        set progress    $_args(-progressvirtualtext)
        set doStatus    [string length $status]
        set doProgress  [string length $progress]
        set virtualtext {}

        if {$doStatus} {
            lappend virtualtext $status
            set info($status) "<%UnpackingFilesText%>"
        }

        if {$doProgress} {
            lappend virtualtext $progress
            set info($progress) 0
        }

        ::InstallJammer::UpdateWidgets -updateidletasks 1

        set files [recursive_glob $mnt *]

        if {$doStatus} {
            set info($status) "<%UnpackingFileText%>"
        }

        ::InstallAPI::SetUpdateWidgets -save 1 -widgets \
            [::InstallJammer::FindUpdateWidgets $virtualtext]

        set count   0
        set total   [llength $files]
        set lastpct 0
        foreach file $files {
            set update 0

            set new [file join $dest [string map $map $file]]
            set dir [file dirname $new]

            if {![file exists $dir]} { file mkdir $dir }

            if {$doStatus} {
                set update 1
                set info(CurrentFile) $new
            }

            if {$doStatus} {
                set pct [expr {([incr count] * 100) / $total}]
                if {$pct != $lastpct} {
                    set update 1
                    set lastpct $pct
                    set info($progress) $pct
                }
            }

            if {$update} {
                ::InstallJammer::UpdateSelectedWidgets
                update
            }

            file copy -force $file $new
        }

        zvfs::unmount $mnt

        if {$_args(-deletearchive)} {
            catch { file delete $_args(-file) }
        }
        
        ::InstallAPI::SetUpdateWidgets -restore 1
    }
}

proc ::InstallAPI::URLIsValid { args } {
    ::InstallAPI::ParseArgs _args $args {
        -proxyhost           { string 0 "" }
        -proxyport           { string 0 "" }
        -returnerror         { boolean 0 0 }
        -url                 { string 1 }
    }

    package require http

    set return 0

    http::config -proxyhost $_args(-proxyhost)
    http::config -proxyport $_args(-proxyport)

    if {[catch { http::geturl $_args(-url) -validate 1 } tok]} {
        if {$_args(-returnerror)} {
            return -code error $tok
        }

        set error $tok
        return 0
    }

    upvar #0 $tok state

    set code [http::ncode $tok]

    if {$code == 200} {
        set return 1
    } elseif {$code == 302} {
        ## This URL is a redirect.  We need to fetch the redirect URL.
        array set meta $state(meta)

        if {![info exists meta(Location)]} {
            return -code error "302 Redirect without new Location"
        }

        set _args(-url) $meta(Location)
        debug "URL Redirected to $_args(-url)."

        set return [eval ::InstallAPI::URLIsValid [array get _args]]
    }

    http::cleanup $tok

    return $return
}

proc ::InstallAPI::ValidateApplication { args } {
    ::InstallAPI::ParseArgs _args $args {
        -path           { string  1 }
        -separator      { string  0 "" }
        -expression     { string  0 }
    }

    set _path $_args(-path)
    if {![file exists $_path]} { return 0 }
    if {![info exists _args(-expression)]} { return 1 }

    set result $_args(-path)
    if {$_args(-separator) eq ""} {
        set lines $_args(-expression)
    } else {
        set lines [split $_args(-expression) $_args(-separator)]
    }

    foreach exp $lines {
        set exp [string trim $exp]
        if {$exp eq ""} { continue }
        if {[string index $exp 0] eq "#"} { continue }

        switch -- [string tolower [lindex $exp 0]] {
            "collect" {
                lassign $exp x var "in" what varName

                if {$what eq "virtualtext"} { set varName ::info($varName) }
                set collect($var) ""
                set collectVars($var) $varName
                if {$var eq "_path"} { lappend collect($var) $_path }
            }

            "execute" {
                set file [subst [lindex $exp 1]]

                set file [file join $_path $file]
                if {![file exists $file]} { return 0 }
                set args [lrange $exp 2 end]
                set _exitcode 0
                if {[catch { eval exec [list $file] $args } _result]} {
                    set _exitcode [lindex $::errorCode 2]
                }
            }

            "exists" {
                set file [subst [lindex $exp 1]]

                set file [file join $_path $file]
                if {![file exists $file]} { return 0 }
            }

            "read" {
                set file [subst [lindex $exp 1]]
                set var  [lindex $exp 3]

                set file [file join $_path $file]
                if {![file exists $file]} { return 0 }
                set $var [read_file $file]
                if {[info exists collect($var)]} {
                    lappend collect($var) [set $var]
                }
            }

            "regexp" {
                set pattern [lindex $exp 1]
                set vars    [lrange $exp 2 end]

                if {![eval [list regexp $pattern $_result] $vars]} { return 0 }
                foreach var $vars {
                    if {[info exists collect($var)]} {
                        lappend collect($var) [set $var]
                    }
                }
            }

            "test" {
                set val1 [subst [lindex $exp 1]]
                set op   [lindex $exp 2]
                set val2 [subst [lindex $exp 3]]

                switch -- $op {
                    "eq" { if {![string equal $val1 $val2]} { return 0 } }
                    "ne" { if {[string equal $val1 $val2]} { return 0 } }
                    "in" {
                        set val2 [lrange $exp 3 end]
                        if {[lsearch -exact $val2 $val1] < 0} { return 0 }
                    }
                    "ni" {
                        set val2 [lrange $exp 3 end]
                        if {[lsearch -exact $val2 $val1] > -1} { return 0 }
                    }

                    default {
                        if {![expr $val1 $op $val2]} { return 0 }
                    }
                }
            }

            "version" {
                set ver1 [subst [lindex $exp 1]]
                set op   [lindex $exp 2]
                set ver2 [subst [lindex $exp 3]]

                set res [vercmp $ver1 $ver2]
                if {![expr $res $op 0]} { return 0 }
            }

            default {
                return -code error "invalid expression $exp"
            }
        }
    }

    set level 1
    if {![catch {info level -1} info]
        && [string match "::InstallAPI::*" [lindex $info 0]]} { set level 2 }
    foreach {var value} [array get collect] {
        uplevel $level lappend $collectVars($var) $value
    }

    return 1
}

proc ::InstallAPI::VirtualTextAPI { args } {
    ::InstallAPI::ParseArgs _args $args {
        -do          { choice 1 "" "settype"}
        -type        { choice 0 "" "boolean directory"}
        -virtualtext { string 1 }
    }

    if {$_args(-do) eq "settype"} {
        if {![info exists _args(-type)]} {
            return -code error "must specify -type"
        }

        foreach var $_args(-virtualtext) {
            set ::InstallJammer::VTTypes($var) $_args(-type)
        }
    }
}

proc ::InstallAPI::VirtualTextExists { args } {
    ::InstallAPI::ParseArgs _args $args {
        -language    { string 0 None }
        -virtualtext { string 1 }
    }

    return [::msgcat::mcexists $_args(-virtualtext) $_args(-language)]
}
