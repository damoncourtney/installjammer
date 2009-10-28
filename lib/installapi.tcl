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

proc ::InstallAPI::ExecuteAction { args } {
    ::InstallAPI::ParseArgs _args $args {
        -action          { string  1 }
        -parent          { string  0 "" }
        -checkconditions { boolean 0 1 }
    }

    ::InstallJammer::ExecuteActions $_args(-action) \
        -parent $_args(-parent) -conditions $_args(-checkconditions)
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

proc ::InstallAPI::FindObjects { args } {
    ::InstallAPI::ParseArgs _args $args {
        -active    { boolean 0 }
        -alias     { string  0 }
        -component { string  0 }
        -glob      { boolean 0 }
        -name      { string  0 }
        -parent    { string  0 }
        -type      {   string  0 }
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
        set objects [::itcl::find objects]
    } else {
        set type [string tolower $type]
        incr check -1
        set chktype 0
        switch -- $type {
            "file" { set class ::File }
            "filegroup" { set class ::FileGroup }
            "component" { set class ::Component }
            "setuptype" { set class ::SetupType }
            "condition" { set class ::Condition }
            "action" - "actiongroup" - "pane" {
                incr check
                set chktype 1
                set class ::InstallComponent
            }

            default {
                return -code error [BWidget::badOptionString type $_args(-type)\
                    {action actiongroup pane file filegroup component
                        setuptype condition}]
            }
        }
        set objects [::itcl::find objects -class $class]
        if {$type eq "file"} {
            set objects [::itcl::find objects -class $class]
            if {$chkcomponent} {
                ## Files don't have a component.
                incr check -1
                set chkcomponent 0
            }
        } else {
            ## All other types besides files have a root
            ## parent object that we don't want to show up.
            set objects {}
            foreach obj [::itcl::find objects -class $class] {
                if {[$obj parent] eq ""} { continue }
                lappend objects $obj
            }
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

    ::InstallAPI::ParseArgs _args $args {
        -widget { string 1 }
        -window { string 0 }
    }

    if {[info exists _args(-window)]
        && [::InstallJammer::ObjExists $_args(-window)]
        && [$_args(-window) ispane]} {
        set window $_args(-window)
    } else {
        set window $info(CurrentPane)
    }

    set name [join [::InstallJammer::ID [string trim $_args(-widget)]] ""]

    if {[lsearch -exact $conf(ButtonWidgets) $name] > -1} {
        set name [string tolower [string map {Button ""} $name]]
        set widg [$info(Wizard) widget get $name]
    } else {
        set widg  [$window widget get $name]
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

    if {![catch { registry keys $_args(-key) } keys]} {
        return $keys
    }
}

proc ::InstallAPI::EnvironmentVariableExists { args } {
    ::InstallAPI::ParseArgs _args $args {
        -variable { string 1 }
    }

    return [info exists ::env($_args(-variable))]
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

proc ::InstallAPI::ModifyWidget { args } {
    global conf
    global info
    global hidden

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
    debug "API Call: [info level -1]"

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

proc ::InstallAPI::RollbackInstall { args } {
    ::InstallJammer::CleanupCancelledInstall
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

    set info(InstallType)   $name
    set info(InstallTypeID) $obj
    unset -nocomplain conf(PopulatedSetupType)

    array unset ::InstallJammer::Components $info(InstallTypeID),*
    set components [$info(InstallTypeID) get Components]
    foreach component [Components children recursive] {
        if {[lsearch -exact $components $component] < 0} {
            $component active 0
        } else {
            $component active 1
        }
    }

    ::InstallJammer::UpdateInstallInfo

    return $obj
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

    ::InstallAPI::ParseArgs _args $args {
        -widgets  { string 1 }
    }

    set conf(UpdateWidgets) {}
    foreach widget $_args(-widgets) {
        lappend conf(UpdateWidgets) [join $widget ""]
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

proc ::InstallAPI::VirtualTextExists { args } {
    ::InstallAPI::ParseArgs _args $args {
        -language    { string 0 None }
        -virtualtext { string 1 }
    }

    return [::msgcat::mcexists $_args(-virtualtext) $_args(-language)]
}
