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
namespace eval ::InstallJammer {}
namespace eval ::InstallJammer::subst {}

proc lempty { list } {
    if {[catch {expr [llength $list] == 0} ret]} { return 0 }
    return $ret
}

proc lremove { list args } {
    foreach arg $args {
	set x [lsearch -exact $list $arg]
	set list [lreplace $list $x $x]
    }
    return $list
}

proc lassign_array {list arrayName args} {
    upvar 1 $arrayName array
    foreach elem $list var $args {
	set array($var) $elem
    }
}

proc incr0 { varName {n 1} } {
    upvar 1 $varName var
    if {![info exists var]} { set var 0 }
    set var [expr {wide($var) + $n}]
}

proc iincr { varName {n 1} } {
    upvar 1 $varName var
    if {![info exists var]} { set var 0 }
    set var [expr {$var + $n}]
}

proc patheq {path1 path2} {
    global conf

    set path1 [::InstallJammer::Normalize $path1]
    set path2 [::InstallJammer::Normalize $path2]
    if {$conf(windows)} { return [string equal -nocase $path1 $path2] }
    return [string equal $path1 $path2]
}

proc recursive_glob {dir pattern} {
    set files [glob -nocomplain -type f -dir $dir $pattern]
    foreach dir [glob -nocomplain -type d -dir $dir *] {
        eval lappend files [recursive_glob $dir $pattern]
    }
    return $files
}

proc noop {args} {}

proc read_file { file args } {
    set fp [open $file]
    eval [list fconfigure $fp] $args
    set x [read $fp]
    close $fp
    return $x
}

proc sha1hex {string} {
    set sha1 [sha1 $string]
    binary scan $sha1 H* hex
    return $hex
}

proc stack {} {
    set list {}
    for {set i [expr {[info level] - 1}]} {$i > 0} {incr i -1} {
        lappend list "$i: [info level $i]"
    }
    return [join $list \n]
}

proc debugging { {value ""} {level ""} {file ""} } {
    global info

    if {$value eq "ison"} {
        if {[info exists info(DebugLogLevel)]
            && ($info(DebugLogToFile) || $info(DebugLogToConsole))} {
            return $info(DebugLogLevel)
        }
        return 0
    }

    if {$value eq "level"} {
        if {$level eq ""} { return $info(DebugLogLevel) }

        if {$level < 0 || $level > 3} {
            return -code error "invalid debug level \"$level\":\
                should be 0, 1, 2 or 3"
        }

        set info(DebugLogLevel) $level
        return $info(DebugLogLevel)
    }

    if {[string is true -strict $value]} {
        if {$level eq "" || $level eq "console"} {
            set info(DebugLogToConsole) 1
        } elseif {$level eq "file"} {
            set info(DebugLogToFile) 1
            if {$file eq ""} {
                set file $info(DebugLogFile)
            } else {
                set info(DebugLogFile) $file
            }
            if {[info exists ::debugfp]} {
                catch { close $::debugfp }
                set ::debugfp ""
            }
            set ::debugfp [open [sub $file] w]
        } else {
            return -code error "bad debugging option \"$level\":\
                should be console or file"
        }
    } elseif {[string is false -strict $value]} {
        if {$level eq ""} {
            set info(DebugLogToFile) 0
            set info(DebugLogToConsole) 0
            if {[info exists ::debugfp]} {
                catch { close $::debugfp }
                set ::debugfp ""
            }
        } elseif {$level eq "console"} {
            set info(DebugLogToConsole) 0
        } elseif {$level eq "file"} {
            set info(DebugLogToFile) 0
            if {[info exists ::debugfp]} {
                catch { close $::debugfp }
                set ::debugfp ""
            }
        }
    } elseif {$value eq "level"} {
        if {$level eq ""} { return $info(DebugLogLevel) }

        if {$level < 0 || $level > 3} {
            return -code error "invalid debug level \"$level\":\
                should be 0, 1, 2 or 3"
        }

        set info(DebugLogLevel) $level
    } elseif {$value ne ""} {
        return -code error "usage: debugging ?on|off? ?file|console? ?logfile?"
    }

    if {$info(DebugLogToConsole)} {
        if {[debugging ison]} {
            echo "Debugging is turned on"
        } else {
            echo "Debugging is turned off"
        }
        echo "Debug level is set to $info(DebugLogLevel)"
        if {$info(DebugLogToConsole)} {
            echo "Debug output is being written to the console"
        }
        if {$info(DebugLogToFile)} {
            echo "Debug output is being saved to a log file"
            echo "Debug log file is <%DebugLog%>" 1
        }
    }
}

proc debug { message args } {
    global info

    ## We don't output debugging in the builder.
    if {[info exists ::InstallJammer]} { return }

    ## If we're not logging to a file or the console, we've got nothing to do.
    if {!$info(DebugLogToFile) && !$info(DebugLogToConsole)} { return }

    set logToFile 1
    if {[set x [lsearch -exact $args "-nofile"]] > -1} {
        set logToFile 0
        set args [lreplace $args $x $x]
    }
    set id [lindex $args 0]

    set time [clock format [clock seconds] -format "%m/%d/%Y %H:%M:%S%p"]
    set string "$time - $message"

    if {[set x [::InstallJammer::SubstText $message]] ne $message} {
        append string "\n$time - ** $x"
    }

    if {$id ne "" && [$id get Comment comment] && $comment ne ""} {
        append string "\n$time - # $comment"
        if {[set x [::InstallJammer::SubstText $comment]] ne $comment} {
            append string "\n$time - ** $x"
        }
    }

    if {![info exists ::InstallJammer]} {
        if {$info(DebugLogToConsole)} {
            puts  stderr $string
            flush stderr
            update
        }

        if {$logToFile && $info(DebugLogToFile)} {
            if {![info exists ::debugfp]} {
                set ::debugfp [open [::InstallJammer::TmpDir debug.log] w]
            } elseif {$::debugfp eq ""} {
                set ::debugfp [open [::InstallJammer::TmpDir debug.log] a]
            }

            puts  $::debugfp $string
            flush $::debugfp
        }
    }
}

proc threaded {} {
    global conf

    if {![info exists conf(threaded)]} {
        set conf(threaded) [info exists ::tcl_platform(threaded)]
        if {$conf(threaded)} {
            package require Thread
            if {[catch {thread::send [thread::id] #}]} { set conf(threaded) 0 }
        }
    }
    return $conf(threaded)
}

proc ::echo { string {subst 0} } {
    if {$subst} { set string [::InstallJammer::SubstText $string] }
    puts  stdout $string
    flush stdout
}

proc ::more { args } {
    global conf

    if {[expr {[llength $args] % 2}]} {
        set text [lindex $args end]
        set args [lrange $args 0 end-1]
    }

    array set _args {
        -file      ""
        -width     0
        -height    0
        -allowquit 1
    }
    array set _args $args

    if {$_args(-file) ne ""} {
        set text [read_file $_args(-file)]
    }

    set height $_args(-height)
    if {$height == 0} {
        set height 24
        if {[info exists conf(ConsoleHeight)]} {
            set height $conf(ConsoleHeight)
        } else {
            if {![catch { exec stty size } result]} {
                set height [lindex $result 0]
            }
        }
    }

    incr height -1

    if {$_args(-width) > 0} {
        set text [::InstallJammer::WrapText $text $_args(-width)]
    }

    catch { exec stty raw -echo <@stdin }

    if {!$_args(-allowquit)} {
        set prompt [::InstallJammer::SubstText "<%ConsolePauseText%>"]
    } else {
        set prompt [::InstallJammer::SubstText "<%ConsolePauseQuitText%>"]
    }

    catch { 
        set i 0
        foreach line [split $text \n] {
            puts stdout $line

            if {[incr i] >= $height} {
                puts -nonewline stdout $prompt
                flush stdout

                while {1} {
                    set x [read stdin 1]
                    if {$_args(-allowquit) && $x eq "q" || $x eq "Q"} {
                        return
                    } elseif {$x eq " "} {
                        break
                    }
                }

                puts  stdout ""
                flush stdout

                set i 0
            }
        }
    }

    catch { exec stty -raw echo <@stdin }

    return
}

proc ::tk_safeDialog {command opts safeOpts} {
    set args {}
    array set _args $opts
    foreach opt $safeOpts {
        if {[info exists _args($opt)]} {
            if {$opt eq "-parent" && ![winfo exists $_args($opt)]} { continue }
            if {$opt eq "-initialdir" && ![file exists $_args($opt)]} {continue}
            lappend args $opt $_args($opt)
        }
    }
    return [eval $command $args]
}

proc ::ij_chooseDirectory {args} {
    ::tk_safeDialog ::tk_chooseDirectory $args \
        {-initialdir -mustexist -parent -title}
}

proc ::ij_getOpenFile {args} {
    ::tk_safeDialog ::tk_getOpenFile $args \
        {-defaultextension -filetypes -initialdir -initialfile \
              -message -multiple -parent -title -typevariable}
}

proc ::ij_getSaveFile {args} {
    ::tk_safeDialog ::tk_getSaveFile $args \
        {-defaultextension -filetypes -initialdir -initialfile \
              -message -multiple -parent -title -typevariable}
}

if {[info exists ::conf(unix)] && $::conf(unix)} {
    ## Repace the core Tk get file dialogs with our own that looks nicer.
    proc ::tk_getOpenFile { args } {
        return [eval ChooseFile .__tk_getOpenFile $args -type open]
    }

    proc ::tk_getSaveFile { args } {
        return [eval ChooseFile .__tk_getSaveFile $args -type save]
    }

    proc ::tk_messageBox { args } {
        return [eval ::InstallJammer::MessageBox $args]
    }
}

proc SafeSet { varName value } {
    upvar 1 $varName var
    if {![info exists var]} { set var $value }
    return $value
}

proc SafeArraySet { arrayName list } {
    upvar 1 $arrayName array
    foreach {varName elem} $list {
	if {![info exists array($varName)]} { set array($varName) $elem }
    }
}

package require msgcat

## We're going to redefine some of Tcl's msgcat commands in the
## name of simplifying things.
proc ::msgcat::mc { src args } {
    foreach loc [::msgcat::mcpreferences] {
        if {[info exists ::msgcat::Msgs_${loc}($src)]} { break }
    }
    return [eval [list ::msgcat::mcget $loc $src] $args]
}

proc ::msgcat::mcexists { src {locales {}} } {
    if {![llength $locales]} {
        set locales [::msgcat::mcpreferences]
    }
    foreach locale $locales {
        if {$locale eq "None"} {
            upvar #0 ::info msgs
        } else {
            upvar #0 ::msgcat::Msgs_${locale} msgs
        }

        if {[info exists msgs($src)]} { return 1 }
    }
    return 0
}

proc msgcat::mclocale { args } {
    variable Locale
    variable Loclist

    if {[llength $args] == 1 && [lindex $args 0] eq ""} {
        set Loclist {}
        return
    }

    if {[llength $args]} {
        foreach locale $args {
            set loc  {}
            set word ""
            foreach part [split [string tolower $locale] _] {
                set word [string trimleft "${word}_${part}" _]
                if {[set x [lsearch -exact $Loclist $word]] > -1} {
                    set Loclist [lreplace $Loclist $x $x]
                }
                set Loclist [linsert $Loclist 0 $word]
            }
        }
        set Locale $locale
    }
    return $Locale
}

proc ::msgcat::mcset { locale src {dest ""} } {
    if {$locale eq "None"} {
        upvar #0 ::info msgs
    } else {
        upvar #0 ::msgcat::Msgs_${locale} msgs
    }

    if {[llength [info level 0]] == 3} { set dest $src }
    set msgs($src) $dest
}

proc ::msgcat::mcunset { locale src } {
    if {$locale eq "None"} {
        upvar #0 ::info msgs
    } else {
        upvar #0 ::msgcat::Msgs_${locale} msgs
    }

    array unset msgs $src
}

proc ::msgcat::mcmset { locale pairs } {
    if {$locale eq "None"} {
        upvar #0 ::info msgs
    } else {
        upvar #0 ::msgcat::Msgs_${locale} msgs
    }

    array set msgs $pairs
}

proc ::msgcat::mcgetall { locale } {
    if {$locale eq "None"} {
        upvar #0 ::info msgs
    } else {
        upvar #0 ::msgcat::Msgs_${locale} msgs
    }

    return [array get msgs]
}

proc ::msgcat::mcget { locale src args } {
    if {$locale eq "None"} {
        upvar #0 ::info msgs
    } else {
        upvar #0 ::msgcat::Msgs_${locale} msgs
    }

    if {![info exists ::msgcat::renderer($locale)]} {
        set ::msgcat::renderer($locale) \
            [expr {[info commands ::${locale}::render] ne ""}]
    }

    if {[info exists msgs($src)]} {
        set src $msgs($src)

        if {$::msgcat::renderer($locale)} {
            set src [::${locale}::render $src]
        }
    }

    if {[llength $args]} {
        return [uplevel 1 [linsert $args 0 ::format $src]]
    } else {
        return $src
    }
}

proc ::msgcat::mcclear { locale } {
    unset -nocomplain ::msgcat::Msgs_${locale}
}

namespace eval ::InstallJammer {}
namespace eval ::InstallJammer::actions {}
namespace eval ::InstallJammer::conditions {}

proc ::InstallJammer::CommonInit {} {
    global info
    global conf

    if {[info exists conf(commonInit)]} { return }

    set conf(osx)       [string equal $::tcl_platform(os) "Darwin"]
    set conf(unix)      [string equal $::tcl_platform(platform) "unix"]
    set conf(windows)   [string equal $::tcl_platform(platform) "windows"]
    set conf(windows98) [expr {$conf(windows)
                           && $::tcl_platform(osVersion) < 5.0}]
    set conf(vista)     [expr {$conf(windows)
                           && $::tcl_platform(osVersion) >= 6.0}]
    set conf(wine)      [expr {$conf(windows) && [info exists ::env(_)]
                         && [file tail $::env(_)] eq "wine"}]

    set conf(cmdlineMessages) 1
    if {$conf(windows)} { set conf(cmdlineMessages) 0 }

    array set conf {
	commonInit   1

        logInit      0

        ExitCode     ""

        ObjectStack   {}
        ParentWindow  {}
        UpdateWidgets {}
        ButtonWidgets {BackButton NextButton CancelButton}

        UserRCFiles {~/.bashrc ~/.cshrc ~/.tcshrc ~/.zshrc ~/.kshrc ~/.profile}
        SystemRCFiles {/etc/bashrc /etc/csh.cshrc /etc/zshrc /etc/profile}

        ShowConsoleProgress 1

        UpdateWindowsRegistry 0

        ModifySelectedComponents 1

        ComponentTrees  {}
        SetupTypeTrees  {}

        SaveResponseVars {
            "CreateDesktopShortcut boolean"
            "CreateQuickLaunchShortcut boolean"
            "InstallDir string"
            "InstallMode string"
            "InstallType string"
            "LaunchApplication boolean"
            "ProgramFolderName string"
            "SelectedComponents list"
            "ViewReadme boolean"
        }

        VirtualTextMap            {}
	VirtualTextRecursionLimit 10
    }

    lappend conf(VirtualTextMap) "\\" "\\\\" "\[" "\\\["
    lappend conf(VirtualTextMap) "<%" {[::InstallJammer::SubstVar [list }
    lappend conf(VirtualTextMap) "%>" {]]}

    if {[info exists ::installkit::root]} {
        set conf(vfs) $::installkit::root
    }
    set conf(exe)      [info nameofexecutable]
    set conf(script)   [info script]

    if {$conf(windows)} {
        set info(Ext)       ".exe"
        set info(ScriptExt) ".bat"
    } else {
        set info(Ext)       ""
        set info(ScriptExt) ".sh"
    }

    array set ::InstallJammer::PropertyMap {
        Include {
            "Always include"
            "Include only when testing"
            "Include only when not testing"
        }

        ExecuteAction {
            "After Pane is Cancelled"
            "After Pane is Displayed"
            "After Pane is Finished"
            "Before Next Pane is Displayed"
            "Before Pane is Cancelled"
            "Before Pane is Displayed"
            "Before Pane is Finished"
            "Before Previous Pane is Displayed"
        }

        FileUpdateMethod {
            "Update files with more recent dates"
            "Update files with a newer version"
            "Always overwrite files"
            "Never overwrite files"
        }

        CheckCondition {
            "After Pane is Cancelled"
            "Before Next Pane is Displayed"
            "Before Pane is Cancelled"
            "Before Pane is Displayed"
            "Before Pane is Finished"
            "Before Previous Pane is Displayed"
            "Before Action is Executed"
            "Before Next Action is Executed"
        }
    }

    ## Append some default directories to the PATH so that
    ## we can find tools we're looking for even if the user
    ## running the installer doesn't include these directoris.
    if {!$conf(windows)} {
        append ::env(PATH) ":/bin:/sbin:/usr/bin:/usr/sbin"
        append ::env(PATH) ":/usr/local/bin:/usr/local/sbin"
    }

    ::InstallJammer::InitializeMessageCatalogs

    set conf(NativeChooseFile)       [expr {!$conf(unix)}]
    set conf(NativeMessageBox)       [expr {!$conf(unix)}]
    set conf(NativeChooseDirectory)  [expr {!$conf(unix)}]

    if {![info exists ::InstallJammer]} {
        ## Running from an installer or uninstaller

        ## Setup common variables for an install or uninstall.
        set ::debug "off"

        array set conf {
            ExitScripts       {}
            ConsoleWidth      80
            ConsoleHeight     24
            NativeMessageBox  0
        }
        set conf(NativeChooseDirectory) $conf(osx)

        SafeArraySet conf {
            twapi               0
            Wow64Disabled       0
        }

        array set info {
            Wizard             .wizard
            WizardFirstStep    0
            WizardLastStep     0
            WizardStarted      0
            WizardFinished     0
            WizardCancelled    0

	    Errors             ""

            SilentMode         0
            DefaultMode        0

            UserMovedBack      0
            UserMovedNext      0

            InAppBundle        0
        }

	SafeArraySet info {
            AllowLanguageSelection 1
            PromptForRoot          1

            Testing                 0
            Debugging               0
            DebugLogFile            "<%Temp%>/debug.log"
            DebugLogLevel           1
            DebugLogToFile          0
            DebugLogToConsole       0
            ShowConsole             0
            SaveTempDirectory       0

            Language                "en"
            DefaultToSystemLanguage "Yes"

            InstallMode            "Standard"
            UninstallMode          "Standard"

            FallBackToConsole      0

            InstallVersionInfo     1
            InstallRegistryInfo    1

            RunningInstaller       0
            RunningUninstaller     0

            SpaceRequiredText      "<%DiskSpace <%SpaceRequired%>%>"
            SpaceAvailableText     "<%DiskSpace <%SpaceAvailable%>%>"
	}

        set info(Home) [::InstallJammer::HomeDir]

        if {$info(RunningInstaller)} {
            if {[info exists info(DefaultLanguage)]} {
                set info(Language) [GetLanguageCode $info(DefaultLanguage)]
            }

            msgcat::Init
            set info(SystemLanguage) [::msgcat::mclocale]
            set codes [::InstallJammer::GetLanguageCodes]
            foreach lang [::msgcat::mcpreferences] {
                if {[lsearch -exact $codes $lang] > -1} {
                    set info(SystemLanguage) $lang
                    if {$info(DefaultToSystemLanguage)} {
                        set info(Language) $lang
                    }
                    break
                }
            }

            set info(InstallStartupDir) [pwd]
        } elseif {$info(RunningUninstaller)} {
            set info(UninstallStartupDir) [pwd]
        }

        ::InstallJammer::SetupPlatformVirtualText

        if {$conf(windows)} {
            set info(Username)      $::tcl_platform(user)
            set info(PathSeparator) \;

            set info(Desktop) <%DESKTOP%>

            ::InstallJammer::SetupRegVirtualText
        } else {
            set info(Username)        [id user]
            set info(PathSeparator)   :
            set info(HasKDEDesktop)   [::InstallJammer::HasKDEDesktop]
            set info(HasGnomeDesktop) [::InstallJammer::HasGnomeDesktop]

            switch -- [::InstallJammer::GetDesktopEnvironment] {
                "KDE" {
                    set info(Desktop) <%KDEDesktop%>
                }

                "Gnome" {
                    set info(Desktop) <%GnomeDesktop%>
                }
            }

            set info(HaveTerminal) [expr {[catch { exec tty }] == 0}]

            if {!$info(HaveTerminal)} { set conf(cmdlineMessages) 0 }
        }

        if {[info exists info(Language)]} {
            ::InstallAPI::LanguageAPI -do setlanguage -language $info(Language)
        }

        set info(UserIsRoot)   [string equal $info(Username) "root"]
        set info(RealUsername) $::tcl_platform(user)

        ## Setup <%Status%> to automatically update the screen when modified.
        ::InstallAPI::SetVirtualText -virtualtext Status -value "" -autoupdate 1

        ## Call a command when <%InstallDir%> is modified.
        ::InstallAPI::SetVirtualText -virtualtext InstallDir \
            -command ::InstallJammer::ModifyInstallDir

        ## Change the main window title when <%InstallTitleText%> is changed.
        ::InstallAPI::SetVirtualText -virtualtext Language \
            -command ::InstallJammer::ModifyInstallTitle
        ::InstallAPI::SetVirtualText -virtualtext InstallTitleText \
            -command ::InstallJammer::ModifyInstallTitle
        ::InstallAPI::SetVirtualText -virtualtext InstallTitleText \
            -language all -command ::InstallJammer::ModifyInstallTitle

        ## Setup some other variables when <%Debugging%> is modidifed.
        ::InstallAPI::SetVirtualText -virtualtext Debugging -command {
            set ::info(DebugLogToFile) $::info(Debugging)
            set ::info(SaveTempDirectory) $::info(Debugging)
        }

        ::InstallAPI::VirtualTextAPI -do settype -type directory -virtualtext {
	    CommonStartMenu
            Desktop
            FileBeingInstalled
	    GnomeCommonStartMenu
	    GnomeDesktop
	    GnomeStartMenu
	    Home
	    InstallDir
	    Installer
	    InstallLogDirectory
	    InstallSource
	    KDECommonStartMenu
	    KDEDesktop
	    KDEStartMenu
            ProgramReadme
            ProgramLicense
	    ProgramExecutable
	    ProgramFolder
	    Uninstaller
	    UninstallDirectory
	}
    }

    SafeArraySet info {
        Date            "<%Date <%DateFormat%>%>"
        DateFormat      "%Y%m%d"
    }
}

proc ::InstallJammer::CommonPostInit {} {
    global conf
    global info

    set conf(stop)  [::InstallJammer::TmpDir .stop]
    set conf(pause) [::InstallJammer::TmpDir .pause]

    if {[info exists conf(vfs)]} {
        set bin    [file join $conf(vfs) lib bin]
        set tmpbin [::InstallJammer::TmpDir bin]
        if {[file exists $bin] && ![file exists $tmpbin]} {
            set ::env(PATH) "$tmpbin$info(PathSeparator)$::env(PATH)"
            file copy -force $bin $tmpbin
            if {!$conf(windows)} {
                foreach file [glob -dir $tmpbin *] {
                    file attributes $file -permissions 0755
                }
            }
        }
    }
}

proc ::InstallJammer::InitializeGui {} {
    global conf
    global info

    if {[info exists ::InstallJammer]} { return }
    if {[info exists conf(InitGui)]} { return }
    set conf(InitGui) 1
    SourceCachedFile gui.tcl
    InitGui

    if {$conf(osx)} {
        bind Text <Command-Tab> [bind Text <Tab>]

        proc ::tk::mac::Quit {} {
            global info
            $info(Wizard) cancel 1
        }
    } else {
        bind Text <Control-Tab> [bind Text <Tab>]
    }
    bind Text <Tab> "# nothing"
    bind Text <Shift-Tab> ""

    set wizard $info(Wizard)
    bind $wizard <<WizardStep>>      "::InstallJammer::RaiseEventHandler  %W"
    bind $wizard <<WizardCancel>>    "::InstallJammer::CancelEventHandler %W"
    bind $wizard <<WizardFinish>>    "::InstallJammer::FinishEventHandler %W"
    bind $wizard <<WizardLastStep>>  "set ::info(WizardLastStep)  1"
    bind $wizard <<WizardFirstStep>> "set ::info(WizardFirstStep) 1"
}

proc ::InstallJammer::InitializeMessageCatalogs {} {
    global conf
    global info

    variable languages
    variable languagecodes

    if {[info exists ::InstallJammer]} {
        set langfile [file join $conf(lib) msgs languages.txt]
        if {[file exists $langfile]} {
            array set ::InstallJammer::languagecodes [read_file $langfile]
        }
    }

    if {![array exists languagecodes]} {
        array set languagecodes {
            de      "German"
            en      "English"
            es      "Spanish"
            fr      "French"
            pl      "Polish"
            pt_br   "Brazilian Portuguese"
        }
    }

    if {[info exists info(Languages)]} {
        array set languagecodes $info(Languages)
    }

    foreach var [array names languagecodes] {
        set languages($languagecodes($var)) $var
    }
    set languages(None) None

    return [lsort [array names languagecodes]]
}

proc ::InstallJammer::GetLanguageCode { lang } {
    set lang [string tolower $lang]

    set codes [::InstallJammer::GetLanguageCodes]
    if {[set x [lsearch -exact [string tolower $codes] $lang]] > -1} {
        return [lindex $codes $x]
    }

    set langs [::InstallJammer::GetLanguages]
    if {[set x [lsearch -exact [string tolower $langs] $lang]] > -1} {
        return $::InstallJammer::languages([lindex $langs $x])
    }
}

proc ::InstallJammer::GetLanguageCodes {} {
    return [lsort [array names ::InstallJammer::languagecodes]]
}

proc ::InstallJammer::GetLanguage { code } {
    set code [string tolower $code]

    set langs [::InstallJammer::GetLanguages]
    if {[set x [lsearch -exact [string tolower $langs] $code]] > -1} {
        return [lindex $langs $x]
    }

    set codes [::InstallJammer::GetLanguageCodes]
    if {[set x [lsearch -exact [string tolower $codes] $code]] > -1} {
        return $::InstallJammer::languagecodes([lindex $codes $x])
    }
}

proc ::InstallJammer::GetLanguages { {includeNone 0} } {
    variable languages
    set list [lremove [lsort [array names languages]] None]
    if {$includeNone} { set list [linsert $list 0 None] }
    return $list
}

proc ::InstallJammer::ConfigureBidiFonts {} {
    if {$::info(Language) eq "ar"} {
        foreach font [font names] {
            font configure $font -family Arial -size 10
        }
    }
}

proc ::InstallJammer::LoadTwapi {} {
    global conf

    set conf(twapi) 0
    ## Check to see if the user included the TWAPI extension
    ## and that we're on Windows XP or higher.  If so, require
    ## the extension to load the commands.
    set conf(twapi) 0
    if {$conf(windows)
        && $::tcl_platform(osVersion) >= 5.0
        && [info exists ::installkit::root]
        && [llength [package versions twapi]]} {
        ## Set a variable to trick TWAPI.
        namespace eval ::starkit {
            variable topdir $::installkit::root
        }

        package require twapi
        set conf(twapi) 1
    }
}

proc ::InstallJammer::InitializeCommandLineOptions {} {
    global conf
    global info

    if {[info exists conf(initializeCommandLine)]} { return }
    set conf(initializeCommandLine) 1

    variable ::InstallJammer::CommandLineOptions

    ## Setup the default options that all InstallJammer installers have.
    ## These options should not be removed by the developer.

    set CommandLineOptions(help) {
        {} Switch 0 0 {}
        "display this information" 
    }

    set CommandLineOptions(temp) {
        TempRoot String 0 0 {}
        "set the temporary directory used by this program"
    }

    set CommandLineOptions(version) {
        {} Switch 0 0 {}
        "display installer version information"
    }

    if {$info(EnableResponseFiles)} {
        set CommandLineOptions(response-file) {
            ResponseFile String 0 0 {}
            "a file to read installer responses from"
        }

        set CommandLineOptions(save-response-file) {
            SaveResponseFile String 0 0 {}
            "a file to write installer responses to when the installer exits"
        }
    }

    ## Make all the options case-insensitive.

    foreach opt [array names CommandLineOptions] {
        set name [string tolower $opt]
        set CommandLineOptions($name) [concat $opt $CommandLineOptions($opt)]
        if {$opt ne $name} { unset CommandLineOptions($opt) }

        lassign $CommandLineOptions($name) x var type debug hide values desc
        if {$type eq "Prefix"} {
            lappend CommandLineOptions(PREFIX) $name
        }
    }
}

proc ::InstallJammer::HideMainWindow {} {
    global conf
    global info

    if {[info exists ::tk_patchLevel]} {
        wm withdraw .
        ::InstallJammer::ModifyInstallTitle
    }
}

proc ::InstallJammer::NewStyle { newStyle oldStyle args } {
    style layout $newStyle [style layout $oldStyle]
    eval [list style configure $newStyle] [style configure $oldStyle] $args
    return $newStyle
}

proc ::InstallJammer::CreateDir { dir {log 1} } {
    variable CreateDir

    if {![info exists CreateDir($dir)]} {
        set list [file split $dir]

        for {set i 0} {$i < [llength $list]} {incr i} {
            lappend dirlist [lindex $list $i]
            set dir [eval file join $dirlist]
            if {![info exists CreateDir($dir)]} {
                set CreateDir($dir) 1
                if {![file exists $dir]} {
                    file mkdir $dir
                    if {$log} { ::InstallJammer::LogDir $dir }
                }
            }
        }
    }

    return $dir
}

proc ::InstallJammer::DirIsEmpty { dir } {
    set list1 [glob -nocomplain -directory $dir *]
    set list2 [glob -nocomplain -directory $dir -types hidden *]
    set list  [lremove [concat $list1 $list2] $dir/. $dir/..]
    return    [lempty $list]
}

proc ::InstallJammer::PlaceToplevel { path args } {
    array set _args {
        -width      0
        -height     0
        -anchor     "center"
    }
    set _args(-parent) [winfo parent $path]
    array set _args $args

    if {$_args(-parent) eq "."} { set _args(-parent) "" }

    if {$_args(-width) != 0 && $_args(-height) != 0} {
        wm geometry $path $_args(-width)x$_args(-height)
    }

    if {$_args(-parent) eq ""} {
        BWidget::place $path $_args(-width) $_args(-height) $_args(-anchor)
    } else {
        BWidget::place $path $_args(-width) $_args(-height) $_args(-anchor) \
            $_args(-parent)
    }
}

proc ::InstallJammer::PlaceWindow { id args } {
    set id [::InstallJammer::ID $id]
    set anchor center

    if {[winfo exists $id]} {
        set target $id
    } else {
        set target [$id window]
        if {![$id get Anchor anchor]} { set anchor center }
    }

    array set data "
	-anchor $anchor
	-width  [winfo reqwidth  $target]
	-height [winfo reqheight $target]
    "

    array set data $args

    set w  $data(-width)
    set h  $data(-height)
    set sw [winfo screenwidth $target]
    set sh [winfo screenheight $target]
    lassign [wm maxsize .] maxw maxh
    set anchor $data(-anchor)
    switch -- $anchor {
	"center" {
	    set x0 [expr ($sw - $w) / 2 - [winfo vrootx $target]]
	    set y0 [expr ($sh - $h) / 2 - [winfo vrooty $target]]
	}

	"n" {
	    set x0 [expr ($sw - $w)  / 2 - [winfo vrootx $target]]
	    set y0 20
	}

	"ne" {
	    set x0 [expr $maxw - $w - 40]
	    set y0 20
	}

	"e" {
	    set x0 [expr $maxw - $w - 40]
	    set y0 [expr ($sh - $h) / 2 - [winfo vrooty $target]]
	}

	"se" {
	    set x0 [expr $maxw - $w - 40]
	    set y0 [expr $maxh - $h - 80]
	}

	"s" {
	    set x0 [expr ($sw - $w)  / 2 - [winfo vrootx $target]]
	    set y0 [expr $maxh - $h - 80]
	}

	"sw" {
	    set x0 20
	    set y0 [expr $maxh - $h - 80]
	}

	"w" {
	    set x0 20
	    set y0 [expr ($sh - $h) / 2 - [winfo vrooty $target]]
	}

	"nw" {
	    set x0 20
	    set y0 20
	}

	default {
	    append msg "bad anchor \"$anchor\": must be"
	    append msg "n, ne, e, se, s, sw, w, nw or center"
	    return -code error $msg
	}
    }

    set x "+$x0"
    set y "+$y0"
    if { $x0+$w > $sw } {set x "-0"; set x0 [expr {$sw-$w}]}
    if { $x0 < 0 }      {set x "+0"}
    if { $y0+$h > $sh } {set y "-0"; set y0 [expr {$sh-$h}]}
    if { $y0 < 0 }      {set y "+0"}

    wm geometry $target $x$y
    update
}

proc ::InstallJammer::CenterWindow { target {w 473} {h 335} {lower 0} } {
    set args [list -width $w -height $h]
    if {$lower} { lappend args -anchor s }
    eval [list PlaceWindow $target] $args
}

proc ::InstallJammer::ID { args } {
    set alias [string trim [join $args]]
    if {[info exists ::InstallJammer::aliases($alias)]} {
        return $::InstallJammer::aliases($alias)
    }
    return $alias
}

proc ::InstallJammer::FindCommonPane { pane } {
    foreach id [Common children] {
        if {[string equal [$id component] $pane]} { return $id }
    }
}

proc ::InstallJammer::FindObjByName { name objects } {
    foreach object $objects {
        if {[string equal [$object name] $name]} { return $object }
    }
}

proc ::InstallJammer::GetPaneProc { id prefix } {
    set proc $prefix.$id
    if {![::InstallJammer::CommandExists $proc]} {
    	set proc $prefix.[$id component]
    }
    if {[::InstallJammer::CommandExists $proc]}  { return $proc }
}

proc ::InstallJammer::CurrentObject { {command "get"} {id ""} } {
    global conf
    global info

    if {$command eq "get"} {
        set id [lindex $conf(ObjectStack) end]
    } elseif {$command eq "pop"} {
        set id [lindex $conf(ObjectStack) end]
        set conf(ObjectStack) [lrange $conf(ObjectStack) 0 end-1]
    } elseif {$command eq "push" && $id ne ""} {
        lappend conf(ObjectStack) $id
    }

    set info(CurrentObject) [lindex $conf(ObjectStack) end]

    return $id
}

proc ::InstallJammer::ExecuteActions { id args } {
    global conf
    global info

    array set _args {
        -when       ""
        -type       ""
        -parent     ""
        -conditions 1
    }
    array set _args $args

    set id [::InstallJammer::ID $id]

    if {![::InstallJammer::ObjExists $id]} { return 1 }

    if {[$id is action]} {
        set idlist [list $id]
    } else {
        set idlist [$id children]
    }

    if {![llength $idlist]} { return 1 }

    if {[debugging ison]} {
        set word action
        if {[$id is pane]} { set word "actions for pane" }
        if {[$id is actiongroup]} { set word "action group" }

        set msg "Executing $word"
        if {[catch { $id title } title]} {
            append msg " $id"
        } else {
            append msg " $title ($id)"
        }

        if {$_args(-when) ne ""} {
            append msg " [string tolower $_args(-when)]"
        }

        debug $msg
    }

    set res 1
    set conf(moveToPane) ""
    foreach id $idlist {
        if {![$id active]} { continue }

        set obj  $id
        set type [$obj component]

        if {$_args(-type) ne "" && $type ne $_args(-type)} { continue }

        ## If we have a parent, it means that we're an ExecuteAction
        ## action that is calling another action, which can also be
        ## an action group.  We want the action we're executing
        ## to have the attributes of the parent action, so we create
        ## a temporary object that inherits the attributes of the
        ## parent and execute that in place of the original object.

        if {$_args(-parent) ne ""} {
            set obj [::InstallJammer::CreateTempAction $_args(-parent) $id]
            lappend tempObjects $obj
        }

        if {$_args(-when) ne ""
            && [$obj get ExecuteAction when]
            && ![string equal -nocase $_args(-when) $when]} { continue }

        set info(CurrentAction) $id
        ::InstallJammer::CurrentObject push $id

        set when "Before Action is Executed"
        if {$_args(-conditions) && ![$obj checkConditions $when]} {
            if {[debugging ison]} {
                set msg "Skipping action"
                if {[$id title] eq ""} {
                    append msg " $id"
                } else {
                    append msg " [$id title] ($id)"
                }
                append msg ". Conditions failed."
                debug $msg $id
            }
            ::InstallJammer::CurrentObject pop
            continue
        }

        set when "Before Next Action is Executed"
        while {1} {
            $obj execute
            
            ## Check the after conditions.  If the conditions fail,
            ## we want to repeat the action until they succeed.
            ## This is mainly for console actions to repeat if their
            ## conditions fail, like the user entered bad data or
            ## select a bad option.
            if {!$_args(-conditions) || [$obj checkConditions $when]} { break }
        }

        ::InstallJammer::CurrentObject pop
        if {$conf(moveToPane) ne ""} {
            set res  0
            set pane $conf(moveToPane)
            if {$pane eq "next"} {
                ::InstallJammer::Wizard next 1
            } else {
                ::InstallJammer::Wizard raise $pane
            }
            break
        }
    }

    if {[info exists tempObjects]} {
        ::obj::object destroy {*}$tempObjects
    }

    return $res
}

proc ::InstallJammer::CreateTempAction { id child } {
    set obj [Action new -temp 1 -parent [$id parent] \
        -setup [$id setup] -type action -id $child -name [$child name] \
        -component [$child component] -conditions [$child conditions] \
        -operator [$child operator] -title [$child title]]

    return $obj
}

## Uses the wizard's -backcommand option.
proc ::InstallJammer::BackCommand { wizard id } {
    global info

    set when "Before Previous Pane is Displayed"

    if {![$id checkConditions $when]} { return 0 }

    set res [::InstallJammer::ExecuteActions $id -when $when]

    set info(UserMovedBack) 1
    set info(UserMovedNext) 0

    return $res
}

## This command is executed when the user hits next but before
## the next pane is displayed.
## Uses the wizard's -nextcommand option.
proc ::InstallJammer::NextCommand { wizard id } {
    global info

    set when "Before Next Pane is Displayed"

    if {![$id checkConditions $when]} { return 0 }

    set res [::InstallJammer::ExecuteActions $id -when $when]

    set info(UserMovedBack) 0
    set info(UserMovedNext) 1

    return $res
}

## This command is executed before the installer is cancelled.
## Uses the wizard's -cancelcommand option.
proc ::InstallJammer::CancelCommand { wizard id } {
    set when "Before Pane is Cancelled"

    if {![$id checkConditions $when]} { return 0 }

    return [::InstallJammer::ExecuteActions $id -when $when]
}

## Uses the wizard's <<WizardCancel>> event.
proc ::InstallJammer::CancelEventHandler { wizard } {
    global info
    #set id [$wizard raise]

    #set when "After Pane is Cancelled"
    #::InstallJammer::ExecuteActions $id -when $when

    if {!$info(WizardStarted)} {
        ::InstallJammer::exit 0
        return
    }

    if {![$wizard itemcget cancel -hide]
        && [$wizard itemcget cancel -state] eq "normal"} {
        ::InstallJammer::exit 1
    }
}

## This command is executed before the installer is finished.
## Uses the wizard's -finishcommand option.
proc ::InstallJammer::FinishCommand { wizard id } {
    set when "Before Pane is Finished"

    if {![$id checkConditions $when]} { return 0 }

    return [::InstallJammer::ExecuteActions $id -when $when]
}

## Uses the wizard's <<WizardFinish>> event.
proc ::InstallJammer::FinishEventHandler { wizard } {
    set id [$wizard raise]

    set when "After Pane is Finished"
    ::InstallJammer::ExecuteActions $id -when $when

    ::InstallJammer::exit
}

## Uses the wizard's <<WizardStep>> event.
proc ::InstallJammer::RaiseEventHandler { wizard } {
    global conf
    global info

    set id [$wizard raise]

    set info(CurrentPane) $id

    set when "Before Pane is Displayed"
    if {![$id active] || ![$id checkConditions $when]} {
        ## This pane is inactive or doesn't meet its conditions.
        ## Remove it from the order of the wizard and continue on
        ## to the next pane.

        if {[debugging ison]} {
            set msg "Skipping pane"
            if {[$id title] eq ""} {
                append msg " $id"
            } else {
                append msg " [$id title] ($id)"
            }

            if {![$id active]} {
                append msg ". Pane is inactive."
            } else {
                append msg ". Conditions failed."
            }
            debug $msg $id
        }

        $wizard order [lrange [$wizard order] 0 end-1]
        ::InstallAPI::WizardAPI -do next
        return
    }

    if {[debugging ison]} {
        set msg "Displaying pane"
        if {[$id title] eq ""} {
            append msg " $id"
        } else {
            append msg " [$id title] ($id)"
        }
        debug $msg $id
    }

    ::InstallJammer::ExecuteActions $id -when $when

    ## If the wizard isn't currently displaying our object, then
    ## that means that the previous actions have moved us somewhere
    ## else in the wizard.  We want to just stop.
    if {$id ne [$wizard raise]} { return }

    ## If the last step we displayed was a window and not part
    ## of the wizard, we need to withdraw the window as we move on.
    if {[info exists conf(LastStepId)] && [$conf(LastStepId) is window]} {
        set window [$conf(LastStepId) window]
        ::InstallJammer::TransientParent $window 1
        wm withdraw $window
    } else {
        ::InstallJammer::TransientParent $wizard 1
    }

    ## If this component is a window, we need to withdraw the
    ## wizard and display it.
    if {[$id is window]} {
        set base [$id window]
        ::InstallJammer::TransientParent $base

        wm withdraw $wizard 

        ::InstallJammer::UpdateWidgets

        if {![$id get Modal  modal]}  { set modal  0 }
        if {![$id get Dialog dialog]} { set dialog 0 }

        if {[winfo exists $base]} {
            wm deiconify $base
            raise $base
            if {$modal} { ::InstallJammer::Grab set $base }
        }

        update

        set when "After Pane is Displayed"
        ::InstallJammer::ExecuteActions $id -when $when

        if {[winfo exists $base]} {
            if {$dialog} {
                if {[$id get DialogVairiable varName]} {
                    tkwait variable $varName
                } else {
                    tkwait window $base
                }
            }

            if {$modal} { ::InstallJammer::Grab release $base }
        }
    } else {
        ::InstallJammer::TransientParent $wizard
        ::InstallJammer::UpdateWidgets -wizard $wizard -step $id -buttons 1

        $wizard show
        focus [$wizard widget get next]
        update

        set when "After Pane is Displayed"
        ::InstallJammer::ExecuteActions $id -when $when
    }

    set info(WizardLastStep)  0
    set info(WizardFirstStep) 0

    set conf(LastStepId) $id
}

proc ::InstallJammer::IsButtonWidget {widget {varName ""}} {
    global conf
    if {$varName ne ""} { upvar 1 $varName name }

    set widget [join $widget ""]
    if {$widget in $conf(ButtonWidgets)} {
        set name [string tolower [string map {Button ""} $widget]]
        return 1
    }
    return 0
}

proc ::InstallJammer::UpdateWizardButtons { args } {
    global info
    variable HiddenWidgets

    if {[llength $args]} {
        lassign $args wizard id
    } else {
        set wizard $info(Wizard)
        set id [$wizard raise]
    }

    if {![$id get Buttons buttons]} { return }

    foreach button [list back next cancel finish help] {
        if {![$wizard exists $button]} { continue }

        set hide 0
        if {[info exists HiddenWidgets($button)]} {
            set hide $HiddenWidgets($button)
        }

        set text [string totitle $button]
        if {[string match "*$text*" $buttons]} {
            set w [$wizard widget get $button -step $id]
            ::InstallJammer::SetText $w $id [string toupper $button 0]Button
        } else {
            set hide 1
        }

        $wizard itemconfigure $button -hide $hide
    }
}

proc ::InstallJammer::Wizard { args } {
    global info

    set wizard $info(Wizard)

    if {![llength $args]} { return $wizard }

    set command [lindex $args 0]
    set args    [lrange $args 1 end]

    set id [::InstallJammer::ID [lindex $args 0]]

    switch -- $command {
        "back" {
            if {![llength $args]} { set args 1 }
            eval [list $info(Wizard) back] $args
        }

        "next" {
            if {![llength $args]} { set args 1 }
            eval [list $info(Wizard) next] $args
        }

        "create" {
            ::InstallJammer::CreateWindow $wizard $id
        }

        "raise" {
            set args [lreplace $args 0 0 $id]
            if {[llength $args] == 1} { lappend args 1 }
            $info(Wizard) order [concat [$info(Wizard) order] $id]
            eval [list $info(Wizard) raise] $args
        }

        "show" {
            $wizard show
        }

        "hide" {
            if {$id eq ""} {
                $wizard hide
            } else {
                wm withdraw [$id window]
            }
        }
    }
}

proc ::InstallJammer::CreateWindow { wizard id {preview 0} } {
    set id    [::InstallJammer::ID $id]
    set pane  [$id component]
    set istop [$id is window]

    set base  .[$id name]

    if {$istop} {
        if {[winfo exists $base]} { return $base }
    } else {
        if {[$wizard exists $id] && ($preview || [$id created])} {
            return [$wizard widget get $id]
        }
    }

    set parent [$id parent]

    if {$preview && ![$wizard exists $id]} {
        set parent root
        $id get WizardOptions opts
        eval [list $wizard insert step end $parent $id] $opts
    }

    if {!$preview && [$wizard exists $id]} {
        $wizard itemconfigure $id \
            -backcommand   [list ::InstallJammer::BackCommand  $wizard $id]  \
            -nextcommand   [list ::InstallJammer::NextCommand  $wizard $id]  \
            -cancelcommand [list ::InstallJammer::CancelCommand $wizard $id] \
            -finishcommand [list ::InstallJammer::FinishCommand $wizard $id]

        $id created 1
    }

    set proc [GetPaneProc $id CreateWindow]

    if {[string length $proc]} {
        if {!$istop} {
            $wizard createstep $id

            $proc $wizard $id
            set base [$wizard widget get $id]
            $id window $base
        } else {
            $id window $base
            $proc $wizard $id
        }
    }

    return $base
}

proc ::InstallJammer::TransientParent { {parent ""} {remove 0} } {
    global conf

    if {$parent ne ""} {
        if {$remove} {
            set conf(ParentWindow) [lremove $conf(ParentWindow) $parent]
        } else {
            lappend conf(ParentWindow) $parent
        }
    }

    set parent ""
    if {[info exists conf(ParentWindow)]} {
        set windows $conf(ParentWindow)
        set conf(ParentWindow) [list]

        ## Strip out any windows that have been destroyed.
        foreach window $windows {
            if {[winfo exists $window]} {
                lappend conf(ParentWindow) $window
            }
        }

        ## Find the first parent that is actually visible.
        foreach window [lreverse $conf(ParentWindow)] {
            if {[wm state $window] eq "normal"} {
                set parent $window
                break
            }
        }
    }

    if {![winfo exists $parent] || [wm state $parent] ne "normal"} {
        set parent ""
    }

    return $parent
}

proc ::InstallJammer::ParseArgs { arrayName arglist args } {
    upvar 1 $arrayName a

    array set _args $args

    if {[info exists _args(-switches)]} {
        foreach switch $_args(-switches) {
            set a($switch) 0
            set s($switch) 1
        }
    }

    if {[info exists _args(-options)]} {
        array set o $_args(-options)
        foreach {option default} $_args(-options) {
            set a($option) $default
        }
    }

    set a(_ARGS_) [list]

    set len [llength $arglist]
    for {set i 0} {$i < $len} {incr i} {
        set arg [lindex $arglist $i]

        if {[info exists s($arg)]} {
            set a($arg) 1
        } elseif {[info exists o($arg)]} {
            set a($arg) [lindex $arglist [incr i]]
        } else {
            set a(_ARGS_) [lrange $arglist $i end]
            break
        }
    }
}

proc ::InstallJammer::ObjectRecursiveList { what object } {
    set list {}
    foreach x [$object $what] {
        lappend list $x {*}[::InstallJammer::ObjectRecursiveList $what $x]
    }
    return $list
}

proc ::InstallJammer::SetTitle { w id } {
    set id [::InstallJammer::ID $id]
    set title [::InstallJammer::GetText $id Title]
    wm title $w $title
}

proc ::InstallJammer::SetVirtualText { languages window args } {
    if {[llength $args] == 1} { set args [lindex $args 0] }

    if {[string equal -nocase $languages "all"]} {
        $window set $args
        return
    }

    foreach lang $languages {
        if {$lang eq "None"} {
            global info
            foreach {name value} $args {
                set info($name) $value
                debug "Virtual Text $name is now set to $value"
            }
        } else {
            set lang [::InstallJammer::GetLanguageCode $lang]
            foreach {name value} $args {
                ::msgcat::mcset $lang $window,$name $value
            }
        }
    }
}

proc ::InstallJammer::GetText { id field args } {
    global info

    array set _args {
        -subst      1
        -default    0
        -forcesubst 0
    }
    array set _args $args

    if {$_args(-default)} { set _args(-subst) 0 }

    if {$_args(-default) || ![$id get $field text]} {
        set comp [$id component]
        set text [::msgcat::mc $comp,$field]
        if {$text ne "$comp,$field"} {
            set text "<%${comp},${field}%>"
        } else {
            set text [::msgcat::mc ${field}Text]
            if {$text ne "${field}Text"} {
                set text "<%${field}Text%>"
            } else {
                set text ""
            }
        }
    }

    if {$_args(-forcesubst)
        || ($_args(-subst) && [$id get $field,subst subst] && $subst)} {
        set text [::InstallJammer::SubstText $text]
    }

    return $text
}

proc ::InstallJammer::SetText { args } {
    if {[llength $args] == 3} {
        lassign $args w id field
        set id   [::InstallJammer::ID $id]
        set text [::InstallJammer::GetText $id $field]
    } elseif {[llength $args] == 2} {
        lassign $args w text
    }

    if {![winfo exists $w]} { return }

    set class [winfo class $w]

    if {$class eq "Frame" || $class eq "TFrame"} {
    	foreach child [winfo children $w] {
	    set class [winfo class $child]
	    if {$class eq "Label" || $class eq "TLabel"} {
	    	set w $child
		break
	    }
	}
    }

    if {$class eq "Text"} {
        ## We're using the -maxundo property as a trick for other
        ## code to tell us not to update this widget.
        if {![$w cget -maxundo]} {
            set state [$w cget -state]
            $w configure -state normal
            $w delete 0.0 end
            $w insert end $text
            $w configure -state $state
        }
    } elseif {($class eq "Label" || $class eq "TLabel")
    	&& [string length [$w cget -textvariable]]} {
        set [$w cget -textvariable] $text
    } else {
	$w configure -text $text
    }
}

proc ::InstallJammer::Image { id image } {
    global images

    set id    [::InstallJammer::ID $id]
    set image $id,$image

    if {![ImageExists $image]} { set image [$id component],$image }
    if {![ImageExists $image]} { return }

    set x [::InstallJammer::SubstText $images($image)]
    if {[string index $x 0] eq "@"} { set x [string range $x 1 end] }
    set x [::InstallJammer::ID $x]
    set x [::InstallJammer::Normalize $x unix]
    if {[info exists images($x)]} { return $images($x) }

    if {[::InstallJammer::IsID $x] && [::InstallJammer::ObjExists $x]} {
        set images($x) [image create photo -file [$x srcfile]]
    } elseif {[file exists $x]} {
        set images($x) [image create photo -file $x]
    }

    if {[info exists images($x)]} { return $images($x) }
}

proc ::InstallJammer::SetImage { w id image } {
    set image [::InstallJammer::Image $id $image]
    if {[winfo class $w] eq "TLabel"} { set image [list $image] }
    $w configure -image $image
}

proc ::InstallJammer::ImageExists {img} {
    global images
    return [info exists images($img)]
}

proc ::InstallJammer::GetWidget { widget {id ""} } {
    global info

    if {![info exists info(Wizard)] || ![winfo exists $info(Wizard)]} { return }

    if {$id eq ""} { set id [$info(Wizard) raise] }
    if {$id eq ""} { return }

    while {![$id ispane]} {
        if {$id eq ""} { return }
        set id [$id parent]
    }

    set widget [join [string trim $widget] ""]

    switch -- $widget {
        "BackButton" - "NextButton" - "CancelButton" {
            set widget [string tolower [string range $widget 0 end-6]]
            set widget [$info(Wizard) widget get $widget]
        }

        default {
            if {![winfo exists $widget]} {
                set widget [$id widget get $widget]
            }
        }
    }

    return $widget
}

proc ::InstallJammer::FindUpdateWidgets { textList args } {
    global conf
    global info

    if {![::InstallJammer::WizardExists]} { return }

    set _args(-wizard)  $info(Wizard)
    array set _args $args

    set wizard $_args(-wizard)
    if {![winfo exists $wizard]} { return }

    if {![info exists _args(-step)]} { set _args(-step) [$wizard raise] }
    set step $_args(-step)

    if {$step eq ""} { return }

    set widgets [concat [$step widgets] $conf(ButtonWidgets)]

    ## Remove the trace on the info array so that we don't accidentally
    ## trigger something with our testing.
    trace remove variable ::info write ::InstallJammer::VirtualTextTrace

    set include {}
    foreach virtualtext $textList {
        unset -nocomplain orig
        if {[info exists info($virtualtext)]} { set orig $info($virtualtext) }

        foreach widget $widgets {
            set info($virtualtext) TEST1
            set text1 [::InstallJammer::GetText $step $widget]

            set info($virtualtext) TEST2
            set text2 [::InstallJammer::GetText $step $widget]

            if {$text1 ne $text2} { lappend include $widget }
        }

        if {[info exists orig]} {
            set info($virtualtext) $orig
        } else {
            unset info($virtualtext)
        }
    }

    ## Reset the trace on the info array.
    trace add variable ::info write ::InstallJammer::VirtualTextTrace

    return $include
}

proc ::InstallJammer::UpdateSelectedWidgets { {widgets {}} args } {
    if {![::InstallJammer::WizardExists]} { return }

    if {![llength $widgets]} { set widgets $::conf(UpdateWidgets) }

    if {[llength $args]} {
        set wizard [lindex $args 0]
        set step   [lindex $args 1]
        if {![winfo exists $wizard]} { return }
    } else {
        set wizard $::info(Wizard)
        if {![winfo exists $wizard]} { return }

        set step   [$wizard raise]
    }

    foreach widget $widgets {
        if {[lsearch -exact $::conf(ButtonWidgets) $widget] > -1} {
            set name [string tolower [string map {Button ""} $widget]]
            if {[$wizard exists $name]} {
                set w [$wizard widget get $name -step $step]
                ::InstallJammer::SetText $w $step $widget
            }
        } else {
            set w [$step widget get $widget]

            if {![winfo exists $w]} { continue }

            switch -- [$step widget type $widget] {
                "progress" {
                    set value [::InstallJammer::GetText $step $widget]
                    if {[string is double -strict $value]} {
                        $w configure -value $value
                    }
                }

                "image" {
                    ::InstallJammer::SetImage $w $step $widget
                }

                "text" {
                    ::InstallJammer::SetText $w $step $widget
                }

                "usertext" {
                    if {![$w cget -maxundo]} {
                        $w clear
                        $w insert end [::InstallJammer::GetText $step $widget]
                    }
                }
            }
        }
    }
}

proc ::InstallJammer::UpdateWidgets { args } {
    global conf
    global info

    if {![::InstallJammer::WizardExists]} { return }

    array set _args {
        -update          0
        -buttons         0
        -widgets         {}
        -updateidletasks 0
    }
    set _args(-wizard)  $info(Wizard)
    set _args(-widgets) $conf(UpdateWidgets)

    array set _args $args

    set wizard $_args(-wizard)
    if {![winfo exists $wizard]} { return }

    if {![info exists _args(-step)]} { set _args(-step) [$wizard raise] }
    set step $_args(-step)

    if {$step eq ""} { return }

    if {![llength $_args(-widgets)]} { set _args(-widgets) [$step widgets] }
    
    ::InstallJammer::UpdateSelectedWidgets $_args(-widgets) $wizard $step

    if {$_args(-buttons)} { ::InstallJammer::UpdateWizardButtons $wizard $step }

    set update     $_args(-update)
    set updateIdle $_args(-updateidletasks)
    if {[info exists conf(update)]} {
        set update     $conf(update)
        set updateIdle $conf(update)
    }
    if {$update} { update; set updateIdle 0 }
    if {$updateIdle} { update idletasks }
}

proc ::InstallJammer::DirIsWritable {dir} {
    global conf

    ## Assume wine is always writable.
    if {$conf(wine)} { return 1 }
    if {$conf(windows98)} { return [expr {![catch {file attributes $dir}]}] }
    return [file writable $dir]
}

proc ::InstallJammer::Normalize { file {style ""} } {
    global conf

    if {$file ne ""} {
        set file [eval file join [file split $file]]

        if {[string match "p*" $style]} {
            ## platform
            set style $::tcl_platform(platform)
        }

        switch -glob -- $style {
            "u*" {
                ## unix
                set style forwardslash
                if {[string index $file 1] == ":"} {
                    set file [string range $file 2 end]
                }
            }

            "w*" {
                ## windows
                set style backslash
                if {[string index $file 1] == ":"} {
                    set file [string toupper $file 0]
                }
            }
        }

        switch -glob -- $style {
            "f*" {
                ## forward
                set file [string map [list \\ /] $file]
            }

            "b*" {
                ## backward
                set file [string map [list / \\] $file]
            }
        }
    }

    return $file
}

proc ::InstallJammer::RelativeFile { file {relativeDir "<%InstallDir%>"} } {
    if {[file pathtype $file] eq "relative"} {
        set file [::InstallJammer::SubstText "$relativeDir/$file"]
    }
    return [::InstallJammer::Normalize $file]
}

proc ::InstallJammer::RollbackName { file } {
    global info
    return [file join [file dirname $file] .$info(InstallID).[file tail $file]]
}

proc ::InstallJammer::SaveForRollback {file} {
    file rename -force $file [::InstallJammer::RollbackName $file]
}

proc ::InstallJammer::GetShellFolder { folder } {
    set folder [string toupper $folder]
    array set map {DESKTOP DESKTOPDIRECTORY MYDOCUMENTS PERSONAL}
    if {[info exists map($folder)]} { set folder $map($folder) }
    if {[catch {twapi::get_shell_folder csidl_[string tolower $folder]} path]} {
        return [installkit::Windows::getFolder $folder]
    }
    return $path
}

proc ::InstallJammer::WindowsDir { dir } {
    set dir [string toupper $dir]

    ## We can't trust the WINDOWS directory on some systems apparently,
    ## so it's safer to trust the windir or SYSTEMROOT environment variables.
    if {$dir eq "WINDOWS"
        || [catch { ::InstallJammer::GetShellFolder $dir } windir]} {
        set windir ""
    }

    if {$windir ne ""} { return [::InstallJammer::Normalize $windir windows] }

    ## We couldn't find the directory they were looking for.
    ## See if we can give them something.

    if {[string match "COMMON_*" $dir]} {
        ## Windows 9x doesn't support COMMON_* directories, so let's
        ## see if we can give them the normal one.
        set chk [string range $dir 7 end]
        if {[catch { ::installkit::Windows::getFolder $chk } windir]} {
            set windir ""
        }
        if {[string length $windir]} {
            return [::InstallJammer::Normalize $windir windows]
        }
    }

    set curr {Software\Microsoft\Windows\CurrentVersion}
    set key  "HKEY_LOCAL_MACHINE\\$curr"

    switch -- $dir {
	"MYDOCUMENTS" {
	    set windir [::InstallJammer::WindowsDir PERSONAL]
	}

	"WINDOWS" {
	    if {[info exists ::env(SYSTEMROOT)]} {
		set windir $::env(SYSTEMROOT)
            } elseif {[info exists ::env(windir)]} {
		set windir $::env(windir)
	    } elseif {![catch {registry get $key SystemRoot} result]} {
		set windir $result
	    } else {
		set windir "C:\\Windows"
	    }
	}

	"PROGRAM_FILES" {
	    if {[info exists ::env(ProgramFiles)]} {
		set windir $::env(ProgramFiles)
	    } elseif {![catch {registry get $key ProgramFilesDir} result]} {
		set windir $result
	    } else {
		set windir "C:\\Program Files"
	    }
	}

	"SYSTEM" {
	    set windir [file join [WindowsDir WINDOWS] system]
	}

	"SYSTEM32" {
	    set windir [file join [WindowsDir WINDOWS] system32]
	}

	"QUICK_LAUNCH" {
	    set windir [WindowsDir APPDATA]
	    set windir [file join $windir \
	    	"Microsoft" "Internet Explorer" "Quick Launch"]
	}

	"COMMON_QUICK_LAUNCH" {
	    set windir [WindowsDir COMMON_APPDATA]
	    set windir [file join $windir \
	    	"Microsoft" "Internet Explorer" "Quick Launch"]
	}

	"WALLPAPER" {
	    set windir [registry get $key WallPaperDir]
	}

	default {
            ## We couldn't find the directory.  Let's try one more
            ## time by looking through the known registry values.

            array set regkeys {
                ADMINTOOLS        {USER "Administrative Tools"}
                APPDATA           {USER AppData}
                CACHE             {USER Cache}
                CDBURN_AREA       {USER "CD Burning"}
                COOKIES           {USER Cookies}
                DESKTOP           {USER Desktop}
                FAVORITES         {USER Favorites}
                FONTS             {USER Fonts}
                HISTORY           {USER History}
                INTERNET_CACHE    {USER Cache}
                LOCAL_APPDATA     {USER "Local AppData"}
                LOCAL_SETTINGS    {USER "Local Settings"}
                MYDOCUMENTS       {USER Personal}
                MYMUSIC           {USER "My Music"}
                MYPICTURES        {USER "My Pictures"}
                MYVIDEO           {USER "My Video"}
                NETHOOD           {USER NetHood}
                PERSONAL          {USER Personal}
                PRINTHOOD         {USER PrintHood}
                PROGRAMS          {USER Programs}
                RECENT            {USER Recent}
                SENDTO            {USER SendTo}
                STARTMENU         {USER "Start Menu"}
                STARTUP           {USER Startup}
                TEMPLATES         {USER Templates}

                COMMON_ADMINTOOLS {SYS "Common Administrative Tools"}
                COMMON_APPDATA    {SYS "Common AppData"}
                COMMON_DESKTOP    {SYS "Common Desktop"}
                COMMON_DOCUMENTS  {SYS "Common Documents"}
                COMMON_FAVORITES  {SYS "Common Favorites"}
                COMMON_MUSIC      {SYS CommonMusic}
                COMMON_PICTURES   {SYS CommonPictures}
                COMMON_PROGRAMS   {SYS "Common Programs"}
                COMMON_STARTMENU  {SYS "Common Start Menu"}
                COMMON_STARTUP    {SYS "Common Startup"}
                COMMON_TEMPLATES  {SYS "Common Templates"}
                COMMON_VIDEO      {SYS CommonVideo}
            }

            set SYS  "HKEY_LOCAL_MACHINE\\$curr\\Explorer\\Shell Folders"
            set USER "HKEY_CURRENT_USER\\$curr\\Explorer\\Shell Folders"

            if {[info exists regkeys($dir)]} {
                upvar 0 [lindex $regkeys($dir) 0] regkey
                set val [lindex $regkeys($dir) 1]
                set windir [::installkit::Windows::GetKey $regkey $val]
            }

            ## We still found nothing.  Return the virtual text string.
            if {$windir eq ""} { return <%$dir%> }
	}
    }

    return [::InstallJammer::Normalize $windir windows]
}

proc ::InstallJammer::SetupRegVirtualText {} {
    global info

    set env        {HKEY_LOCAL_MACHINE}
    set user       {HKEY_CURRENT_USER}
    set current    {HKEY_LOCAL_MACHINE}
    append env     {\SYSTEM\CurrentControlSet\Control\Session Manager}
    append current {\Software\Microsoft\Windows\CurrentVersion}

    set info(REG_USER_ENV)        "$user\\Environment"
    set info(REG_SYSTEM_ENV)      "$env\\Environment"
    set info(REG_UNINSTALL)       "$current\\Uninstall"
    set info(REG_CURRENT_VERSION) "$current"

    return
}

proc ::InstallJammer::SetupPlatformVirtualText {} {
    global conf
    global info

    set arch $::tcl_platform(machine)
    if {$conf(windows)} {
        set arch x86
        if {$::env(PROCESSOR_ARCHITECTURE) ne "x86"
            || [info exists ::env(PROCESSOR_ARCHITEW6432)]} { set arch x86_64 }

        set info(WindowsPlatform) "Windows"
        if {$conf(windows)} {
            switch -- $::tcl_platform(os) {
                "Windows 95" { set info(WindowsPlatorm) "Win95" }
                "Windows 98" { set info(WindowsPlatorm) "Win98" }
                "Windows ME" { set info(WindowsPlatorm) "WinME" }
                "Windows NT" {
                    switch -- $::tcl_platform(osVersion) {
                        "4.0" { set info(WindowsPlatorm) "WinNT" }
                        "5.0" { set info(WindowsPlatorm) "Win2k" }
                        "5.1" { set info(WindowsPlatorm) "WinXP" }
                        "5.2" { set info(WindowsPlatorm) "Win2003" }
                        "6.0" { set info(WindowsPlatorm) "Vista" }
                        "6.1" { set info(WindowsPlatorm) "Windows 7" }
                    }
                }
            }
        }
    }

    if {[string match "*86" $arch]} { set arch x86 }
    set info(ProcessorArchitecture) $arch
    set info(PlatformVersion) $::tcl_platform(osVersion)
    set info(Is64Bit) [expr {$arch eq "x86_64"}]
}

proc ::InstallJammer::SubstVar { var } {
    global conf
    global info

    ## If this variable exists in the info array, return its value.
    if {[info exists info($var)]} {
	set string $info($var)
        if {[info exists ::InstallJammer::VTTypes($var)]} {
            if {$::InstallJammer::VTTypes($var) eq "boolean"} {
                set string [string is true $string]
            } elseif {$::InstallJammer::VTTypes($var) eq "directory"} {
                set string [::InstallJammer::Normalize $string platform]
            }
	}
   	return $string
    }

    ## See if this is a virtual text variable that exists in
    ## our message catalog.  If it does, we want to use the
    ## language-specific version instead of a generic.
    if {[::msgcat::mcexists $var]} { return [::msgcat::mc $var] }

    if {![info exists ::InstallJammer::subst]} {
        foreach proc [info commands ::InstallJammer::subst::*] {
            set ::InstallJammer::subst([namespace tail $proc]) $proc
        }
    }

    set idx  [string wordend $var 0]
    set word [string range $var 0 [expr {$idx - 1}]]
    set args [string trim [string range $var $idx end]]

    if {[info exists ::InstallJammer::subst($word)]} {
        return [eval ::InstallJammer::subst::$word $args]
    }

    if {$var ne "" && $conf(windows) && $var eq [string toupper $var]} {
        ## If the string is all uppercase and we haven't matched
        ## something yet, it's a Windows directory.
        return [::InstallJammer::WindowsDir $var]
    }

    return "<%$var%>"
}

proc ::InstallJammer::subst::Date { args } {
    set secs [lindex $args 0]
    if {[string is integer -strict $secs]} {
        set format [join [lrange $args 1 end]]
    } else {
        set secs   [clock seconds]
        set format [join $args]
    }

    return [clock format $secs -format $format]
}

proc ::InstallJammer::subst::Dir { args } {
    set dir      [lindex $args 0]
    set platform [lindex $args 1]
    if {$platform eq ""} { set platform $::tcl_platform(platform) }
    return [::InstallJammer::Normalize $dir $platform]
}

proc ::InstallJammer::subst::Dirname { args } {
    return [file dirname [join $args]]
}

proc ::InstallJammer::subst::DiskSpace { args } {
    return [::InstallJammer::FormatDiskSpace [join $args]]
}

proc ::InstallJammer::subst::DOSName { args } {
    global conf

    set file [join $args]

    if {$conf(windows) && [file exists $file]} { 
        set file [file attributes $file -shortname]
        set file [::InstallJammer::Normalize $file windows]
    }

    return $file
}

proc ::InstallJammer::subst::Env { args } {
    set var [lindex $args 0]
    if {[info exists ::env($var)]} { return $::env($var) }
}

proc ::InstallJammer::subst::FileGroup { args } {
    set group [join $args]
    set obj [::InstallJammer::FindObjByName $group [FileGroups children]]
    if {$obj ne ""} {
        set str [$obj directory]
        set str [::InstallJammer::Normalize $str $::tcl_platform(platform)]
        return $str
    }
}

proc ::InstallJammer::subst::FormatDescription { args } {
    set lines  [join $args]
    set string ""
    foreach line [split [string trim $lines] \n] {
        if {[string trim $line] eq ""} { set line "." }
        append string " $line\n"
    }
    return $string
}

proc ::InstallJammer::subst::GUID { args } {
    global info
    set info(LastGUID) [::InstallJammer::guid]
    return $info(LastGUID)
}

proc ::InstallJammer::subst::InstallInfoDir { args } {
    return [::InstallJammer::InstallInfoDir]
}

proc ::InstallJammer::subst::Property { args } {
    set property [lindex $args end]
    if {[llength $args] == 1} {
        set object [::InstallJammer::CurrentObject]
    } else {
        set object [::InstallJammer::ID [lindex $args 0]]
    }

    return [$object get $property]
}

proc ::InstallJammer::subst::RegValue { args } {
    set key  [lindex $args 0]
    set val  [lindex $args 1]
    return [::installkit::Windows::GetKey $key $val]
}

proc ::InstallJammer::subst::SpaceAvailable { args } {
    global info
    set dir [join $args]
    if {$dir eq ""} { set dir $info(InstallDir) }
    return [::InstallJammer::GetFreeDiskSpace $dir]
}

proc ::InstallJammer::subst::Tail { args } {
    return [file tail [join $args]]
}

proc ::InstallJammer::subst::Temp { args } {
    return [::InstallJammer::TmpDir]
}

proc ::InstallJammer::subst::Tolower { args } {
    return [string tolower $args]
}

proc ::InstallJammer::subst::Toupper { args } {
    return [string toupper $args]
}

proc ::InstallJammer::subst::UUID { args } {
    global info
    set info(LastUUID) [::InstallJammer::uuid]
    return $info(LastUUID)
}

proc ::InstallJammer::SubstForEval { string } {
    set map [list "<%" "\[::InstallJammer::SubstText \{<%" "%>" "%>\}\]"]
    return [string map $map $string]
}

proc ::InstallJammer::SubstForPipe { string } {
    set list [list]
    foreach arg $string {
        lappend list [::InstallJammer::SubstText $arg]
    }
    return $list
}

proc ::InstallJammer::SubstText { str {num 0} } {
    global conf

    if {$num > $conf(VirtualTextRecursionLimit)} { return $str }

    if {$str eq ""} { return }

    set s $str
    set s [string map $conf(VirtualTextMap) $s]
    set s [subst -novariables $s]
    if {$str ne $s} { set s [::InstallJammer::SubstText $s [incr num]] }

    return $s
}
interp alias {} sub {} ::InstallJammer::SubstText
interp alias {} gettext {} ::InstallJammer::GetText

proc settext {args} {
    if {[llength $args] == 2} {
        set ::info($var) $val
    } else {
        ::InstallJammer::SetVirtualText all {*}$args
    }
}

proc vercmp {ver1 ver2} {
    foreach v1 [split $ver1 ._-] v2 [split $ver2 ._-] {
        if {$v1 eq ""} { set v1 0 }
        if {$v2 eq ""} { set v2 0 }
        if {$v1 < $v2} { return -1 }
        if {$v1 > $v2} { return 1 }
    }
    return 0
}

proc ::InstallJammer::HasVirtualText { string } {
    return [string match "*<%*%>*" $string]
}

proc ::InstallJammer::TmpDir { {file ""} } {
    global conf
    global info

    if {![info exists info(TempRoot)] || ![file exists $info(TempRoot)]} {
        set dirs [list]
	if {[info exists ::env(TEMP)]} { lappend dirs $::env(TEMP) }
	if {[info exists ::env(TMP)]}  { lappend dirs $::env(TMP)  }
        if {$conf(windows)} {
            set local [::InstallJammer::WindowsDir LOCAL_APPDATA]
            lappend dirs [file join [file dirname $local] Temp]

            lappend dirs [::InstallJammer::WindowsDir INTERNET_CACHE]

	    lappend dirs C:/Windows/Temp
	    lappend dirs C:/WINNT/Temp
            lappend dirs C:/Temp

	} else {
	    lappend dirs /usr/tmp /tmp /var/tmp
	}

	foreach dir $dirs {
	    if {[DirIsWritable $dir]} {
                if {[info exists ::InstallJammer]} {
                    if {$file ne ""} { set dir [file join $dir $file] }
                    return $dir
                }
                set info(TempRoot) [::InstallJammer::Normalize $dir forward]
                break
            }
        }

        if {![info exists info(TempRoot)]} {
            if {[info exists ::env(TMP)]} { set tmp $::env(TMP) }
            if {[info exists ::env(TEMP)]} { set tmp $::env(TEMP) }
            if {![info exists tmp] || [catch {file mkdir $tmp}]} {
                return -code error \
                    "could not find a suitable temporary directory"
            }
            set info(TempRoot) $tmp
        }
    }

    if {![info exists info(Temp)]} {
        set info(Temp) [::InstallJammer::Normalize \
            [file join $info(TempRoot) ijtmp_[::InstallJammer::uuid]]]
    }

    if {![file exists $info(Temp)]} { file mkdir $info(Temp) }

    if {$file ne ""} {
	return [::InstallJammer::Normalize [file join $info(Temp) $file]]
    }

    return $info(Temp)
}

proc ::InstallJammer::TmpFile {} {
    global conf
    return [::InstallJammer::TmpDir [pid]-[incr0 conf(tmpFileCount)]]
}

proc ::InstallJammer::TmpMount {} {
    variable tmpMountCount

    if {![info exists tmpMountCount]} { set tmpMountCount 0 }

    while {1} {
        set mnt /installjammervfs[incr tmpMountCount]
        if {![file exists $mnt]} { break }
    }

    return $mnt
}

proc ::InstallJammer::ModifyInstallDir {} {
    global conf
    global info

    set dir [::InstallJammer::SubstText $info(InstallDir)]

    if {[info exists info(InstallDirSuffix)]} {
        set suf [::InstallJammer::SubstText $info(InstallDirSuffix)]

        set dir [::InstallJammer::Normalize $dir forward]
        set suf [::InstallJammer::Normalize $suf forward]
        if {![string match "*$suf" $dir]} { set dir [file join $dir $suf] }
    }

    if {[file pathtype $dir] eq "relative"} { set dir [file normalize $dir] }

    set info(InstallDir) [::InstallJammer::Normalize $dir platform]

    if {$conf(windows)} {
        set info(InstallDrive) [string range $info(InstallDir) 0 1]
    }
}

proc ::InstallJammer::ModifyInstallTitle {} {
    if {[info exists ::tk_patchLevel]} {
        set title [::InstallJammer::SubstText "<%InstallTitleText%>"]
        wm title . $title
        if {[info exists ::info(Wizard)] && [winfo exists $::info(Wizard)]} {
            $::info(Wizard) configure -title $title
        }
    }
}

proc ::InstallJammer::GetInstallInfoDir { {create 0} } {
    global conf
    global info

    if {![info exists info(InstallJammerRegistryDir)]} {
        if {$conf(windows)} {
            set root [::InstallJammer::WindowsDir PROGRAM_FILES]
            if {![::InstallJammer::DirIsWritable $root]} {
                ## If the Program Files directory is not writable
                ## it means this user has no permissions on this
                ## system.  We need to store our registry in the
                ## Application Data directory.
                set root [::InstallJammer::WindowsDir APPDATA]
            }

            set dir  [file join $root "InstallJammer Registry"]
        } else {
            if {[id user] eq "root"} {
                set dir "/var/lib/installjammer"
            } else {
                set dir "[::InstallJammer::HomeDir]/.installjammerinfo"
            }
        }

        set info(InstallJammerRegistryDir) [::InstallJammer::Normalize $dir]
    }

    if {![info exists info(InstallInfoDir)]} {
        set id $info(ApplicationID)
        if {[info exists info(UpgradeInstall)] && $info(UpgradeInstall)} {
            set id $info(UpgradeApplicationID)
        }

        set dir [file join $info(InstallJammerRegistryDir) $id]
        set info(InstallInfoDir) [::InstallJammer::Normalize $dir]
    }

    if {$create && ![file exists $info(InstallInfoDir)]} {
        ::InstallJammer::CreateDir $info(InstallInfoDir) 0
        if {$conf(windows)} {
            file attributes [file dirname $info(InstallInfoDir)] -hidden 1
        }
    }

    if {[info exists ::InstallJammer]} {
        set infodir $info(InstallInfoDir)
        unset info(InstallInfoDir) info(InstallJammerRegistryDir)
        return $infodir
    } else {
        return $info(InstallInfoDir)
    }
}

proc ::InstallJammer::InstallInfoDir { {file ""} } {
    set dir [::InstallJammer::GetInstallInfoDir 1]
    if {[string length $file]} { append dir /$file }
    return $dir
}

proc ::InstallJammer::SetPermissions { file perm } {
    if {$perm eq ""} { return }

    if {$::tcl_platform(platform) eq "windows"} {
	if {[string length $perm] > 4} { return }
	lassign [split $perm ""] a h r s
	file attributes $file -archive $a -hidden $h -readonly $r -system $s
    } else {
        file attributes $file -permissions $perm
    }
}

proc ::InstallJammer::EvalCondition { condition } {
    if {[string is true  $condition]} { return 1 }
    if {[string is false $condition]} { return 0 }

    set test [::InstallJammer::SubstForEval $condition]

    if {![string length $test]} { return 1 }
    if {[catch {expr [subst $test]} result]} {
        set msg "Error in condition '$condition'\n\n$::errorInfo"
        return -code error $msg
    }
    return $result
}

proc ::InstallJammer::HomeDir { {file ""} } {
    set return [file normalize ~]
    if {$file ne ""} { set return [file join $return $file] }
    return $return
}

proc ::InstallJammer::PauseInstall {} {
    global conf
    global info

    if {$info(Installing) && [info exists conf(pause)]} {
        ::InstallJammer::TmpDir
        close [open $conf(pause) w]
    }
}

proc ::InstallJammer::ContinueInstall {} {
    global conf
    if {[info exists conf(pause)]} { file delete $conf(pause) }
}

proc ::InstallJammer::StopInstall {} {
    global conf
    global info
    if {$info(Installing) && [info exists conf(stop)]} {
        ::InstallJammer::TmpDir
        close [open $conf(stop) w]
        set info(InstallStopped) 1
    }
}

proc ::InstallJammer::PauseCheck {} {
    global conf
    global info

    if {$info(InstallStopped)} { return 0 }

    while {[file exists $conf(pause)]} {
	if {[file exists $conf(stop)]} {
	    set info(InstallStopped) 1
            return 0
	}
	after 500
    }

    return 1
}

proc ::InstallJammer::UninstallFile {file} {
    file delete -force $file
}

proc ::InstallJammer::UninstallDirectory { dir {force ""} } {
    file delete $force $dir
}

## Uninstall a registry key.  If value is specified, we only want to delete
## that value from the registry key.  If, once the value has been deleted,
## the registry key is empty, we will delete that as well.
proc ::InstallJammer::UninstallRegistryKey {key {value ""}} {
    if {![lempty $value]} {
	catch { registry delete $key $value }
	if {[catch { registry keys $key } keys]} { return }
	if {[catch { registry values $key } values]} { return }
	if {[lempty $keys] && [lempty $values]} {
	    UninstallRegistryKey $key
	}
    } else {
	catch { registry delete $key }
    }
}

proc ::InstallJammer::LogDir { dir } {
    global conf
    global info

    if {!$conf(logInit)} {
        set conf(logInit) 1
        ::InstallJammer::TmpDir
        ::InstallJammer::GetInstallInfoDir
    }

    set dir [::InstallJammer::Normalize $dir forward]
    if {![string match $info(InstallInfoDir)* $dir]
        && ![string match $info(Temp)* $dir]} {
        ::InstallJammer::InstallLog [list :DIR $dir]
    }
}

proc ::InstallJammer::LogFile { file } {
    global conf
    global info

    if {!$conf(logInit)} {
        set conf(logInit) 1
        ::InstallJammer::TmpDir
        ::InstallJammer::GetInstallInfoDir
    }

    set file [::InstallJammer::Normalize $file forward]
    if {![string match $info(InstallInfoDir)* $file]
        && ![string match $info(Temp)* $file]} {
        ::InstallJammer::InstallLog [list :FILE $file]
    }
}

proc ::InstallJammer::SetVersionInfo { file {version ""} } {
    global info
    global versions
    if {$version eq ""} { set version $info(InstallVersion) }
    set versions($file) $version
}

proc ::InstallJammer::StoreLogsInUninstall {} {
    global conf
    global info

    if {[info exists conf(uninstall)]} {
        set tmp [::InstallJammer::TmpDir]

        foreach file [glob -nocomplain -dir $tmp *.info] {
            lappend files $file
            lappend names [file tail $file]

            set file [file root $file].log

            if {[file exists $file]} {
                lappend files $file
                lappend names [file tail $file]
            }
        }

        foreach file [glob -nocomplain -dir $tmp *.dead] {
            lappend files $file
            lappend names [file tail $file]
        }

        installkit::addfiles $conf(uninstall) $files $names
    }
}

proc ::InstallJammer::SetDialogArgs {which arrayName} {
    global conf
    upvar 1 $arrayName _args

    set parent [::InstallJammer::TransientParent]

    set _args(-parent)        $parent
    set _args(-transient)     [expr {$parent ne ""}]
    set _args(-usenative)     $conf(Native$which)
    set _args(-placerelative) [expr {$parent ne "" && $parent ne "."}]
}

proc ::InstallJammer::MessageBox { args } {
    global conf
    global widg

    ::InstallJammer::InitializeGui

    set win  .__message_box

    array set _args {
        -type        "ok"
        -buttonwidth 12
    }
    ::InstallJammer::SetDialogArgs MessageBox _args

    if {[info exists ::InstallJammer]} {
        set _args(-title) "InstallJammer"
    } else {
        set _args(-title) [::InstallJammer::SubstText "<%AppName%>"]
    }

    array set _args $args
    if {$_args(-title) eq ""} { set _args(-title) " " }

    set type $_args(-type)
    if {!$_args(-usenative) && $type ne "user"} {
        set idx 0

        set cancel     -1
        set default    -1
        set buttonlist {Retry OK Yes No Cancel}
        switch -- $type {
            "abortretryignore" {
                set default 0
                set buttonlist {Abort Retry Ignore}
            }

            "ok"          { set default 0 }
            "okcancel"    { set default 0; set cancel 1 }
            "retrycancel" { set default 0; set cancel 1 }
            "yesno"       { set default 0; set cancel 1 }
            "yesnocancel" { set default 0; set cancel 2 }
        }

        if {![info exists _args(-cancel)]} { set _args(-cancel) $cancel }
        if {![info exists _args(-default)]} { set _args(-default) $default }

        foreach button $buttonlist {
            set lbutton [string tolower $button]
            if {[string first $lbutton $type] > -1} {
                lappend buttons $lbutton
                lappend _args(-buttons) [::InstallJammer::SubstText <%$button%>]

                if {[info exists _args(-default)]
                    && $_args(-default) eq $lbutton} {
                    set _args(-default) $idx
                }

                incr idx
            }
        }

        if {[llength $buttons] == 1} { set _args(-default) 0 }

        set _args(-type) user
    }

    set result [eval [list MessageDlg $win] [array get _args]]

    if {!$_args(-usenative) && $type ne "user"} {
        return [lindex $buttons $result]
    }
    return $result
}

proc ::InstallJammer::Message { args } {
    global conf

    if {[info exists conf(cmdlineMessages)]} {
        set cmdline $conf(cmdlineMessages)
    } else {
        set cmdline [expr {![info exists ::tk_patchLevel]}]
    }

    if {[info exists ::InstallJammer]
        && $::tcl_platform(platform) eq "windows"
        && [file extension [info nameof]] eq ".com"} { set cmdline 1 }

    if {$cmdline} {
        set _args(-icon) "info"
	array set _args $args
	if {![info exists _args(-message)]} { return }

        set chan stdout
        if {$_args(-icon) eq "error"} { set chan stderr }

        puts  $chan "$_args(-message)"
        flush $chan
    } else {
        if {[catch { eval ::InstallJammer::MessageBox $args } error]} {
            if {[info exists ::conf(unix)] && $::conf(unix)} {
                catch {rename ::tk_messageBox ""}
            }
            eval tk_messageBox -title "InstallJammer" $args
        }
    }
}

proc ::InstallJammer::HandleThreadError { tid errorMsg } {
    global info

    set message "Error in thread $tid: $errorMsg"
    if {$info(Installing)} {
        ::InstallJammer::UnpackOutput $message
        ::InstallJammer::UnpackOutput :DONE
    } else {
        ::InstallJammer::MessageBox -message $message
    }
}

proc ::InstallJammer::ChooseDirectory { args } {
    global conf

    ::InstallJammer::SetDialogArgs ChooseDirectory _args

    set _args(-title) \
        [::InstallJammer::SubstText "<%PromptForDirectoryTitle%>"]
    set _args(-message) \
        [::InstallJammer::SubstText "<%PromptForDirectoryMessage%>"]
    set _args(-newfoldertext) \
        [::InstallJammer::SubstText "<%PromptForDirectoryNewFolderText%>"]
    set _args(-oktext)     [::InstallJammer::SubstText "<%OK%>"]
    set _args(-canceltext) [::InstallJammer::SubstText "<%Cancel%>"]
    array set _args $args

    if {[info exists _args(-variable)]} {
        upvar 1 $_args(-variable) dir
        unset _args(-variable)

        if {![info exists _args(-initialdir)] && [info exists dir]} {
            set _args(-initialdir) $dir
        }
    }

    if {$_args(-usenative)} {
        set _args(-title) $_args(-message) 
        set res [eval ij_chooseDirectory [array get _args]]
    } else {
        unset -nocomplain _args(-usenative)
        if {[llength $conf(ParentWindow)] > 1} {
            wm withdraw [lindex $conf(ParentWindow) end]
        }

        set res [eval ::ChooseDirectory .__choose_directory [array get _args]]

        if {[llength $conf(ParentWindow)] > 1} {
            wm deiconify [lindex $conf(ParentWindow) end]
        }
    }

    if {$res ne ""} {
        set dir $res
        return $dir
    }
}

proc ::InstallJammer::ChooseFile { args } {
    global conf

    ::InstallJammer::SetDialogArgs ChooseFile _args
    array set _args $args

    if {[info exists _args(-variable)]} {
        upvar 1 $_args(-variable) file
        unset _args(-variable)
    }

    if {$_args(-usenative)} {
        set type Open
        if {[info exists _args(-type)]} {
            set type [string toupper $_args(-type) 0]
        }
        set res [eval [list ij_get${type}File] [array get _args]]
    } else {
        unset -nocomplain _args(-usenative)
        if {[llength $conf(ParentWindow)] > 1} {
            wm withdraw [lindex $conf(ParentWindow) end]
        }

        set res [eval ::ChooseFile .__choose_file [array get _args]]

        if {[llength $conf(ParentWindow)] > 1} {
            wm deiconify [lindex $conf(ParentWindow) end]
        }
    }

    if {$res ne ""} {
        set file $res
        return $file
    }
}

proc ::InstallJammer::CommandExists { proc } {
    return [string length [info commands $proc]]
}

proc ::InstallJammer::uuid {} {
    global conf

    if {$conf(windows)} {
        return [string range [::InstallJammer::guid] 1 end-1]
    }

    set sha [sha1hex [info hostname][clock seconds][pid][info cmdcount]]

    set i 0
    foreach x {8 4 4 4 12} {
        lappend list [string range $sha $i [expr {$i + $x - 1}]]
        incr i $x
    }

    return [string toupper [join $list -]]
}

proc ::InstallJammer::guid {} {
    global conf
    if {$conf(windows)} {
        return [string toupper [::installkit::Windows::guid]]
    }
    return \{[string toupper [::InstallJammer::uuid]]\}
}

proc ::InstallJammer::IsID { id } {
    if {[string length $id] != 36} { return 0 }
    set list [split $id -]
    if {[llength $list] != 5} { return 0 }
    set i 0
    foreach n {8 4 4 4 12} {
        if {[string length [lindex $list $i]] != $n} { return 0 }
        incr i
    }
    return 1
}

proc ::InstallJammer::ObjExists { obj } {
    return [::obj::object exists $obj]
}

proc ::InstallJammer::ReadMessageCatalog { catalog } {
    set catalog [file join $::installkit::root catalogs $catalog]
    eval [read_file $catalog -encoding utf-8]
}

proc ::InstallJammer::Wrap { args } {
    global conf
    global info

    set include 1
    if {[set x [lsearch -exact $args -noinstall]] > -1} {
        set include 0
        set args [lreplace $args $x $x]
    }

    if {$include} {
        set pkgdir [file join $::installkit::root lib InstallJammer]
        set args [linsert $args 0 -package $pkgdir]
    }

    eval ::installkit::wrap $args
}

proc ::InstallJammer::BaseInstallkit {{file ""}} {
    global info

    if {$file eq ""} { set file [TmpDir installkit$info(Ext)] }
    if {![file exists $file]} { set file [::InstallJammer::Wrap -o $file] }
    return [::InstallJammer::Normalize $file]
}

proc ::InstallJammer::Mount {archive mountPoint} {
    crapvfs::mount $archive $mountPoint
}

proc ::InstallJammer::Unmount {mountPoint} {
    crapvfs::unmount $mountPoint
}

proc ::InstallJammer::Grab { command args } {
    variable GrabStack

    if {![info exists GrabStack]} { set GrabStack [list] }

    switch -- $command {
        "current" {
            return [lindex $GrabStack end]
        }

        "stack" {
            return $GrabStack
        }

        "release" {
            set window [lindex $args 0]
            set search [lsearch -exact $GrabStack $window]
            if {$search < 0} { return }
            if {$search != [expr {[llength $GrabStack] - 1}]} {
                return -code error "$window is not last in the grab stack."
            }
            grab release $window
            set GrabStack [lreplace $GrabStack end end]
            if {[llength $GrabStack]} { grab [lindex $GrabStack end] }
        }

        "set" {
            set window [lindex $args 0]
            grab $window
            if {$window ni $GrabStack} { lappend GrabStack $window }
        }

        default {
            grab $command
            if {$command ni $GrabStack} { lappend GrabStack $command }
        }
    }

    return
}

proc ::InstallJammer::HasKDEDesktop {} {
    global info

    set home [::InstallJammer::HomeDir]
    set kde  [file join $home .kde]

    if {![file exists $kde]} { return 0 }

    if {![info exists info(KDEDesktop)] || [lempty $info(KDEDesktop)]} {
	set globals [file join $kde share config kdeglobals]
	set desktop [file join $home Desktop]
	if {[catch {open $globals} fp]} { return 0 }
	while {[gets $fp line] != -1} {
	    if {[regexp {^Desktop=([^\n].*)\n} $line\n trash desktop]} {
		regsub -all {\$([A-Za-z0-9]+)} $desktop {$::env(\1)} desktop
		break
	    }
	}
	close $fp
	set info(KDEDesktop) $desktop
    }

    return [file exists $info(KDEDesktop)]
}

proc ::InstallJammer::HasGnomeDesktop {} {
    global info

    set home [::InstallJammer::HomeDir]

    foreach dir [list .gnome-desktop Desktop] {
        set desktop [file join $home $dir]
        if {[file exists $desktop]} {
            set info(GnomeDesktop) $desktop
            break
        }
    }

    if {[info exists info(GnomeDesktop)] && [file exists $info(GnomeDesktop)]} {
        return 1
    }
    return 0
}

proc ::InstallJammer::GetDesktopEnvironment {} {
    global env

    ## KDE
    if {[info exists env(DESKTOP)] && $env(DESKTOP) eq "kde"} { return KDE }
    if {[info exists env(KDE_FULL_SESSION)]} { return KDE }

    ## Gnome
    if {[info exists env(GNOME_DESKTOP_SESSION_ID)]} { return Gnome }

    if {[info exists env(DESKTOP_SESSION)]} {
        switch -glob -- $env(DESKTOP_SESSION) {
            "*KDE*"   { return KDE }
            "*GNOME*" { return Gnome }
        }
    }

    return "Unknown"
}

proc ::InstallJammer::GetLinuxDistribution {} {
    set lsb_release [auto_execok lsb_release]
    if {[file executable $lsb_release]} {
        if {![catch { exec $lsb_release -i -s } distrib]} { return $distrib }
    }

    foreach lsb_release [list /etc/lsb-release /etc/lsb_release] {
	if {[file readable $lsb_release]} {

	}
    }

    set check {
	/etc/mandrake-release    Mandrake
	/etc/fedora-release      Fedora
	/etc/SuSE-release        SuSE
	/etc/debian_version      Debian
	/etc/gentoo-release      Gentoo
	/etc/slackware-version   Slackware
	/etc/turbolinux-release  TurboLinux
	/etc/yellowdog-release   YellowDog
	/etc/connectiva-release  Connectiva
	/etc/redhat-release      Redhat
    }
}

proc ::InstallJammer::GetFreeDiskSpace { dir } {
    global conf

    if {$conf(windows)} {
        set drive [lindex [file split $dir] 0]
        return [::installkit::Windows::drive freespace $drive]
    }

    set df [auto_execok df]
    if {[file exists $df]} {
        while {![file exists $dir]} {
            set dir [file dirname $dir]
        }
        catch { exec $df -k $dir } output

        set line [join [lrange [split $output \n] 1 end] " "]
        if {![catch { expr {[lindex $line 3] * wide(1024)} } avail]} {
            return $avail
        }
    }

    return -1
}

proc ::InstallJammer::FormatDiskSpace { space } {
    if {$space < 1048576} {
        return [format "%2.2f KB" [expr {$space / 1024.0}]]
    }
    if {$space < 1073741824} {
        return [format "%2.2f MB" [expr {$space / 1048576.0}]]
    }
    return [format "%2.2f GB" [expr {$space / 1073741824.0}]]
}

proc ::InstallJammer::unpack { src dest {permissions ""} } {
    if {![string length $permissions]} { set permissions "0666" }

    # Extract the file and copy it to its location.
    set fin [open $src r]
    if {[catch {open $dest w $permissions} fout]} {
	close $fin
	return -code error $fout
    }

    set intrans  binary
    set outtrans binary
    if {[info exists ::conf(eol,[file extension $dest])]} {
        set trans $::conf(eol,[file extension $dest])
        if {[llength $trans] == 2} {
            set intrans  [lindex $trans 0]
            set outtrans [lindex $trans 1]
        } else {
            set outtrans [lindex $trans 0]
        }
    }

    fconfigure $fin  -translation $intrans
    fconfigure $fout -translation $outtrans

    fcopy $fin $fout

    close $fin
    close $fout
}

proc ::InstallJammer::ExecAsRoot { command args } {
    global conf
    global info

    array set _args {
        -title   ""
        -message "<%PromptForRootText%>"
    }
    array set _args $args

    set wait 0
    if {[info exists _args(-wait)]} { set wait $_args(-wait) }

    set cmd   [list]
    set msg   [sub $_args(-message)]
    set title [sub $_args(-title)]

    set i 0
    set x [llength $command]
    foreach arg $command {
        if {[string first " " $arg] > -1} {
            append cmdline '$arg'
        } else {
            append cmdline $arg
        }
        if {[incr i] < $x} { append cmdline " " }
    }

    if {$info(GuiMode)} {
        if {$conf(osx)} {
            set binary [::InstallJammer::TmpDir "This installer"]
            file copy -force [auto_execok osascript] $binary

            set script [::InstallJammer::TmpDir escalate.sh]
            set fp [open $script w]
            set cmdline [string map {' \\\"} $cmdline]
            set scr "do shell script \"$cmdline\" with administrator privileges"
            puts $fp "\"$binary\" -e '$scr'"
            puts $fp "rm -rf \"[::InstallJammer::TmpDir]\""
            close $fp

            system "sh $script &"
            return 1
        }

        ## Try to find a graphical SU utility we can use.
        if {[::InstallJammer::GetDesktopEnvironment] eq "Gnome"} {
            set list {gksudo gksu gnomesu kdesudo kdesu xsu}
        } else {
            set list {kdesudo kdesu gksudo gksu gnomesu xsu}
        }

        foreach app $list {
            if {[auto_execok $app] eq ""} { continue }
            
            set cmd [list $app $cmdline]
            catch {exec $app --help} help
            if {$app eq "kdesu" || $app eq "kdesudo"} {
                set cmd [linsert $cmd 1 -d -c]
                if {$msg ne "" && [string match "*--comment*" $help]} {
                    set cmd [linsert $cmd 1 --comment $msg]
                }
            } elseif {$app eq "gksu" || $app eq "gksudo"} {
                if {$msg ne "" && [string match "*--message*" $help]} {
                    set cmd [linsert $cmd 1 --message $msg]
                }
            }

            if {!$wait} { lappend cmd & }
            catch { eval exec $cmd }
            return 1
        }
    }

    ## If we didn't find a GUI we could use, and we don't have a
    ## terminal to talk to, we really can't do anything.
    if {!$info(HaveTerminal)} { return 0 }

    ## We never found a good GUI to ask for the root password,
    ## so we'll just ask on the command line.

    if {[string is punct [string index $msg end]]} {
        set msg [string range $msg 0 end-1]
    }

    if {[auto_execok sudo] ne ""} {
        ## Always invalidate the sudo timestamp.  We don't want
        ## someone running an installer as root without knowing it.
        if {[catch {exec sudo -k} err]} { return 0 }

        set cmd [list sudo]
        if {$msg ne ""} { lappend cmd -p "\[sudo\] $msg: " }
        if {$wait} {
            eval exec $cmd $command
        } else {
            if {[catch {eval exec $cmd -v} err]} { return 0 }
            set res [catch {eval system sudo $cmdline &} err]
        }
    } else {
        puts  stdout "$msg\n\[su root\] "
        flush stdout
        if {!$wait} { append cmdline " &" }
        set res [system su -c \"$cmdline\"]
    }

    return 1
}

proc ::InstallJammer::GetFilesForPattern { patternString args } {
    set relative    1
    set patterns    [list]
    set installdir  [::InstallJammer::SubstText <%InstallDir%>]
    set patternlist [split [::InstallJammer::SubstText $patternString] \;]

    foreach pattern $patternlist {
        set pattern [string trim [::InstallJammer::SubstText $pattern]]
        if {$pattern eq ""} { continue }

        if {[file pathtype $pattern] ne "relative"} {
            set relative 0
            set pattern [::InstallJammer::Normalize $pattern]
        }

        lappend patterns $pattern
    }

    if {![llength $patterns]} { return }

    if {$relative} {
        ## All of our patterns are relative, so we can do a single, quick
        ## glob to find everything relative to the <%InstallDir%>.
        set opts $args
        lappend opts -dir $installdir

        set files [eval glob -nocomplain $opts $patterns]
    } else {
        set files [list]
        foreach pattern $patterns {
            set opts $args
            if {[file pathtype $pattern] eq "relative"} {
                lappend opts -dir $installdir
            }
            eval lappend files [eval glob -nocomplain $opts [list $pattern]]
        }
    }

    return $files
}

proc ::InstallJammer::StartProgress { varName total {current 0} } {
    global conf
    set conf(ProgressCurr)    0
    set conf(ProgressLast)    0
    set conf(ProgressTotal)   $total
    set conf(ProgressVarName) $varName
}

proc ::InstallJammer::ResetProgress {} {
    global conf
    set conf(ProgressLast) 0
}

proc ::InstallJammer::UpdateProgress { args } {
    set total   $::conf(ProgressTotal)
    set varName $::conf(ProgressVarName)
    set current [lindex $args end]

    set bytes [expr {$current - $::conf(ProgressLast)}]
    set ::conf(ProgressLast) $current

    incr ::conf(ProgressCurr) $bytes

    if {$varName ne ""} {
        set $varName [expr {round( ($::conf(ProgressCurr) * 100.0) / $total )}]
    }
}

proc ::InstallJammer::ReadProperties { data arrayName } {
    upvar 1 $arrayName array

    foreach line [split [string trim $data] \n] {
        set line [string trim $line]
        if {[set x [string first : $line]] >= 0} {
            set var [string trim [string range $line 0 [expr {$x-1}]]]
            set val [string trim [string range $line [expr {$x+1}] end]]
            set array($var) $val
        }
    }
}

proc ::InstallJammer::ReadPropertyFile { file arrayName } {
    upvar 1 $arrayName array
    set fp [open $file]
    ::InstallJammer::ReadProperties [read $fp] array
    close $fp
}

proc ::InstallJammer::ShowUsageAndExit { {message ""} {title ""} } {
    global conf
    global info

    variable ::InstallJammer::CommandLineOptions

    ::InstallJammer::InitializeCommandLineOptions

    set head --
    if {$conf(windows)} { set head / }

    set usage ""

    if {$message ne ""} { append usage "$message\n\n" }
    append usage "Usage: [file tail [info nameofexecutable]] \[options ...\]"
    append usage "\n\nAvailable Options:"

    set len 0
    foreach option [array names CommandLineOptions] {
        if {$option eq "PREFIX"} { continue }

        lassign $CommandLineOptions($option) name var type x hide values desc

        if {$type eq "Boolean"} {
            set desc "$name <Yes or No>"
        } elseif {$type eq "Prefix"} {
            set desc "$name<OPTION> \[ARG\]"
        } elseif {$type eq "Switch"} {
            set desc $name
        } else {
            set desc "$name \[ARG\]"
        }

        set options($option) $desc

        if {[string length $desc] > $len} {
            set len [string length $desc]
        }
    }

    incr len 4
    set  pad [expr {$len + 3}]

    foreach option [lsort -dict [array names options]] {
        lassign $CommandLineOptions($option) name var type x hide values desc

        if {$hide} { continue }

        set desc   [::InstallJammer::SubstText $desc]
        set values [::InstallJammer::SubstText $values]

        set line "  [format %-${len}s $head$options($option)] $desc"

        append usage "\n[::InstallJammer::WrapText $line 0 $pad]"

        if {$type eq "Choice"} {
            set values  [lsort -dict $values]
            set last    [lindex $values end]
            set values  [lrange $values 0 end-1]
            set choices [string tolower "[join $values ", "] or $last"]

            set line "[string repeat " " $pad]Available values: $choices"
            append usage "\n[::InstallJammer::WrapLine $line 0 $pad]"
        }
    }

    append usage \n

    if {$title eq ""} { set title "Invalid Arguments" }
    ::InstallJammer::Message -icon error -font TkFixedFont \
        -title $title -message $usage

    ::exit [expr {$message eq "" ? 0 : 1}]
}

proc ::InstallJammer::DisplayVersionInfo {} {
    global conf
    global info

    set msg "InstallJammer Installer version $conf(version)\n\n"
    if {$info(RunningInstaller)} {
        append msg "<%VersionHelpText%>"
    } else {
        append msg "<%AppName%> <%Version%>"
    }
    set msg [sub $msg]

    ::InstallJammer::Message -default ok -title "InstallJammer" -message $msg
}

proc ::InstallJammer::ParseCommandLineArguments { argv } {
    global conf
    global info

    variable ::InstallJammer::CommandLineOptions
    variable ::InstallJammer::PassedCommandLineOptions
    variable ::InstallJammer::VirtualTextSetByCommandLine

    ::InstallJammer::ExecuteActions "Command Line Actions"

    ::InstallJammer::InitializeCommandLineOptions

    if {$conf(osx) && [string match "-psn_0_*" [lindex $argv 0]]} {
        set argv [lreplace $argv 0 0]
        set ::argv [lreplace $::argv 0 0]
        set conf(cmdlineMessages) 0
        set info(InAppBundle) 1
        set info(AppBundlePath) \
            [file join {*}[lrange [file split [info nameof]] 0 end-3]]
    }

    set i 0
    foreach arg $argv {
        if {[string tolower [string trimleft $arg -/]] eq "response-file"} {
            ::InstallAPI::ResponseFileAPI -do read -file [lindex $argv [incr i]]
            break
        }
        incr i
    }

    set len [llength $argv]
    for {set i 0} {$i < $len} {incr i} {
        set arg [lindex $argv $i]
        set opt [string tolower [string trimleft $arg -/]]

        ## The first argument of argv can be the name of our
        ## executable, so we need to check and skip it.
        if {$i == 0 && [file normalize $arg] eq [file normalize [info name]]} {
            continue
        }

        if {$opt eq "help" || $opt eq "?"} {
            ::InstallJammer::ShowUsageAndExit "" "Help"
        }

        if {$opt eq "v" || $opt eq "version"} {
            ::InstallJammer::DisplayVersionInfo
            ::exit 0
        }

        if {![info exists CommandLineOptions($opt)]} {
            set found 0

            if {[info exists CommandLineOptions(PREFIX)]} {
                foreach prefix $CommandLineOptions(PREFIX) {
                    if {[string match -nocase $prefix* $opt]} {
                        set found 1

                        set opt       [string trimleft $arg -/]
                        set xlen      [string length $prefix]
                        set prefixopt [string range $opt $xlen end]

                        set opt $prefix
                        break
                    }
                }
            }

            if {!$found} {
                ::InstallJammer::ShowUsageAndExit "invalid option '$arg'"
                return
            }
        }

        lassign $CommandLineOptions($opt) name var type debug hide value desc
        set choices [::InstallJammer::SubstText $value]

        if {$type eq "Switch"} {
            if {$value eq ""} {
                set val 1
            } else {
                set val $value
            }
        } else {
            if {[incr i] == $len} {
                ## Option without an argument.
                ::InstallJammer::ShowUsageAndExit \
                    "no argument given for option '$arg'"
            }

            set val [lindex $argv $i]

            if {$type eq "Choice"} {
                set val  [string tolower $val]
                set vals [string tolower $choices]
                if {[set x [lsearch -exact $vals $val]] < 0} {
                    ::InstallJammer::ShowUsageAndExit \
                        "invalid value given for option '$arg'"
                }

                set val [lindex $choices $x]
            } elseif {$type eq "Boolean"} {
                if {![string is boolean -strict $val]} {
                    ::InstallJammer::ShowUsageAndExit \
                        "invalid value given for option '$arg'"
                }

                if {$value ne ""} {
                    if {[string is true $val]} {
                        set val [lindex $value 0]
                    } else {
                        set val [lindex $value 1]
                    }
                }
            } elseif {$type eq "Prefix"} {
                if {![info exists prefixopt]}  {
                    ::InstallJammer::ShowUsageAndExit \
                        "no option specified for '$arg'"
                }

                set suffix $prefixopt

                if {$value ne ""} {
                    set opt     [string tolower $prefixopt]
                    set choices [string tolower $choices]
                    if {[set x [lsearch -exact $choices $opt]] < 0} {
                        ::InstallJammer::ShowUsageAndExit \
                            "invalid option '$prefixopt'"
                    }

                    set suffix [lindex $value $x]
                }

                append var $suffix
            }
        }

        set info($var) $val
        set PassedCommandLineOptions($opt) $val
        set VirtualTextSetByCommandLine($var) $val
    }

    ::InstallJammer::SetupModeVariables

    ::InstallJammer::ExecuteActions "Setup Actions"

    if {$info(ShowConsole)} {
        ::InstallJammer::InitializeGui
        if {!$conf(windows)} { SourceCachedFile console.tcl }
        console eval {wm geometry . 80x24+0+0}
        console show
    }

    if {!$info(GuiMode) && !$conf(windows)} {
        if {![catch { exec stty size } result]
            && [scan $result "%d %d" height width] == 2} {
            set conf(ConsoleWidth)  $width
            set conf(ConsoleHeight) $height
        }
    }
}

proc ::InstallJammer::SetupModeVariables {} {
    global conf
    global info

    ## If the command-line arguments have given us a mode that
    ## doesn't exist in our list of possible modes, use whatever
    ## the default mode is (Standard).
    if {[lsearch -exact $conf(modes) $info($conf(mode))] < 0} {
        set mode [lindex $conf(modes) 0]
        debug "Bad $conf(mode) \"$info($conf(mode))\": using $mode mode"
        set info($conf(mode)) $mode
    }

    set mode $info($conf(mode))
    set info(GuiMode)     [expr {$mode eq "Default" || $mode eq "Standard"}]
    set info(SilentMode)  [string equal $mode "Silent"]
    set info(DefaultMode) [string equal $mode "Default"]
    set info(ConsoleMode) [string equal $mode "Console"]
}

proc ::InstallJammer::CommonExit { {cleanupTmp 1} } {
    global conf

    catch { 
        foreach script $conf(ExitScripts) {
            uplevel #0 $script
        }
    }

    catch { ::InstallJammer::ExecuteActions "Exit Actions" }

    catch { ::InstallJammer::ExecuteActions "Exit Actions" }

    catch {
        foreach chan [file channels] {
            if {[string match "std*" $chan]} { continue }
            catch {close $chan}
        }
    }

    if {$cleanupTmp} {
        catch {
            ::InstallJammer::WriteDoneFile
            ::InstallJammer::CleanupTmpDirs
        }
    }

    if {[info exists ::debugfp]} { catch { close $::debugfp } }
}

proc ::InstallJammer::WrapText { string {width 0} {start 0} } {
    global conf

    if {$width == 0} { set width $conf(ConsoleWidth) }

    set splitstring {}
    foreach line [split $string "\n"] {
	lappend splitstring [::InstallJammer::WrapLine $line $width $start]
    }
    return [join $splitstring "\n"]
}

proc ::InstallJammer::WrapLine { line {width 0} {start 0} } {
    global conf

    if {$width == 0} { set width $conf(ConsoleWidth) }

    set slen  0
    set words [split $line " "]
    set line  [lindex $words 0]
    set lines [list]
    foreach word [lrange $words 1 end] {
	if {[string length $line] + [string length " $word"] > $width} {
	    lappend lines $line

            set slen $start
	    set line [string repeat " " $slen]$word
	} else {
            append line " $word"
        }
    }

    if {$line ne ""} { lappend lines $line }

    return [join $lines "\n"]
}

proc ::InstallJammer::DisplayConditionFailure { id } {
    set string [::InstallJammer::SubstText [$id get FailureMessage]]
    set list   [split $string |]

    set icon    ""
    set title   ""
    set message [string trim $string]

    if {[llength $list] == 2} {
        set title   [string trim [lindex $list 0]]
        set message [string trim [lindex $list 1]]
    } elseif {[llength $list] >= 3} {
        set icon    [string trim [lindex $list 0]]
        set title   [string trim [lindex $list 1]]
        set message [string trim [join [lrange $list 2 end] |]]
    }

    if {$icon eq ""} { set icon "error" }
    if {$title eq ""} { set title "Install Error" }

    if {$message ne ""} {
        ::InstallJammer::Message -icon $icon -title $title -message $message
    }

    set focus [string trim [$id get FailureFocus]]
    if {$focus ne "" && [::InstallJammer::InGuiMode]} {
        set focus [::InstallAPI::GetWidgetPath -widget $focus]
        if {[winfo exists $focus]} { focus -force $focus }
    }
}

proc ::InstallJammer::GetAllTreeNodes { tree {parent "root"} } {
    set nodes {}
    foreach node [$tree nodes $parent] {
        lappend nodes $node
        eval lappend nodes [::InstallJammer::GetAllTreeNodes $tree $node]
    }
    return $nodes
}

proc ::InstallJammer::IsValidFilename {name} {
    return [expr {[regexp {[:\*\?\"<>|]} $name] == 0}]
}

proc ::InstallJammer::IsValidPath {path} {
    global conf

    set list [file split $path]
    if {$conf(windows) && [string match {[a-zA-Z]:/} [lindex $list 0]]} {
        set list [lrange $list 1 end]
    }
    foreach name $list {
        if {![::InstallJammer::IsValidFilename $name]} { return 0 }
    }
    return 1
}

proc ::InstallJammer::InGuiMode {} {
    return [info exists ::tk_patchLevel]
}

proc ::InstallJammer::WizardExists {} {
    return [expr {[info exists ::tk_patchLevel]
                    && [info exists ::info(Wizard)]
                    && [winfo exists $::info(Wizard)]}]
}

proc ::InstallJammer::ConsoleClearLastLine { {len 0} } {
    global conf
    if {!$len} {
        if {[info exists conf(ConsoleProgressLastLen)]} {
            set len $conf(ConsoleProgressLastLen)
            if {!$len} { return }
        } else {
            return
        }
    }
    puts -nonewline [string repeat   $len]
    puts -nonewline [string repeat " " $len]
    puts -nonewline [string repeat   $len]
}

proc ::InstallJammer::ConsoleProgressBar { percent } {
    global conf

    if {![info exists conf(ConsoleProgressWidth)] || $percent == 0} {
        SafeSet conf(ConsoleProgressNewline) 0
	SafeSet conf(ConsoleProgressFormat) {[%s%s] %d%%}
	set s [string map {%s "" %d "" %% %} $conf(ConsoleProgressFormat)]
	set conf(ConsoleProgressWidth) [string length $s]
        set conf(ConsoleProgressLastLen) 0
	SafeSet conf(ConsoleProgressCompletedHash) =
	SafeSet conf(ConsoleProgressIncompleteHash) -
    }

    set len  0
    set cols $conf(ConsoleWidth)
    ::InstallJammer::ConsoleClearLastLine

    set width [expr {$cols - 2 - $conf(ConsoleProgressWidth)}]
    if {[string match "*%d*" $conf(ConsoleProgressFormat)]} { incr width -3 }
    set pct   [expr {(100 * $percent) / 100}]
    set cnt   [expr {($width * $percent) / 100}]
    set done  [expr {$width - $cnt}]
    set args  [list $conf(ConsoleProgressFormat)]
    if {$conf(ConsoleProgressCompletedHash) ne ""} {
	lappend args [string repeat $conf(ConsoleProgressCompletedHash) $cnt]
    }
    if {$conf(ConsoleProgressIncompleteHash) ne ""} {
	lappend args [string repeat $conf(ConsoleProgressIncompleteHash) $done]
    }
    if {[string match "*%d*" $conf(ConsoleProgressFormat)]} {
	lappend args $pct
    }

    set string [eval format $args]
    puts -nonewline $string
    set conf(ConsoleProgressLastLen) [string length $string]

    if {$percent == 100} {
        ::InstallJammer::ConsoleClearLastLine
        if {$conf(ConsoleProgressNewline)} { puts "" }
    }
    flush stdout
}

proc ::InstallJammer::MountSetupArchives {} {
    global conf
    global info

    set found 0
    if {[info exists info(ArchiveFileList)]} {
        foreach file $info(ArchiveFileList) {
            set file [file join $info(InstallSource) $file]
            if {[file exists $file]} {
                set found 1
                ::InstallJammer::Mount $file $conf(vfs)
            }
        }
    }
    return $found
}

proc ::InstallJammer::GetHandCursor {} {
    global conf
    if {$conf(osx)} { return "pointinghand" }
    return "hand2"
}

proc ::InstallJammer::GetCommonInstallkit { {base ""} } {
    global info
    ::InstallJammer::InstallInfoDir
    set kit [file join $info(InstallJammerRegistryDir) \
        $info(Platform) installkit$info(Ext)]
    set opts [list -noinstall -o $kit]
    if {$base ne ""} { lappend opts -w $base }
    file mkdir [file dirname $kit]

    ## FIXME:  Remove this crap when the installkit is fixed to always
    ## build from the VFS instead of just copying.
    set main [::InstallJammer::TmpDir main[pid].tcl]
    set fp [open $main w]
    puts $fp {
        if {[llength $argv]} {
            uplevel #0 [list source [lindex $argv end]]
        }
    }
    close $fp
    return  [eval ::InstallJammer::Wrap $opts [list $main]]
}

## OBJ

namespace eval ::obj::class {
    namespace export create defaults exists instances \
        subclasses superclass variables
    namespace ensemble create
}

namespace eval ::obj::object {
    namespace export class create destroy exists instances
    namespace ensemble create
}

namespace eval ::obj::create  {}
namespace eval ::obj::classes {}
namespace eval ::obj::private {}

namespace eval :: {
    namespace unknown [list ::obj::Unknown [namespace unknown]]
}

proc ::obj::Unknown {next args} {
    set obj [string trimleft [lindex $args 0] :]
    if {![info exists ::obj::instances($obj)]} {return [uplevel 1 $next $args]}

    set method  [lindex $args 1]
    set classns ::obj::classes::[lindex $::obj::instances($obj) 0]
    if {[catch {dict get [set ${classns}::methods] $method} command]} {
        return -code error "unknown or ambiguous method \"$method\": must be\
            [join [dict keys [set ${classns}::methods]] ", "]"
    }

    uplevel 1 [list $command $classns $obj {*}[lrange $args 2 end]]
}

proc ::obj::object::class {objName} {
    if {![info exists ::obj::instances($objName)]} {
        return -code error "object \"$objName\" does not exist"
    }
    return [lindex $::obj::instances($objName) 0]
}

proc ::obj::object::create {class objName args} {
    set ns ::obj::classes::${class}
    set objName [string trimleft $objName :]
    if {$objName eq "#auto" || $objName eq "new"} {
        set objName [namespace tail $class][incr ::obj::autocount]
    }
    set ::obj::instances($objName) [set ${ns}::default]

    set configure [dict get [set ${ns}::methods] configure]
    if {[llength $args]} { $configure $ns $objName {*}$args }

    if {![catch {dict get [set ${ns}::methods] __constructor__} constructor]
        && [catch {$constructor $ns $objName {*}$args} err]} {
        return -code error "error in object constructor: $::errorInfo"
    }

    return $objName
}

proc ::obj::object::destroy {args} {
    foreach obj $args {
        set obj [string trimleft $obj :]
        if {![info exists ::obj::instances($obj)]} { continue }
        set ns ::obj::classes::[lindex $::obj::instances($obj) 0]
        catch {[dict get [set ${ns}::methods] __destructor__] $ns $obj}
        unset ::obj::instances($obj)
    }
}

proc ::obj::object::exists {objName} {
    set objName [string trimleft $objName :]
    return [info exists ::obj::instances($objName)]
}

proc ::obj::object::instances {{class ""}} {
    if {$class eq ""} { return [array names ::obj::instances] }

    set list {}
    foreach obj [array names ::obj::instances] {
        if {[lindex $::obj::instances($obj) 0] eq $class} { lappend list $obj }
    }
    return $list
}

proc ::obj::class::create {className bodyScript} {
    variable ::obj::create::class $className
    variable ::obj::create::ns ::obj::classes::$className

    namespace eval ${ns} {
        namespace path ::obj::private
    }

    set ${ns}::default    [list $class]
    set ${ns}::methods    [dict create]
    set ${ns}::varcount   0
    set ${ns}::variables  [dict create __class 0]
    set ${ns}::superclass ""
    set ${ns}::subclasses [list]

    ::obj::create::superclass ::obj::Object

    namespace eval ::obj::create $bodyScript

    interp alias {} ::$className {} ::obj::object::create $className

    return $className
}

proc ::obj::class::defaults {className} {
    return [set ::obj::classes::${className}::default]
}

proc ::obj::class::exists {className} {
    return [namespace exists ::obj::classes::$className]
}

proc ::obj::class::instances {className} {
    set classes [list $className {*}[::obj::class subclasses $className]]

    set list {}
    foreach obj [array names ::obj::instances] {
        if {[lindex $::obj::instances($obj) 0] in $classes} {
            lappend list $obj
        }
    }
    return $list
}

proc ::obj::class::subclasses {className} {
    variable ::obj::classes::${className}::subclasses
    set list {}
    foreach subclass $subclasses {
        lappend list $subclass
        lappend list {*}[::obj::class subclasses $subclass]
    }
    return $list
}

proc ::obj::class::superclass {className} {
    return [set ::obj::classes::${className}::superclass]
}

proc ::obj::class::variables {className} {
    return [dict keys [set ::obj::classes::${className}::variables]]
}

proc ::obj::create::constructor {arguments body} {
    method __constructor__ $arguments $body
}

proc ::obj::create::destructor {body} {
    method __destructor__ {} $body
}

proc ::obj::create::method {methodName arguments body} {
    ::variable ns

    set proc ${ns}::Method_$methodName
    dict set ${ns}::methods $methodName $proc
    proc $proc [list __classns self {*}$arguments] "set __methodns $ns\n$body"
}

proc ::obj::create::mixin {args} {
    ::variable ns
    ::variable class

    foreach className $args {
        if {![::obj::class exists $className]} {
            return -code error "class \"$className\" does not exist"
        }
        if {$class eq $className} { return }

        set superns ::obj::classes::$className
        dict for {method command} [set ${superns}::methods] {
            dict set ${ns}::methods $method $command
        }
        lappend ${ns}::mixins $className
    }
}

proc ::obj::create::superclass {className} {
    ::variable ns
    ::variable class

    if {![::obj::class exists $className]} {
        return -code error "class \"$className\" does not exist"
    }
    if {$class eq $className} { return }

    set superns ::obj::classes::$className

    set ${ns}::default    [lreplace [set ${superns}::default] 0 0 $class]
    set ${ns}::methods    [set ${superns}::methods]
    set ${ns}::varcount   [set ${superns}::varcount]
    set ${ns}::variables  [set ${superns}::variables]
    set ${ns}::superclass $superns
    lappend ${superns}::subclasses $class
}

proc ::obj::create::variable {varName args} {
    ::variable ns
    ::variable ${ns}::variables

    if {[dict exists $variables $varName]} {
        if {[llength $args]} {
            set idx [dict get $variables $varName]
            lset ${ns}::default $idx [lindex $args 0]
        }
    } else {
        set idx [incr ${ns}::varcount]
        dict set variables $varName $idx
        lappend ${ns}::default [lindex $args 0]

        method $varName {{value ""}} [string map "@IDX@ $idx" {
            if {[llength [info level 0]] == 4} {
                lset ::obj::instances($self) @IDX@ $value
            }
            return [lindex $::obj::instances($self) @IDX@]
        }]
    }
}

proc ::obj::private::my {method args} {
    upvar 1 __classns __classns
    set proc [dict get [set ${__classns}::methods] $method]
    uplevel 1 $proc \$__classns \$self $args
}

proc ::obj::private::next {args} {
    upvar 1 __classns __classns __methodns __methodns self self
    upvar #0 ${__methodns}::superclass super

    if {$super ne ""} {
        set method [namespace tail [lindex [info level -1] 0]]
        set method [string range $method 7 end]
        if {[dict exists [set ${super}::methods] $method]} {
            set command [dict get [set ${super}::methods] $method]
            namespace eval $super [list $command $__classns $self {*}$args]
        }

        if {![info exists ::obj::instances($self)]} { return -code return }
    }
}

::obj::class create ::obj::Object {
    ::obj::create::method destroy {args} {
        ::obj::object destroy $self
    }

    ::obj::create::method class {} {
        return [my cget __class]
    }

    ::obj::create::method cget {var} {
        if {[string index $var 0] eq "-"} { set var [string range $var 1 end] }
        set idx [dict get [set ${__classns}::variables] $var]
        return [lindex $::obj::instances($self) $idx]
    }

    ::obj::create::method configure {args} {
        if {[llength $args] == 0} {
            return $::obj::instances($self)
        } elseif {[llength $args] == 1} {
            set ::obj::instances($self) [lindex $args 0]
        } else {
            variable ${__classns}::variables
            foreach {var val} $args {
                if {[string index $var 0] eq "-"} {
                    set var [string range $var 1 end]
                }
                if {[dict exists $variables $var]} {
                    set idx [dict get $variables $var]
                    lset ::obj::instances($self) $idx $val
                } else {
                    my $var $val
                }
            }
            return $val
        }
    }
}


## CLASSES

::obj::class create TreeObject {
    variable id        ""
    variable temp      0
    variable name      ""
    variable type      ""
    variable parent    ""
    variable platforms {}

    constructor {args} {
        if {[my cget temp]} { return }

        my id $self
        set parent [my cget parent]
        if {$parent ne ""} {
            if {![::obj::object exists $parent]} {
                ## If our parent doesn't exist, we don't exist.
                my destroy
                return
            }
            $parent children add $self
        }

        set alias [my alias]
        if {$alias ne ""} {
            set ::InstallJammer::aliases($alias) $self
            set ::InstallJammer::aliasmap($self) $alias
        }
    }

    destructor {
        if {[my cget temp]} { return }

        set parent [my cget parent]

        if {$parent ne "" && [::obj::object exists $parent]} {
            $parent children remove $self
        }

        foreach child [my children] {
            $child destroy
        }

        my set Alias ""
        array unset ::InstallJammer::Properties $self,*

        foreach lang [::InstallJammer::GetLanguageCodes] {
            ::msgcat::mcunset $lang $self,*
        }

        foreach array [info vars ::InstallJammer::Obj_*] {
            unset -nocomplain ${array}($self)
        }
    }

    method options {} {
        set list  [list -active [my active]]
        set class [::obj::object class $self]
        set defs  [::obj::class defaults $class]
        set vars  [::obj::class variables $class]
        foreach var $vars def $defs {
            if {$var eq "id"} { continue }
            set val [my cget $var]
            if {$val eq $def} { continue }
            lappend list -$var $val
        }
        return $list
    }

    method serialize {{forBuild 0}} {
        if {$forBuild} {
            return "[my cget __class] $self [my configure]"
        } else {
            return "[my cget __class] $self [my options]"
        }
    }

    method parent { args } {
        if {![llength $args]} { return [my cget parent] }

        set arg [lindex $args 0]
        if {$arg ne "recursive"} { return [my configure parent $arg] }

        set x    [my parent]
        set list [list]
        while {[string length $x]} {
            set list [linsert $list 0 $x]
            set x [$x parent]
        }
        return $list
    }

    method reparent { newParent } {
        set parent [my cget parent]

        ## If this is already our parent, don't do anything.
        if {$parent eq $newParent} { return }

        ## If we have an old parent, remove us from their children.
        if {$parent ne ""} { $parent children remove $self }

        ## Add ourselves to the new parent.
        my configure parent $newParent
        if {$parent ne ""} { $parent children add $self }
    }

    method Treecmd { what args } {
        upvar #0 ::InstallJammer::Obj_${what}($self) list

        if {![llength $args]} {
            if {[info exists list]} { return $list }
            return
        }

        lassign $args command obj idx
        switch -- $command {
            "add" {
                lappend list $obj
            }

            "index" {
                return [lsearch -exact $list $obj]
            }

            "move" {
                set list [linsert [lremove $list $obj] $idx $obj]
            }

            "remove" - "delete" {
                set list [lremove $list $obj]
            }

            "reorder" {
                if {[llength $obj]} { set list $obj }
            }

            "recursive" {
                return [::InstallJammer::ObjectRecursiveList $what $self]
            }

            default {
                set list $command
            }
        }
    }

    method children {args} {
        return [my Treecmd children {*}$args]
    }

    method set { args } {
        variable ::InstallJammer::Properties

        set id [my id]
        if {$id eq ""} { set id $self }

        ::InstallJammer::ParseArgs _args $args -switches {-safe -nocomplain}

        set args $_args(_ARGS_)

        if {[llength $args] == 1} { set args [lindex $args 0] }
        if {[llength $args] == 1} { return [my get [lindex $args 0]] }

        foreach {property value} $args {
            if {$_args(-safe) && [info exists Properties($id,$property)]} {
                continue
            }

            if {$property eq "Alias"} {
                if {[info exists ::InstallJammer::aliasmap($id)]} {
                    set alias $::InstallJammer::aliasmap($id)
                    unset -nocomplain ::InstallJammer::aliasmap($id)
                    unset -nocomplain ::InstallJammer::aliases($alias)
                }

                if {$value ne ""} {
                    set ::InstallJammer::aliases($value) $id
                    set ::InstallJammer::aliasmap($id) $value
                }
            }

            if {![info exists ::InstallJammer]} {
                variable ::InstallJammer::PropertyMap
                if {[info exists PropertyMap($property)]} {
                    set n $value
                    if {![string is integer -strict $n]} {
                        set n [lsearch -exact $PropertyMap($property) $value]
                        if {$n < 0} {
                            return -code error [BWidget::badOptionString value \
                                $value $PropertyMap($property)]
                        }
                    }
                    set value $n
                }
            }

            set Properties($id,$property) $value
        }

        return $Properties($id,$property)
    }

    method get { property {varName ""} } {
        variable ::InstallJammer::Properties

        set id [my id]
        if {$id eq ""} { set id $self }

        set value  ""
        set exists [info exists Properties($id,$property)]
        if {$exists} {
            variable ::InstallJammer::PropertyMap
            set value $Properties($id,$property)
            if {[info exists PropertyMap($property)]
                && [string is integer -strict $value]} {
                set value [lindex $PropertyMap($property) $value]
            }
        }

        if {$varName eq ""} { return $value }
        upvar 1 $varName var
        set var $value
        return $exists
    }

    method properties { arrayName args } {
        upvar 1 $arrayName array
        variable ::InstallJammer::Properties

        set id [my id]
        if {$id eq ""} { set id $self }

        ::InstallJammer::ParseArgs _args $args -options {-prefix "" -subst 0}

        set slen 0
        if {[info exists _args(-subst)]} {
            set subst $_args(-subst)
            set slen  [llength $subst]
        }

        set props $_args(_ARGS_)
        if {![llength $props]} {
            foreach varName [array names Properties $id,*] {
                lappend props [string map [list $id, ""] $varName]
            }
        }

        set vars {}
        foreach prop $props {
            if {![info exists Properties($id,$prop)]} { continue }

            set val $Properties($id,$prop)
            if {$slen && ($subst eq "1" || $prop in $subst)} {
                set val [sub $val]
            }
            if {[info exists ::InstallJammer::PropertyMap($prop)]
                && [string is integer -strict $val]} {
                set val [lindex $::InstallJammer::PropertyMap($prop) $val]
            }
            set prop $_args(-prefix)$prop
            lappend vars $prop
            set array($prop) $val
        }

        return $vars
    }

    method getText { field args } {
        ::InstallJammer::GetText [my id] $field {*}$args
    }

    method setText { languages args } {
        if {[llength $args]} {
            ::InstallJammer::SetVirtualText $languages [my id] {*}$args
        }
    }

    method is { args } {
        return [expr {[my type] in $args}]
    }

    method index {} {
        set parent [my parent]
        if {$parent ne ""} { return [$parent children index $self] }
    }

    method component {} {
        return "ClassObject"
    }
    
    method alias {args} {
        if {[llength $args]} { my set Alias [lindex $args 0] }
        return [my get Alias]
    }
    
    method comment {args} {
        if {[llength $args]} { my set Comment [lindex $args 0] }
        return [my get Comment]
    }

    method active {args} {
        if {[llength $args]} { my set Active [lindex $args 0] }
        return [expr {[string is true [my get Active]] ? "Yes" : "No"}]
    }
}

::obj::class create InstallType {
    superclass TreeObject

    variable setup ""

    constructor {args} {
        next {*}$args
        my type installtype
    }

    method component {} {
    
    }

    method widget {args} {
    
    }
}

::obj::class create File {
    superclass TreeObject

    variable type               "file"
    variable size               0
    variable mtime              0
    variable version            ""
    variable srcfile            ""
    variable location           ""
    variable directory          ""
    variable linktarget         ""
    variable filemethod         ""
    variable attributes         ""
    variable permissions        ""
    variable targetfilename     ""
    variable compressionmethod  ""

    constructor {args} {
        next {*}$args
        if {[info exists ::InstallJammer]} {
            set ::InstallJammer::FileObjects([my parent],[my name]) [my id]
        }
    }

    method component {} {
    
    }

    method setup {} {
        return "Install"
    }

    method srcfile {} {
        set srcfile [my cget srcfile]
        if {[file pathtype $srcfile] eq "absolute"} { return $srcfile }
	return [file join $::conf(vfs) $srcfile]
    }

    method checkFileMethod { dest } {
        set mtime   [my cget mtime]
        set parent  [my cget parent]
        set method  [my cget filemethod]
        set version [my cget version]

        if {$method eq ""} { set method [$parent filemethod] }

        set doInstall 1

        if {$method ne "Always overwrite"} { set exists [file exists $dest] }

        switch -- $method {
            "Update files with more recent dates" {
                ## We only want to overwrite if the file we have is newer
                ## than the one already installed.  If the one we have is
                ## older, skip it.
                if {$exists && [file mtime $dest] >= $mtime} { set doInstall 0 }
            }

            "Update files with a newer version" {
                ## We want to overwrite the file if we have a newer version
                ## than the one stored.  If there isn't one stored, we'll go
                ## ahead and store ours.
                global versions
                if {$exists && [info exists versions($dest)]} {
                    set c [vercmp $version $versions($dest)]
                    if {$c == 0 || $c == -1} { set doInstall 0 }
                }
            }

            "Always overwrite files" {
                ## We want to always overwrite the file.
                ## This is the default action, so we do nothing.
            }

            "Never overwrite files" {
                ## We don't want to overwrite.  If the file exists, skip it.
                if {$exists} { set doInstall 0 }
            }

            "Prompt user" {
                if {$exists} {
                    set txt "<%FileOverwriteText%>"
                    set msg [::InstallJammer::SubstText $txt]
                    set ans [::InstallJammer::MessageBox -type yesno \
                        -name FileOverwrite -title "File Exists" -message $msg]
                    set doInstall [expr {$ans eq "yes"}]
                }
            }
        }

        return $doInstall
    }

    method destdir {} {
        return [::InstallJammer::SubstText [destdirname]]
    }

    method destdirname {} {
        return [my directory]
    }

    method destfile {} {
	return [file join [my destdir] [my destfilename]]
    }

    method destfilename {} {
	if {[my targetfilename] ne ""} { return [sub [my targetfilename]] }
        return [file tail [my name]]
    }

    method srcfilename {} {
    	return [file tail [my name]]
    }

    method createdir {} {
	::InstallJammer::CreateDir [my destdir]
    }

    method install {} {
	if {![::InstallJammer::PauseCheck]} { return 0 }
        my install[my type]
        return 1
    }

    method installdir {} {
        my createdir

        if {!$::conf(windows)} {
            set dest        [my destdir]
            set permissions [my permissions]
            if {$permissions eq ""} {
                set permissions $::info(DefaultDirectoryPermission)
            }

            if {[info commands output] eq "output"} {
                output [list :DIR $dest $permissions]
            }

            ::InstallJammer::SetPermissions $dest 00777
        }
    }

    method installlink {} {
        set dest    [my destfile]
        set link    [my linktarget]
        set version [sub [my version]]

        if {![my checkFileMethod $dest]} { return }

        my createdir

        if {[file exists $dest] && [catch { file delete -force $dest } error]} {
            return -code error $error
        }

        if {[catch { exec ln -s $link $dest } error]} {
            return -code error $error
        }

        if {$version eq ""} { set version $::info(InstallVersion) }

        ::InstallJammer::LogFile $dest
        ::InstallJammer::SetVersionInfo $dest $version

        return $dest
    }

    method installfile { {dest ""} {createDir 1} {checkMethod 1} {logFile 1} } {
	global conf
	global info

        set size        [my size]
        set mtime       [my mtime]
        set version     [sub [my version]]
        set permissions [my permissions]

        if {$createDir} {
            my createdir
        }

	set src [my srcfile]
        if {![file exists $src] && [info exists info(ArchiveFileList)]} {
            while {![file exists $src]} {
                ::InstallJammer::PauseInstall
                output [list :DISC [[parent] name]]
                ::InstallJammer::MountSetupArchives
            }
        }

        if {$dest eq ""} { set dest [my destfile] }
        if {$version eq ""} { set version $::info(InstallVersion) }

        set doInstall 1
        if {$checkMethod} { set doInstall [my checkFileMethod $dest] }

	set info(FileSize) [my size]
	if {!$doInstall} {
            set progress ::InstallJammer::IncrProgress
            if {[::InstallJammer::CommandExists $progress]} { $progress $size }
	    return $dest
	}

        if {$permissions eq ""} {
            if {$conf(windows)} {
                set permissions 0666
            } else {
                set permissions $info(DefaultFilePermission)
            }
        }

	if {!$size} {
            ## Empty file.
	    if {[catch { open $dest w $permissions } err]} {
		return -code error $err
	    }
	    close $err
	} else {
	    ::InstallJammer::unpack $src $dest $permissions
	}

        if {$info(InstallStopped)} { return }

        ## Set the modified time to the one we have stored.
        if {$mtime} {
            file mtime $dest $mtime
        }

        if {$logFile} {
            ::InstallJammer::LogFile $dest
            ::InstallJammer::SetVersionInfo $dest $version
        }

        if {$conf(windows)} {
            ::InstallJammer::SetPermissions $dest [my attributes]
        }

	return $dest
    }

    method group {} {
        return [lindex [my parent recursive] 1]
    }

    method isfile {} {
        return [my is file link]
    }

    method object {} {
        return ::FileObject
    }

    method filemethod { {newMethod ""} } {
        if {$newMethod ne ""} {
            if {![info exists ::InstallJammer]} {
                variable ::InstallJammer::PropertyMap
                set n [lsearch -exact $PropertyMap(FileUpdateMethod) $newMethod]
                if {$n < 0} {
                    return -code error [BWidget::badOptionString method \
                        $newMethod $PropertyMap(FileUpdateMethod)]
                }
                set newMethod $n
            }
            my configure filemethod $newMethod
        }

        set filemethod [my cget filemethod]
        if {[string is integer -strict $filemethod]} {
            return [lindex $::InstallJammer::PropertyMap(FileUpdateMethod) \
                $filemethod]
        }

        return $filemethod
    }
    
    method data {args} {
        if {[llength $args]} { my set Data [lindex $args 0] }
        return [my get Data]
    }
    
    method filesavemethod {args} {
        if {[llength $args]} { my set FileSaveMethod [lindex $args 0] }
        return [my get FileSaveMethod]
    }
}

::obj::class create InstallComponent {
    superclass TreeObject

    variable setup       ""
    variable title       ""
    variable operator    "AND"
    variable component   ""

    constructor { args } { 
        next {*}$args

        set name [my cget name]
        if {$name eq ""} { my configure name [string tolower $self] }

        if {[my cget temp]} { return }

        if {![info exists ::InstallJammer] && [my get Include incl]} {
            if {$::info(Testing) && $incl eq "Include only when not testing"
                || !$::info(Testing) && $incl eq "Include only when testing"} {
                my destroy
                return
            }
        }
    }

    destructor {
        next

        if {[my cget temp]} { return }

        foreach condition [my conditions] {
            catch { $condition destroy }
        }
    }

    method ispane {} {
        return 0
    }

    method initialize {} {
        [my object] initialize $self
    }

    method conditions {args} {
        return [my Treecmd conditions {*}$args]
    }

    method checkConditions { {when ""} } {
        set title      [my cget title]
        set operator   [my cget operator]
        set conditions [my conditions]

        if {[my ispane]} {
            global info
            set info(CurrentPane) $self
        }

        set return 1
        if {[llength $conditions]} {
            set conditionlist [list]

            foreach cid $conditions {
                if {![::InstallJammer::ObjExists $cid]} { continue }

                if {$when eq "" || [$cid get CheckCondition] eq $when} {
                    lappend conditionlist $cid
                }
            }

            if {[llength $conditionlist]} {
                if {[debugging ison]} {
                    set msg "Checking conditions for"
                    if {$title eq ""} {
                        append msg " [my id]"
                    } else {
                        append msg " $title ([my id])"
                    }
                    if {$when ne ""} { append msg " [string tolower $when]" }
                    debug $msg $self
                }

                foreach cid $conditionlist {
                    if {![$cid active]} {
                        if {[debugging ison]} {
                            set msg "Skipping condition"
                            if {[$cid title] eq ""} {
                                append msg " $cid"
                            } else {
                                append msg " [$cid title] ($cid)"
                            }
                            append msg ". Condition is inactive."
                            debug $msg $cid
                        }
                        continue
                    }

                    if {[debugging ison]} {
                        set msg "Checking condition"
                        if {[$cid title] eq ""} {
                            append msg " $cid"
                        } else {
                            append msg " [$cid title] ($cid)"
                        }
                        debug $msg $cid
                    }
                    set result [$cid check 0]

                    if {!$result} {
                        debug "Condition failed."
                        set return 0
                        lappend failures $cid
                        if {$operator eq "AND"} { break }
                    } else {
                        debug "Condition passed."
                        if {$operator eq "OR"} {
                            set return 1
                            break
                        }
                    }
                }
            }
        }

        if {!$return} {
            ::InstallJammer::DisplayConditionFailure [lindex $failures 0]
        }

        return $return
    }

    method widget {command widget args} {
        upvar #0 ::InstallJammer::Obj_widgets($self) d

        switch -- $command {
            "get" {
                if {[info exists d] && [dict exists $d $widget widget]} {
                    return [dict get $d $widget widget]
                }
            }

            "set" {
                foreach {opt val} $args {
                    set opt [string range $opt 1 end]
                    dict set d $widget $opt $val
                }
            }
            
            "type" {
                if {[info exists d] && [dict exists $d $widget type]} {
                    return [dict get $d $widget type]
                }
                return text
            }

            default {
                return -code error "invalid option \"$command\""
            }
        }
    }

    method widgets {} {
        upvar #0 ::InstallJammer::Obj_widgets($self) d
        if {[info exists d]} { return [dict keys $d] }
    }
}

::obj::class create Pane {
    superclass InstallComponent

    variable window     ""
    variable created    0

    constructor {args} {
        next {*}$args

        if {![info exists ::InstallJammer]} {
            global conf
            global info

            set parent [my parent]

            set install $parent
            set wizard  $info(Wizard)

            set node $parent
            if {[$parent is installtype]} { set node root }

            if {$install eq "Common"} {
                ::InstallJammer::CreateWindow $wizard $self
            } elseif {$install eq $info($conf(mode))} {
                set create [list ::InstallJammer::CreateWindow $wizard $self]

                my get WizardOptions wo
                $wizard insert step end $node $self \
                    -createcommand $create {*}$wo

                if {[my is window]} {
                    $wizard itemconfigure $self -appendorder 0
                }
            }
        }
    }

    method object {} {
        return $::InstallJammer::panes([my component])
    }

    method type {} {
        return pane
    }

    method ispane {} {
        return 1
    }
}

::obj::class create ActionGroup {
    superclass InstallComponent

    method object {} {
        return ActionGroupObject
    }

    method type {} {
        return actiongroup
    }
}

::obj::class create Action {
    superclass InstallComponent

    variable window ""

    method object {} {
        return $::InstallJammer::actions([my component])
    }

    method type {} {
        return action
    }

    method execute {} {
        global info

        set component [my cget component]

        if {[debugging ison]} {
            set msg "Executing action"
            if {[my title] eq ""} {
                append msg " [my id]"
            } else {
                append msg " [my title] ([my id])"
            }
            debug $msg $self
        }

        set info(CurrentAction) $self
        ::InstallJammer::CurrentObject push $self

        ## Remember our current directory.
        if {[file exists .]} { set pwd [pwd] }

        set time [clock seconds]
        set err [catch {::InstallJammer::actions::$component $self} res]
        if {[debugging ison]} {
            set time [expr {[clock seconds] - $time}]
            set time [clock format $time -format "%Mm%Ss" -gmt 1]
            if {[my title] eq ""} {
                set msg "[my id]"
            } else {
                set msg "[my title] ([my id])"
            }
            append msg " action completed in $time"
            debug $msg
        }

        ## Actions can sometimes change to a directory.  We want
        ## to make sure to change back if the action didn't do
        ## that itself.
        if {[info exists pwd] && [file exists .]
            && [file exists $pwd] && $pwd ne [pwd]} { cd $pwd }

        ::InstallJammer::CurrentObject pop

        if {$err && [string is false [my get IgnoreErrors]]} {
            set msg "Error in action $component\n\n$::errorInfo"
            return -code error $msg
        }

        return $res
    }
}

::obj::class create FileGroup {
    superclass InstallComponent

    constructor { args } {
        next {*}$args
        my setup "Install"
    }

    method type {} {
        return filegroup
    }

    method install {} {
        global conf

        set dir [my directory]

        ::InstallJammer::CreateDir $dir

        if {$conf(windows)} {
            ::InstallJammer::SetPermissions $dir [my get Attributes]
        } else {
            ::InstallJammer::SetPermissions $dir [my get Permissions]
        }
    }

    method destdirname {} {
        return [my get Destination]
    }

    method directory {} {
        return [sub [my destdirname]]
    }

    method version {} {
        return [my get Version]
    }

    method filemethod {} {
        return [my get FileUpdateMethod]
    }

    method object {} {
        return ::FileGroupObject
    }

    method compressionmethod {} {
        return [my get CompressionMethod]
    }

    method filesavemethod {args} {
        if {[llength $args]} { my set FileSaveMethod [lindex $args 0] }
        return [my get FileSaveMethod]
    }
}

::obj::class create Component {
    superclass InstallComponent

    constructor { args } {
        next {*}$args
        my setup "Install"
    }

    method object {} {
        return ::ComponentObject
    }

    method type {} {
        return "component"
    }
}

::obj::class create SetupType {
    superclass InstallComponent

    constructor { args } {
        next {*}$args
        my setup "Install"
    }

    method object {} {
        return ::SetupTypeObject
    }

    method type {} {
        return "setuptype"
    }
}

::obj::class create Condition {
    superclass InstallComponent

    constructor { args } {
        next {*}$args

        set parent [my cget parent]
        if {$parent ne ""} {
            $parent conditions add $self
            $parent children remove $self
        }
    }

    destructor {
        next
    }

    method object {} {
        return $::InstallJammer::conditions([my component])
    }

    method type {} {
        return "condition"
    }

    method check { {showError 1} } {
        global info

        set component [my component]

        set info(CurrentCondition) $self
        ::InstallJammer::CurrentObject push $self

        set res [string is true [::InstallJammer::conditions::$component $self]]
        if {!$res && $showError} {
            ::InstallJammer::DisplayConditionFailure $self
        }

        ::InstallJammer::CurrentObject pop

        return $res
    }
}
