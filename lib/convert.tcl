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

proc ConvertTheme {} {
    global conf
    global info

    ::InstallJammer::LoadThemeConfig theme

    ## If the version is the same or higher, we don't need to do anything.
    if {$info(ThemeVersion) >= $theme(Version)} { return 0 }

    set msg    "The theme for this install has been updated.\n"
    append msg "Would you like to restore the install theme to take "
    append msg "advantage of new features?"
    set ans [tk_messageBox -type yesno -title "Theme Updated" -message $msg]

    if {$ans eq "no"} { return 0 }

    set info(ThemeVersion) $theme(Version)
    RestoreTheme -noload

    return 1
}

proc ConvertProject {} {
    global conf
    global info

    variable ::InstallJammer::Properties

    set modified 0

    if {[package vcompare $info(ProjectVersion) 1.1.0.1] < 0} {
        if {$conf(cmdline)} { return 1 }

        ## 1.1b1
        ##
        ## Added the new command-line options, so we need to setup
        ## the default options for a 1.0 project.

        ## Walk through all of the conditions on our actions and
        ## set their CheckCondition property.  Previously, conditions
        ## on actions were only checked before the action was executed.
        ##
        ## We also want to check panes to see if we need to add our new
        ## Populate actions.  Previously, these panes were auto-populated
        ## by code in the pane itself.  That code has been broken out into
        ## actions instead.

        Status "Converting project to version 1.1b1..."
        update

        incr modified

        foreach id [itcl::find objects -class InstallComponent] {
            if {[$id is action]} {
                foreach cid [$id conditions] {
                    $cid set CheckCondition "Before Action is Executed"
                }
            } elseif {$info(Theme) eq "Modern_Wizard" && [$id ispane]} {
                ## Add the new Populate Components and Populate Setup Types
                ## actions to the correct panes.
                if {[$id component] eq "SetupType"} {
                    ::InstallJammer::AddAction [$id setup] PopulateSetupTypes \
                        -parent $id
                } elseif {[$id component] eq "ChooseComponents"} {
                    ::InstallJammer::AddAction [$id setup] PopulateComponents \
                        -parent $id
                }
            }
        }

        ## We now save files without their full paths, so we want to
        ## alter the names of each file that is not a direct child
        ## of a filegroup.

        foreach file [itcl::find objects -class File] {
            if {![[$file parent] is filegroup]} {
                $file name [file tail [$file name]]
            }
        }

        ## Add the new default command-line options plus the old ones
        ## to keep compatibility.

        ::InstallJammer::AddDefaultCommandLineOptions

        array set ::InstallJammer::InstallCommandLineOptions {
            D { {} Prefix No No {}
                "set the value of an option in the installer"
            }

            S { InstallMode Switch No No "Silent"
                "run the installer in silent mode"
            }

            T { Testing Switch Yes No {}
                "run installer without installing any files"
            }

            Y { InstallMode Switch No No "Default"
                "accept all defaults and run the installer"
            }
        }

        array set ::InstallJammer::UninstallCommandLineOptions {
            S { UninstallMode Switch No No "Silent"
                "run the uninstaller in silent mode"
            }

            Y { UninstallMode Switch No No "Default"
                "accept all defaults and run the uninstaller"
            }
        }

        ## 1.1 added Console installs.  Setup a basic Console install
        ## when converting the project.

        ::NewInstall::AddConsoleInstall

        set info(ProjectVersion) "1.1.0.2"
    }

    if {[package vcompare $info(ProjectVersion) 1.1.0.3] < 0} {
        if {$conf(cmdline)} { return 1 }

        ## 1.1b3

        Status "Converting project to version 1.1b3..."
        update

        ## Walk through and find any Modify Widget actions on
        ## a License pane.  We need to check for their broken
        ## conditions and fix them.
        set str1 {[<%CurrentPane%> get UserMustAcceptLicense]}
        set str2 {<%Property UserMustAcceptLicense%>}
        set str3 {<%Property <%CurrentPane%> UserMustAcceptLicense%>}
        foreach id [itcl::find objects -class InstallComponent] {
            if {$info(Theme) eq "Modern_Wizard" && [$id is action]
                && [$id component] eq "ModifyWidget"
                && [[$id parent] ispane]
                && [[$id parent] component] eq "License"} {

                foreach cid [$id conditions] {
                    if {[$cid component] eq "StringIsCondition"} {
                        set str [$cid get String]
                        if {$str eq $str1 || $str eq $str2} {
                            $cid set String $str3
                            incr modified
                        }
                    }
                }
            }
        }

        set info(ProjectVersion) "1.1.0.3"
    }

    if {[package vcompare $info(ProjectVersion) 1.2.0.3] < 0} {
        ## The VFS format changed for the installkits in the final
        ## 1.2 release.  We need to require a full rebuild.

        incr modified

        set conf(fullBuildRequired) 1

        set info(ProjectVersion) "1.2.0.3"
    }

    if {[package vcompare $info(ProjectVersion) 1.2.5.1] < 0} {
        ## 1.2.5 added the ability to specify the Size of a file
        ## group, but InstallJammer already saves this attribute.
        ## The one saved is auto-generated, and the user never
        ## really sees it, so we want to get rid of it because they
        ## can now actually set it.
        ##
        ## Note that this doesn't actually require any action from
        ## the user, so we don't modify the state of the project.

        foreach id [itcl::find objects -class FileGroup] {
            unset -nocomplain Properties($id,Size)
        }

        set info(ProjectVersion) "1.2.5.1"
    }

    if {[vercmp $info(ProjectVersion) 1.3.0.1] < 0} {
        ## 1.3.0

        Status "Converting project to version 1.3.0..."
        update

        incr modified

        ## Remove the IncludeTWAPI property from Windows.
        unset -nocomplain Properties(Windows,IncludeTWAPI)

        ## Look for orphaned properties and remove them.
        foreach var [array names Properties] {
            lassign [split $var ,] obj prop
            if {[::InstallJammer::IsID $obj]
                && ![::InstallJammer::ObjExists $obj]} {
                unset Properties($var)
            }
        }

        ## Convert to the new File Save Method.
        if {[info exists info(SaveOnlyToplevelDirs)]} {
            if {$info(SaveOnlyToplevelDirs)} {
                set info(FileSaveMethod) "Do not save files and directories"
            } else {
                set info(FileSaveMethod) "Save all files and directories"
            }
            unset info(SaveOnlyToplevelDirs)
        }

        ## The Save Files property was removed for the new File Save Method
        ## property.  Find any Save Files properties and convert them to the
        ## new File Save Method and then remove the Save Files property from
        ## the object.
        foreach var [array names Properties *,SaveFiles] {
            lassign [split $var ,] id prop
            ## If Save Files was true, we set to the new File Save Method
            ## to save all files.  If Save Files was false, we set the new
            ## File Save Method to not save anything.
            if {$Properties($var)} {
                set method "Save all files and directories"
            } else {
                set method "Do not save files and directories"
            }
            unset Properties($var)
            set Properties($id,FileSaveMethod) $method
        }


        ## These properties were originally designated to not be substituted
        ## for virtual text, but now all properties really need to be.
        ## We need to look for these properties and set their subst to 1.
        set props [list DestinationLabel DescriptionLabel AcceptRadiobutton \
            DeclineRadiobutton ProgramFolderLabel FolderListLabel \
            AllUsersCheckbutton UserNameLabel CompanyLabel ComponentLabel \
            AcceptCheck NameLabel CompanyLabel]
        foreach var [array names Properties *,subst] {
            lassign [split $var ,] comp prop subst
            if {$prop in $props} { set Properties($var) 1 }
        }

        ## Convert messages from the message catalogs into the properties
        ## for the new message handling.  Language-specific text is no longer
        ## saved for objects.
        set langs [::InstallJammer::GetLanguageCodes]
        foreach lang $langs {
            upvar #0 ::msgcat::Msgs_$lang msgs

            unset -nocomplain msgs(GREGORIAN_CHANGE_DATE)

            foreach var [array names msgs] {
                set list [split $var ,]
                if {[llength $list] < 2} { continue }

                set text $msgs($var)
                lassign $list comp prop

                if {[::InstallJammer::IsID $comp]} {
                    ## Remove orphaned text.
                    if {![::InstallJammer::ObjExists $comp]} {
                        unset msgs($var)
                        continue
                    }

                    ## Look to see if we can convert this property to a
                    ## single string.  If the property has the same text
                    ## for all the languages it exists for, we can use
                    ## that text instead of saving a new virtual text
                    ## value in the message catalog.
                    set convert 1
                    foreach l $langs {
                        if {[::msgcat::mcexists $var $l]
                            && $text ne [::msgcat::mcget $l $var]} {
                            set convert 0
                            break
                        }
                    }

                    if {$convert} {
                        unset msgs($var)
                        $comp set $prop $text
                    } else {
                        $comp set $var "<%$var%>"
                    }
                }
            }
        }
        
        set info(ProjectVersion) "1.3.0.1"
    }

    if {[vercmp $info(ProjectVersion) 1.3.0.2] < 0} {
        Status "Converting project to version 1.3.0.2..."
        update

        incr modified

        if {$info(ExtractSolidArchivesOnStartup)} {
            set info(ExtractSolidArchives) "On startup"
        } else {
            set info(ExtractSolidArchives) "Before installation"
        }
        unset info(ExtractSolidArchivesOnStartup)

        set info(ProjectVersion) "1.3.0.2"
    }

    rename ::Condition ""
    rename ::_conditionClass ::Condition

    rename ::InstallComponent ""
    rename ::_installComponentClass ::InstallComponent

    if {$modified && !$conf(cmdline)} {
        ::InstallJammer::BackupProjectFile "<%Project%>-<%ProjectVersion%>.mpi"
    }

    return $modified
}

if {[info commands ::_installComponentClass] eq ""} {
    rename ::InstallComponent ::_installComponentClass
    proc ::InstallComponent {id args} {

        set type [dict get $args -type]
        dict unset args -type
        dict unset args -command
        if {[dict exists $args -conditions]} { dict unset args -conditions }

        switch -- $type {
            "action" {
                Action $id {*}$args
            }

            "actiongroup" {
                ActionGroup $id {*}$args
            }

            "pane" - "window" {
                Pane $id {*}$args
            }
        }
    }
}

if {[info commands ::_conditionClass] eq ""} {
    rename ::Condition ::_conditionClass
    proc ::Condition {id args} {
        dict unset args -TreeObject::id
        if {![dict exists $args -setup]} { dict set args -setup "Install" }
        ::_conditionClass $id {*}$args
    }
}

if {[info commands ::_fileClass] eq ""} {
    rename ::File ::_fileClass
    proc ::File {id args} {
        if {[dict exists $args -savefiles]} {
            if {[string is true [dict get $args -savefiles]]} {
                set method "Save all files and directories"
            } else {
                set method "Do not save files and directories"
            }
            set tail [namespace tail $id]
            set ::InstallJammer::Properties($tail,FileSaveMethod) $method
            dict unset args -savefiles
        }

        lappend ::filesLoaded [list $id {*}$args]
    }
}
