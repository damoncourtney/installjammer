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

::obj::class create ::InstallJammer::ComponentDetails {
    variable order         0
    variable title         ""
    variable parent        ""
    variable name          ""
    variable images        [list]
    variable properties    [list]
    variable textfields    [list]
    variable standardprops [list]

    variable help          [dict create]
    variable conditions    [dict create]
    variable propertyopts  [dict create]

    constructor { args } {
        my standard ID         readonly   "ID"
        my standard Active     boolean    "Active"  Yes
        my standard Alias      shorttext  "Alias"
        my standard Comment    text       "Comment"
        my standard Data       text       "Data"
    }

    method initialize { id args } {
        variable ::InstallJammer::ComponentObjectMap

        set textfields   [my textfields]
        set propertyopts [my propertyopts]

        set ComponentObjectMap($id) $self

        foreach {prop val} $args {
            $id set -safe $prop $val
        }

        foreach prop [my properties] {
            $id set -safe $prop [dict get $propertyopts $prop,value]
        }

        foreach field [lsort $textfields] {
            $id set -safe $field [gettext $id $field -default 1]
            $id set -safe $field,subst [dict get $propertyopts $field,subst]
        }
    }

    method get { prop field varName } {
        upvar 1 $varName var

        set propertyopts [my propertyopts]

        if {![dict exists $propertyopts $prop,$field]} { return 0 }
        set var [dict get $propertyopts $prop,$field]
        return 1
    }

    method standard { prop args } {
        set standardprops [my standardprops]

        if {$prop ni $standardprops} {
            my standardprops [lappend standardprops $prop]
        }
        my property $prop {*}$args
    }

    method property { prop type pretty {value ""} {choices ""} } {
        set properties   [my cget properties]
        set propertyopts [my cget propertyopts]

        if {$type eq "image"} { my image $prop }

        if {$prop ni $properties} {
            my configure properties [lappend properties $prop]
        }
        dict set propertyopts $prop,type     $type
        dict set propertyopts $prop,pretty   $pretty
        dict set propertyopts $prop,value    $value
        dict set propertyopts $prop,choices  $choices
        dict set propertyopts $prop,help     ""
        my propertyopts $propertyopts
    }

    method text { field pretty subst } {
        set textfields   [my textfields]
        set propertyopts [my propertyopts]

        my textfields [lappend textfields $field]

        dict set propertyopts $field,subst  $subst
        dict set propertyopts $field,pretty $pretty
        my propertyopts $propertyopts
    }

    method addproperties { prop id args } {
        set propertyopts [my cget propertyopts]

        array set _args {
            -array        ::InstallJammer::active
            -standard     1
            -advanced     1
            -parentnode   ""
            -properties   {}
            -standardnode standard
            -advancednode advanced
        }
        array set _args $args

        if {[llength $_args(-properties)]} {
            foreach property $_args(-properties) {
                set var  $_args(-array)($property)
                AddProperty $prop end $_args(-parentnode) $id $property $var \
                    -data    $property \
                    -help    [dict get $propertyopts $property,help]   \
                    -type    [dict get $propertyopts $property,type]   \
                    -pretty  [dict get $propertyopts $property,pretty] \
                    -choices [dict get $propertyopts $property,choices]
            }
            return
        }

        set standard [my standardproperties]
        if {![string is boolean -strict $_args(-standard)]} {
            set standard $_args(-standard)
            set _args(-standard) 1
        }

        if {$_args(-standard)} {
            foreach property $standard {
                set var  $_args(-array)($property)
                AddProperty $prop end $_args(-standardnode) $id $property $var \
                    -data    $property \
                    -help    [dict get $propertyopts $property,help]   \
                    -type    [dict get $propertyopts $property,type]   \
                    -pretty  [dict get $propertyopts $property,pretty] \
                    -choices [dict get $propertyopts $property,choices]
            }
        }

        set advanced [my properties 0]
        if {![string is boolean -strict $_args(-advanced)]} {
            set advanced $_args(-advanced)
            set _args(-advanced) 1
        }

        if {$_args(-advanced)} {
            if {![llength $advanced]} { return }

            foreach property $advanced {
                set var  $_args(-array)($property)
                AddProperty $prop end $_args(-advancednode) $id $property $var \
                    -data    $property \
                    -help    [dict get $propertyopts $property,help]   \
                    -type    [dict get $propertyopts $property,type]   \
                    -pretty  [dict get $propertyopts $property,pretty] \
                    -choices [dict get $propertyopts $property,choices]
            }
        }
    }

    method addtextfields { prop node id {arrayName ::InstallJammer::active} } {
        set textfields   [my cget textfields]
        set propertyopts [my cget propertyopts]

        set check $prop.editTextFieldSubst
        if {![winfo exists $check]} {
            CHECKBUTTON $check -command Modified
            DynamicHelp::add $check \
                -text "Do virtual text substitution for this field"
        }

        foreach field [lsort $textfields] {
            set var    ${arrayName}($field)
            set subst  ${arrayName}($field,subst)
            set pretty [dict get $propertyopts $field,pretty]

            set start  ::InstallJammer::EditTextFieldNode
            set end    ::InstallJammer::FinishEditTextFieldNode
            set etitle "Edit $pretty"
            $prop insert end $node #auto -text $pretty -variable $var \
                -browsebutton 1 -browseargs [list -style Toolbutton] \
                -browsecommand [list EditTextField $id $field $etitle $var] \
                -editstartcommand  [list $start $prop $id $field $var $subst] \
                -editfinishcommand [list $end $prop $id $field $var $subst]
        }
    }

    method image { image } {
        set images [my images]
        my images [lappend images $image]
    }

    method help { prop {text ""} } {
        set propertyopts [my cget propertyopts]
        if {$text ne ""} {
            dict set propertyopts $prop,help $text
            my propertyopts $propertyopts
        }
        return [dict get $propertyopts $prop,help]
    }

    method condition { which cond arguments } {
        set conditions [my cget conditions]
        dict lappend conditions $which [list $cond $arguments]
        my configure conditions $conditions
    }

    method properties { {includeStandard 1} } {
        set properties    [my cget properties]
        set propertyopts  [my cget propertyopts]
        set standardprops [my cget standardprops]

        if {$includeStandard} {
            return [lsort $properties]
        } else {
            set props [list]
            foreach prop [lremove $properties {*}$standardprops] {
                if {[dict get $propertyopts $prop,type] ne "hidden"} {
                    lappend props $prop
                }
            }
            return [lsort $props]
        }
    }

    method type { property } {
        set propertyopts [my propertyopts]
        return [dict get $propertyopts $property,type]
    }

    method default { property } {
        set propertyopts [my propertyopts]
        return [dict get $propertyopts $property,value]
    }

    method pretty { property } {
        set propertyopts [my propertyopts]
        return [dict get $propertyopts $property,pretty]
    }

    method choices { property } {
        set propertyopts [my propertyopts]
        return [dict get $propertyopts $property,choices]
    }

    method standardproperties {} {
        set standardprops [my standardprops]

        set list [list ID]
        if {"Component" in $standardprops} { lappend list "Component" }
        return [concat $list [lsort [lremove $standardprops {*}$list]]]
    }

    method conditions { which } {
        set conditions [my cget conditions]
        if {[dict exists $conditions $which]} {
            return [dict get $conditions $which]
        }
    }

    method component {} {
        return "ClassObject"
    }
}

::obj::class create ::InstallJammer::Action {
    superclass ::InstallJammer::ComponentDetails

    variable group    ""
    variable includes [list]
    variable requires [list]

    constructor { args } {
        next {*}$args

        set name  [my cget name]
        set group [my cget group]

        set ::InstallJammer::components($name) [namespace tail $self]

        set ::InstallJammer::actions($name) [namespace tail $self]
        lappend ::InstallJammer::actiongroups($group) [namespace tail $self]

        my standard Component      readonly   "Component"
        my standard Conditions     conditions "Conditions"
        my standard Include        choice     "Include" \
            "Always include" $::InstallJammer::PropertyMap(Include)
        my standard IgnoreErrors   boolean    "Ignore Errors" "No"
        my standard ExecuteAction  choice     "Execute Action" \
            "After Pane is Displayed" \
            $::InstallJammer::PropertyMap(ExecuteAction)
    }

    destructor {
        set name [my name]
        unset -nocomplain ::InstallJammer::actions($name)
    }

    method includes { args } {
        set includes [my cget includes]

        if {[llength $args]} {
            my configure includes [lappend includes {*}$args]
            return $includes
        }
        
        variable ::InstallJammer::components

        set list $includes
        foreach include $list {
            lappend list {*}[$components($include) includes]
        }

        return $list
    }

    method requires { args } {
        set requires [my cget requires]

        if {[llength $args]} {
            my configure requires [lappend requires {*}$args]
            return $requires
        }

        variable ::InstallJammer::components

        set list $requires
        foreach include [my includes] {
            lappend list {*}[$components($include) requires]
        }

        return $list
    }

    method group { args } {
        set group [my cget group]

        if {[llength $args]} {
            variable ::InstallJammer::actiongroups

            set groupName [lindex $args 0]
            set tail [namespace tail $self]

            set actiongroups($group) [lremove $actiongroups($group) $tail]

            set group $groupName
            lappend actiongroups($group) $tail
        }

        return $group
    }
}

::obj::class create ::InstallJammer::Pane {
    superclass ::InstallJammer::ComponentDetails

    variable setup        ""
    variable preview      0
    variable deffile      ""
    variable tclfile      ""
    variable files        [list]
    variable actions      [list]
    variable widgets      [list]
    variable includes     [list]
    variable installtypes [list Standard]

    constructor { args } {
        next {*}$args

        set name [my cget name]

        set ::InstallJammer::panes($name) [namespace tail $self]

        my standard Component  readonly   "Component"
        my standard Conditions conditions "Conditions"
        my standard Include    choice     "Include" \
            "Always include" $::InstallJammer::PropertyMap(Include)
    }

    destructor {
        set name [my cget name]
        unset -nocomplain ::InstallJammer::panes($name)
    }

    method includes { args } {
        set includes [my cget includes]

        if {[llength $args]} {
            my configure includes [lappend includes {*}$args]
            return $includes
        }
        
        variable ::InstallJammer::panes

        set list $includes
        foreach include $list {
            lappend list {*}[$panes($include) includes]
        }

        return $list
    }

    method action { action arguments } {
        set actions [my cget actions]
        my actions [lappend actions [list $action $arguments]]
    }

    method file { file } {
        set files [my cget files]
        my configure files [lappend files $file]
    }

    method directories {} {
        global conf
        global info
        global preferences

        set setup [my cget setup]

        lappend dirs [InstallDir Theme/$setup]
        if {$preferences(CustomThemeDir) ne ""} {
            set custom $preferences(CustomThemeDir)
            lappend dirs [file join $custom $info(Theme) $setup]
        }
        lappend dirs [file join $conf(pwd) Themes $info(Theme) $setup]
    }

    method deffile { {file ""} } {
        if {$file ne ""} { return [my configure deffile $file] }
        set name [my cget name]

        foreach dir [my directories] {
            set file [file join $dir $name.pane]
            if {[file exists $file]} { return $file }
        }
        return $deffile
    }

    method tclfile { {file ""} } {
        if {$file ne ""} { return [my configure tclfile $file] }
        set name [my cget name]

        foreach dir [my directories] {
            set file [file join $dir $name.tcl]
            if {[file exists $file]} { return $file }
        }
        return $tclfile
    }
}

::obj::class create ::InstallJammer::ActionGroup {
    superclass ::InstallJammer::ComponentDetails

    constructor { args } {
        next {*}$args

        my standard Conditions conditions "Conditions"
    }
}

::obj::class create ::InstallJammer::FileGroup {
    superclass ::InstallJammer::ComponentDetails

    constructor { args } {
        next {*}$args

        my standard Name              text   "Name"
        my standard Size              text   "Size"
        my standard CompressionMethod choice "Compression Method" "" \
            [concat {{}} $::conf(CompressionMethods)]
        my standard Destination       installedfile "Destination Directory"
        my standard FileUpdateMethod  filemethod "File Update Method" \
            "Update files with more recent dates"
        my standard FollowDirLinks boolean "Follow Directory Links" "Yes"
        my help FollowDirLinks "If this property is true, links to\
            directories will be followed and their contents stored in the\
            installer as normal files.  If this is false, the directory will\
            be stored as a symlink to be recreated on the target system"
        my standard FollowFileLinks   boolean "Follow File Links" "No"
        my help FollowFileLinks "If this property is true, links to files will\
            be followed, and the linked file will be stored as an actual file\
            within the installer.  If it is false, a link will be stored and\
            recreated as a link on the target system"
        my standard Version           version "Version"
        my standard FileSaveMethod    choice "File Save Method" "" \
            $::conf(FileSaveMethods)
        my help FileSaveMethod "This property determines how the files in this\
            file group will be handled when the project is saved.  See the\
            documentation for more information about each option."

        my standard FileSize          hidden "File Size"
        my standard Attributes        hidden "Windows File Attributes"
        my standard Permissions       hidden "File Permissions"
    }
}

::obj::class create ::InstallJammer::Component {
    superclass ::InstallJammer::ComponentDetails

    constructor { args } {
        next {*}$args

        my standard Name              text    "Name"
        my standard Size              text    "Size"
        my standard Checked           boolean "Checked"            "Yes"
        my standard FileGroups        hidden  "File Groups"
        my standard Selectable        boolean "Selectable"         "Yes"
        my standard ShowComponent     boolean "Show Component"     "Yes"
        my standard ComponentGroup    text    "Component Group"
        my standard IncludeComponents text    "Include Components"
        my help IncludeComponents "A list of components (separated by ;)\
            that should be included whenever this component is selected\
            to be installed.  Each component in the list will also be\
            checked when this component is checked"
        my standard RequiredComponent boolean "Required Component" "No"

        my standard IncludeComponents text    "Include Components"
        my help IncludeComponents "A list of components (separated by ;) that\
            will automatically be checked when this component is checked in\
            the installer"

        my text Description "Description"  1
        my text DisplayName "Display Name" 1
    }
}

::obj::class create ::InstallJammer::SetupType {
    superclass ::InstallJammer::ComponentDetails

    constructor { args } {
        next {*}$args

        my standard Name          text    "Name"
        my standard Components    hidden  "Components"
        my standard ShowSetupType boolean "Show Setup Type"     "Yes"

        my text Description "Description"  1
        my text DisplayName "Display Name" 1
    }
}

::obj::class create ::InstallJammer::File {
    superclass ::InstallJammer::ComponentDetails

    constructor { args } {
        next {*}$args

        my standard CompressionMethod choice "Compression Method" "" \
            [concat {{}} $::conf(CompressionMethods)]
        my standard Destination       installedfile "Destination Directory"
        my standard FileUpdateMethod  filemethod "File Update Method" \
            "Update files with more recent dates"
        my standard Version           version "Version"
        my standard Location          location "Location"
        my standard TargetFilename    short "Target Filename"
        my standard FileSaveMethod    choice "File Save Method" "" \
            $::conf(FileSaveMethods)
        my help FileSaveMethod "This property determines how the files in this\
            file directory will be handled when the project is saved.  See the\
            documentation for more information about each option."
    }
}

::obj::class create ::InstallJammer::Condition {
    superclass ::InstallJammer::ComponentDetails

    variable group     ""
    variable includes  [list]

    constructor { args } {
        next {*}$args

        set name  [my cget name]
        set group [my cget group]

        set ::InstallJammer::components($name) [namespace tail $self]

        set ::InstallJammer::conditions($name) [namespace tail $self]
        lappend ::InstallJammer::conditiongroups($group) [namespace tail $self]

        my standard Component      readonly  "Component"
        my standard CheckCondition choice    "Check Condition" \
            "Before Pane is Displayed" $::conf(PaneCheckConditions)
        my standard FailureFocus   text "Failure Focus"
        my help FailureFocus "A widget to move the focus to after the failure\
                                message has been displayed."
        my standard FailureMessage text "Failure Message"
        my help FailureMessage "A message to display to the user if this\
                                conditon fails."
        my standard Include choice "Include" "Always include" \
            $::InstallJammer::PropertyMap(Include)
    }

    destructor {
        unset -nocomplain ::InstallJammer::conditions($name)
    }

    method group { args } {
        variable ::InstallJammer::conditiongroups

        set group [my group]

        if {[llength $args]} {
            set groupName [lindex $args 0]
            set tail [namespace tail $self]

            set conditiongroups($group) [lremove $conditiongroups($group) $tail]

            set group $groupName
            lappend conditiongroups($group) $tail
        }

        return $group
    }

    method includes { args } {
        set includes [my cget includes]

        if {[llength $args]} {
            my configure includes [lappend includes {*}$args]
            return $includes
        }
        
        variable ::InstallJammer::components

        set list $includes
        foreach include $list {
            lappend list {*}[$components($include) includes]
        }

        return $list
    }
}

::obj::class create Platform {
    superclass InstallComponent

    constructor { args } {
        next {*}$args
        my type platform

        my set Active                "NEW"
        my set BuildSeparateArchives "No"
        my set InstallMode           "Standard"
        my set InstallType           "Typical"
        my set ProgramName           ""
        my set ProgramReadme         "<%InstallDir%>/README.txt"
        my set ProgramLicense        "<%InstallDir%>/LICENSE.txt"
        my set ProgramFolderName     "<%AppName%>"
        my set ProgramExecutable     ""
        my set ProgramFolderAllUsers "No"

        if {[my name] eq "windows"} {
            my set Executable   "<%AppName%>-<%Version%>-Setup<%Ext%>"
            my set FileDescription "<%AppName%> <%Version%> Setup"
            my set InstallDir   "<%PROGRAM_FILES%>/<%AppName%>"
            my set WindowsIcon  "Setup Blue Screen.ico"
            my set RequireAdministrator "Yes"
            my set UseUncompressedBinaries "No"
            set LastRequireAdministrator "Yes"
        } else {
            my set Executable \
                "<%AppName%>-<%Version%>-<%Platform%>-Install<%Ext%>"
            my set InstallDir "<%Home%>/<%ShortAppName%>"
            my set PromptForRoot  "Yes"
            my set RequireRoot    "No"
            my set RootInstallDir "/usr/local/<%ShortAppName%>"
            my set DefaultFilePermission      "0755"
            my set DefaultDirectoryPermission "0755"
            my set FallBackToConsole "Yes"
        }

        if {[my name] in "macos-x macos-x-pcc"} {
            my set BuildType ".app bundle"
            my set VersionDescription "<%AppName%> <%Version%> Setup"
        }
    }

    method initialize {} {}

    method object {} {
        return ::PlatformObject
    }
}
