namespace eval ::InstallJammer::plugins {
    array set hooks {
        OnBuildFailure      {platform}
        OnBuildSuccess      {platform filename}
    }
}

proc ::InstallJammer::AttachHook { event proc } {
    variable hooks
    if {![info exists ::InstallJammer::plugins::hooks($event)]} {
        return -code error "bad event \"$event\" attaching hook"
    }
    lappend hooks($event) $proc
}

proc ::InstallJammer::CallHook { name args } {
    variable hooks
    if {[info exists hooks($name)]} {
        foreach proc $hooks($name) {
            eval [list $proc] $args
        }
    }
}

proc ::InstallJammer::plugins::Init { body } {
    variable plugin
    proc ::InstallJammer::plugins::${plugin}::Init {} $body
}

proc ::InstallJammer::plugins::Hook { name event body } {
    variable hooks
    variable plugin
    if {![info exists hooks($event)]} {
        return -code error "bad event \"$event\" creating hook"
    }
    set proc ::InstallJammer::plugins::${plugin}::$name
    proc $proc $hooks($event) $body
    ::InstallJammer::AttachHook $event $proc
}
