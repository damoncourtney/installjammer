catch {wm withdraw .}
set ::env(IJTEST_DIR) [file normalize [file dirname [info script]]]
set ::env(IJTEST_TMP) [file join $::env(IJTEST_DIR) tmp]
set ::env(IJTEST_BIN) \
    [file normalize [file join $::env(IJTEST_DIR) .. installjammer]]

set ::total  0
set ::passed 0
set ::failed {}

proc test {name desc args} {
    array set _args $args

    echo "$name: $desc"
    incr ::total
    set code [catch {uplevel #0 $_args(-body)} result]
    if {$code == 1} {
        lappend ::failed $name
    } else {
        incr ::passed
    }
    cd $::env(IJTEST_DIR)
}

proc echo {string {tag message}} {
    if {[winfo exists .__test]} {
        .__test.t insert end $string\n $tag
        update
    }

    puts  stdout $string
    flush stdout
}

proc readFile {file} {
    set fp [open $file]
    set x  [read $fp]
    close $fp
    return $x
}

proc makeDirectory {dir} {
    variable test
    if {![file exists $dir]} { file mkdir $dir }
    lappend test(dirs) $dir
}

proc project {project} {
    global info

    unset -nocomplain ::env(IJTEST_PROJECT)

    set mpi $project
    if {[file isdir $project]} { set mpi [file join $project $project.mpi] }
    if {[file extension $mpi] ne ".mpi"} { append mpi .mpi }
    if {![file exists $mpi]} { return }
    set ::env(IJTEST_PROJECT) $mpi

    uplevel #0 [list catch [list source $mpi]]
}

proc buildInstaller {args} {
    if {[info exists ::env(IJTEST_INSTALLER)]} {
        return $::env(IJTEST_INSTALLER)
    }
    eval rebuildInstaller $args
}

proc rebuildInstaller {args} {
    makeDirectory build
    makeDirectory output

    set project $::env(IJTEST_PROJECT)

    set opts [list]
    lappend opts --build-dir [file join $::env(IJTEST_DIR) build]
    lappend opts --output-dir [file join $::env(IJTEST_DIR) output]
    lappend opts --build-log-file [file join $::env(IJTEST_DIR) build build.log]
    lappend opts --include-all-apis
    eval lappend opts $args
    lappend opts --build [file join $::env(IJTEST_DIR) $project]
    set result [eval exec [list $::env(IJTEST_BIN)] $opts]

    if {[regexp {Installer: ([^\n]+)} $result -> installer]} {
        set ::env(IJTEST_INSTALLER) $installer
        return $installer
    } else {
        return -code error $result
    }
}

proc runInstallerTest {body args} {
    if {![info exists ::env(IJTEST_INSTALLER)]} {
        if {![info exists ::env(IJTEST_PROJECT)]} {
            return -code error "could not find test installer"
        }
        buildInstaller $::env(IJTEST_PROJECT)
    }

    makeDirectory $::env(IJTEST_TMP)

    set tmp [file join $::env(IJTEST_TMP) script]

    append script {
        proc error {string} {
            incr0 ::ij_test_errors
            puts "Test error: $string"
            exit
        }
        #
        ## Add an exit event to dump the info array for testing.
        inject enter exit {
            if {![info exists ::ij_test_errors]} {
                puts "array set info [list [array get info]]"
            }
        }
    } $body

    set fp [open $tmp w]
    puts $fp $script
    close $fp

    set installer $::env(IJTEST_INSTALLER)
    catch {eval exec [list $installer --test-script $tmp] $args} res
    if {[string match "array set info*" $res]} {
        uplevel #0 $res
    } else {
        error $res
    }
}

proc runBuilderTest {body} {
    variable test

    if {![info exists test(builderSock)]} {
        set port 60006
        exec $::env(IJTEST_BIN) --command-port $port -- &
        set test(builderSock) [socket localhost $port]
        gets $test(builderSock) line
    }

    append script {
        proc error {string} {
            incr0 ::ij_test_errors
            puts "Test error: $string"
            exit
        }
    } $body

    puts  $test(builderSock) $script
    flush $test(builderSock)
    set response {}
    while {[gets $test(builderSock) line] != -1} {
        if {$line eq "OK"} { break }
        lappend response $line
    }
    return [join $response \n]
}

proc makeOutputWindow {} {
    set top .__test
    toplevel    $top
    wm title    $top "Test Output"
    wm protocol $top WM_DELETE_WINDOW quit
    text $top.t -font "Courier 10"
    $top.t tag configure error -foreground red
    pack $top.t -expand 1 -fill both
}

proc runAllTests {} {
    makeOutputWindow

    set first 1
    foreach file [glob -nocomplain *.test] {
        if {$first} {
            set first 0
        } else {
            echo ""
        }
        echo "===== $file ====="
        project [file root $file].mpi
        catch {uplevel #0 [list source $file]}
        unset -nocomplain ::env(IJTEST_INSTALLER)
    }

    echo ""
    echo "Total:  $::total"
    echo "Passed: $::passed"
    echo "Failed: [llength $::failed]"
    if {[llength $::failed]} {
        echo "Failed Test:\n\n[join $::failed \n]"
    }
}

proc error {string} {
    echo $string error
    return -code error
}

proc quit {} {
    cleanupTests
    exit
}

proc cleanupTests {} {
    variable test

    if {[info exists test(builderSock)]} {
        puts  $test(builderSock) exit
        flush $test(builderSock)
    }

    if {[info exists test(dirs)]} {
        foreach dir $test(dirs) {
            file delete -force $dir
        }
    }
}
