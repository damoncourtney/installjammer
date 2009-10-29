if {[catch {package require Tcl 8.2}]} return
package ifneeded Tktable 2.10 \
    [list load [file join $dir libTktable2.10.so] Tktable]
