## InstallJammer Sample Plugin
## 
## This sample plugin shows a very simple example of how to hook into various
## events in InstallJammer.  Each plugin is defined by an initializer
## and any number of hooks into the event system.  A hook has the following
## format:
##
## Hook <hook name> <event> {
##     body
## }
##
## Each plugin is contained within its own namespace within the InstallJammer
## code that is named:
##
## ::InstallJammer::plugins::<plugin name>.
##
## Each hook is created as a proc within the plugin namespace as
##
## ::InstallJammer::plugins::<plugin name>::<hook name>
##
## and then a call is made to attach the hook to the event system.


Init {
    ## Initialize your plugin here.
}

Hook BuildFailure OnBuildFailure {
    ## Attach a hook that executes whenever a build fails.
}

Hook BuildSuccess OnBuildSuccess {
    ## Attach a hook that executes whenever a build succeeds.
}
