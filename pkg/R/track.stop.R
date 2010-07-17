track.stop <- function(pos=1, envir=as.environment(pos), all=FALSE, stop.on.error=FALSE, keepVars=FALSE) {
    ## Detach a tracking env -- should call track.flush first.
    if (all) {
        env.names <- tracked.envs()
        for (e.name in env.names)
            if (stop.on.error)
                track.stop(envir=as.environment(e.name), keepVars=keepVars)
            else
                try(track.stop(envir=as.environment(e.name), keepVars=keepVars))
    } else {
        trackingEnv <- getTrackingEnv(envir)
        opt <- track.options(trackingEnv=trackingEnv)
        if (opt$readonly && keepVars)
            stop("cannot keep vars in a readonly tracking environment")
        tracked.vars <- tracked(envir=envir)
        track.flush(envir=envir)
        # remove the active bindings, and replace with ordinary variable if keepVars=TRUE
        if (!opt$readonly) {
            for (var in tracked.vars) {
                if (keepVars) {
                    objVal <- getTrackedVar(var, trackingEnv=trackingEnv, opt=opt)
                    remove(list=var, envir=envir)
                    assign(var, objVal, envir=envir)
                } else {
                    remove(list=var, envir=envir)
                }
            }
        }
        setTrackingEnv(envir, NULL, readonly=opt$readonly)
        if (exists(".trackAuto", envir=trackingEnv, inherits=FALSE))
            if (opt$readonly) {
                current <- get(".trackAuto", envir=trackingEnv, inherits=FALSE)
                current$on <- FALSE
                assign(".trackAuto", current, envir=trackingEnv)
            } else {
                remove(list=".trackAuto", envir=trackingEnv)
            }
    }
    return(invisible(NULL))
}

