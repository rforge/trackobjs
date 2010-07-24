track.stop <- function(pos=1, envir=as.environment(pos), all=FALSE, stop.on.error=FALSE, keepVars=FALSE) {
    if (missing(pos) && missing(envir) && missing(all))
        stop("must specify one of pos, envir, or all")
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
        if (opt$readonly && keepVars && environmentIsLocked(envir))
            stop("cannot keep vars in a locked readonly tracking environment")
        ## remove the active bindings, and replace with ordinary variable if keepVars=TRUE
        tracked.vars <- tracked(envir=envir)
        if (!opt$readonly) {
            auto <- mget(".trackAuto", ifnotfound=list(list(on=FALSE, last=-1)), envir=trackingEnv)[[1]]
            if (auto$on)
                track.sync(envir=envir, master="envir", full=TRUE, trackingEnv=trackingEnv)
            track.flush(envir=envir)
        }
        if (keepVars) {
            for (var in tracked.vars) {
                objVal <- getTrackedVar(var, trackingEnv=trackingEnv, opt=opt)
                remove(list=var, envir=envir)
                assign(var, objVal, envir=envir)
            }
        } else {
            ## Can't remove the variables from a locked environment -- these
            ## should all be active bindings.  This situation can arise when
            ## a readonly tracking env is being detached, so we can hope that
            ## the active bindings are garbage collected when the tracked
            ## environment is removed from the search path.
            if (!environmentIsLocked(envir))
                remove(list=tracked.vars, envir=envir)
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

