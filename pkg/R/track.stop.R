track.stop <- function(pos=1, envir=as.environment(pos), all=FALSE, stop.on.error=FALSE, keepVars=FALSE, sessionEnd=FALSE) {
    if (missing(pos) && missing(envir) && missing(all))
        stop("must specify one of pos, envir, or all")
    ## Want the quiet arg for when this is run at the end of an R session --
    ## no recovery possible in that case.
    if (keepVars && sessionEnd) {
        warning("cannot keepVars when sessionEnd==TRUE")
        keepVars <- FALSE
    }
    if (stop.on.error && sessionEnd) {
        warning("cannot use stop.on.error when sessionEnd==TRUE")
        stop.on.error <- FALSE
    }
    ## Detach a tracking env -- should call track.flush first.
    if (all) {
        env.names <- tracked.envs()
        for (e.name in env.names)
            if (stop.on.error)
                track.stop(envir=as.environment(e.name), keepVars=keepVars, sessionEnd=sessionEnd)
            else
                try(track.stop(envir=as.environment(e.name), keepVars=keepVars, sessionEnd=sessionEnd))
    } else {
        if (!env.is.tracked(envir)) {
            if (sessionEnd)
                warning("strange: environment ", environmentName(envir), " is not tracked")
            else
                warning("environment ", environmentName(envir), " is not tracked")
            return(invisible(NULL))
        }
        trackingEnv <- getTrackingEnv(envir)
        opt <- track.options(trackingEnv=trackingEnv)
        if (opt$readonly && keepVars && environmentIsLocked(envir))
            stop("cannot keep vars in a locked readonly tracking environment")
        ## Remove the active bindings, and replace with ordinary variable if keepVars=TRUE
        tracked.vars <- tracked(envir=envir)
        if (!opt$readonly) {
            auto <- mget(".trackAuto", ifnotfound=list(list(on=FALSE, last=-1)), envir=trackingEnv)[[1]]
            ## If this is an auto-update tracking env, get vars and files in sync,
            ## otherwise we presume the user knows what they're doing -- ignore
            ## out-of-sync things
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
        ## Set the tracking env pointer to NULL rather than removing it, in
        ## case 'envir' is locked.
        setTrackingEnv(envir, NULL, readonly=opt$readonly)
        if (exists(".trackAuto", envir=trackingEnv, inherits=FALSE)) {
            ## Should always be OK to remove a var from the trackingEnv,
            ## even when the db is readonly or locked -- it's the tracked
            ## env that gets locked, not the tracking env.
            if (FALSE && opt$readonly) {
                current <- get(".trackAuto", envir=trackingEnv, inherits=FALSE)
                current$on <- FALSE
                assign(".trackAuto", current, envir=trackingEnv)
            } else {
                remove(list=".trackAuto", envir=trackingEnv)
            }
        }
        ## Assign a marker variable so that a finalizer can see
        ## when this env is done with.
        assign(".trackFinished", TRUE, envir=trackingEnv)
    }
    return(invisible(NULL))
}

