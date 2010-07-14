track.sync <- function(pos=1, envir=as.environment(pos), trackingEnv=getTrackingEnv(envir), forceFull=TRUE) {
    ## Sync up the tracking database with the contents of the environment
    ## This involves 3 things
    ##   (1) start tracking new untracked variables
    ##   (2) for objects that have disappeared from the environment, delete them from
    ##       the tracking database
    ##   (3) check that all variables are activeBindings (to catch cases where
    ##       {rm(x); assign("x", value)} is performed, which leaves

    ## Could call untracked() and track.orphaned() to find these, but faster
    ## to do ls() on the environment, and look at the tracking file map,
    ## and work out how to sync them up.

    ## Want to use this function as a top-level callback to automatically
    ## track new objects and removed deleted objects, so want it to be fast.

    ## Do check for untrackable objects (isReservedName())

    opt <- track.options(trackingEnv=trackingEnv)
    autoTrack <- mget(".trackAuto", envir=trackingEnv, ifnotfound=list(list(on=FALSE, last=-1)))[[1]]
    fileMap <- getFileMapObj(trackingEnv)
    all.objs <- .Internal(ls(envir, TRUE))
    untracked <- setdiff(all.objs, names(fileMap))
    reserved <- isReservedName(untracked)
    if (opt$debug > 0 && any(reserved))
        cat("track.sync: cannot track variables with reserved names: ", paste(untracked[reserved], collapse=", "), "\n", sep="")
    untracked <- untracked[!reserved]
    activeBindings <- sapply(untracked, bindingIsActive, envir)
    if (opt$debug > 0 && any(activeBindings))
        cat("track.sync: cannot track variables that have active bindings: ", paste(untracked[activeBindings], collapse=", "), "\n", sep="")
    untracked <- untracked[!activeBindings]
    for (re in opt$autoTrackExcludePattern)
        untracked <- grep(re, untracked, invert=TRUE, value=TRUE)
    deleted <- setdiff(names(fileMap), all.objs)
    if (opt$debug > 0)
        if (length(untracked))
            cat("track.sync: tracking ", length(untracked), " untracked variables: ", paste(untracked, collapse=", "), "\n", sep="")
        else
            cat("track.sync: no untracked variables\n")
    if (length(untracked))
        track(list=untracked, envir=envir)
    if (opt$debug > 0)
        if (length(deleted))
            cat("track.sync: removing ", length(deleted), " deleted variables: ", paste(deleted, collapse=", "), "\n", sep="")
        else
            cat("track.sync: no deleted variables\n")
    if (length(deleted))
        track.remove(list=deleted, envir=envir, force=TRUE)
    now <- as.numeric(proc.time()[3])
    doFull <- FALSE
    if (forceFull || opt$autoTrackFullSyncWait==0) {
        doFull <- TRUE
    } else if (opt$autoTrackFullSyncWait>0) {
        if (autoTrack$last < 0 || now - autoTrack$last >= opt$autoTrackFullSyncWait)
            doFull <- TRUE
    }
    if (doFull) {
        # find the vars that look like they are tracked but don't have active bindings
        tracked <- intersect(names(fileMap), all.objs)
        reserved <- isReservedName(tracked)
        if (opt$debug > 0 && any(reserved))
            cat("track.sync: cannot track variables with reserved names: ", paste(tracked[reserved], collapse=", "), "\n", sep="")
        tracked <- tracked[!reserved]
        if (length(tracked)) {
            retrack <- tracked[!sapply(tracked, bindingIsActive, envir)]
            if (length(retrack))
                for (re in opt$autoTrackExcludePattern)
                    retrack <- grep(re, retrack, invert=TRUE, value=TRUE)
            if (length(retrack)) {
                for (objname in retrack) {
                    if (opt$debug > 0)
                        cat("track.sync: re-tracking var: ", objname, "\n", sep="")
                    objval <- get(objname, envir=envir, inherits=FALSE)
                    remove(list=objname, envir=envir)
                    setTrackedVar(objname, objval, trackingEnv, opt)
                    f <- substitute(function(v) {
                        if (missing(v))
                            getTrackedVar(x, envir)
                        else
                            setTrackedVar(x, v, envir)
                    }, list(x=objname, envir=trackingEnv))
                    mode(f) <- "function"
                    ## Need to replace the environment of f, otherwise it is this
                    ## function, which can contain a copy of objval, which can
                    ## use up lots of memory!
                    ## Need to be careful with the choice of env to set here:
                    ##   * emptyenv() doesn't work because then the binding can't find
                    ##     any defns
                    ##   * baseenv() doesn't work because then the function in the
                    ##     binding can't find functions from trackObjs
                    ##   * globalenv() doesn't work because the function in the
                    ##     binding can't find non-exported functions from trackObjs
                    ##   * parent.env(environment(f)) works!
                    environment(f) <- parent.env(environment(f))
                    makeActiveBinding(objname, env=envir, fun=f)
                }
            }
        }
        assign(".trackAuto", list(on=TRUE, last=now), envir=trackingEnv)
    }
    return(invisible(list(new=untracked, removed=deleted)))
}

track.sync.callback <- function(expr, ok, value, visible, data) {
    ## To automatically track new and deleted objects, do
    ##   addTaskCallback(track.sync.callback, data=globalenv())
    ## and
    ##   assign(".trackAuto", list(on=TRUE, last=-1), envir=globalenv())
    trackingEnv <- getTrackingEnv(data, stop.on.not.tracked = FALSE)
    ## trackingEnv will be missing on the callback following the completion
    ## of the command track.stop()
    if (is.null(trackingEnv))
        return(FALSE)
    autoTrack <- mget(".trackAuto", envir=trackingEnv, ifnotfound=list(list(on=FALSE, last=-1)))[[1]]
    ## This is the easist way to remove the callback when it is no longer wanted,
    ## otherwise we have the problem of identifying the appropriate callback
    if (!isTRUE(autoTrack$on))
        return(FALSE)
    ## Don't repeat the work an explicit call to track.sync()
    if (is.call(expr) && as.character(expr[[1]]) == "track.sync")
        return(TRUE)
    track.sync(envir=data, trackingEnv=trackingEnv, forceFull=FALSE)
    return(TRUE) # to keep this callback active
}
