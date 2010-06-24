track.sync <- function(pos=1, envir=as.environment(pos), trackingEnv=getTrackingEnv(envir)) {
    ## Sync up the tracking database with the contents of the environment
    ## This involves 2 things
    ##   (1) start tracking new untracked variables
    ##   (2) for objects that have disappeared from the environment, delete them from
    ##       the tracking database

    ## Could call untracked() and track.orphaned() to find these, but faster
    ## to do ls() on the environment, and look at the tracking file map,
    ## and work out how to sync them up.

    ## Want to use this function as a top-level callback to automatically
    ## track new objects and removed deleted objects, so want it to be fast.
    
    ## Do check for untrackable objects (isReservedName())

    opt <- track.options(trackingEnv=trackingEnv)
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
    for (re in opt$autoTrackExclude)
        untracked <- grep(re, untracked, invert=TRUE, value=TRUE)
    deleted <- setdiff(names(fileMap), all.objs)
    if (opt$debug > 0)
        if (length(untracked))
            cat("track.sync: tracking ", length(untracked), " untracked variables: ", paste(untracked, collapse=", "), "\n", sep="")
        else
            cat("track.sync: no untracked variables\n")
    if (length(untracked)) track(list=untracked, envir=envir)
    if (opt$debug > 0)
        if (length(deleted))
            cat("track.sync: removing ", length(deleted), " deleted variables: ", paste(deleted, collapse=", "), "\n", sep="")
        else
            cat("track.sync: no deleted variables\n")
    if (length(deleted)) track.remove(list=deleted, envir=envir, force=TRUE)
    return(invisible(list(new=untracked, removed=deleted)))
}

track.sync.callback <- function(expr, ok, value, visible, data) {
    ## To automatically track new and deleted objects, do
    ##   addTaskCallback(track.sync.callback, data=globalenv())
    ## and
    ##   assign(".trackAuto", TRUE, envir=globalenv())
    trackingEnv <- getTrackingEnv(data)
    keep.auto.tracking <- mget(".trackAuto", envir=trackingEnv, ifnotfound=FALSE)[[1]]
    ## This is the easist way to remove the callback when it is no longer wanted,
    ## otherwise we have the problem of identifying the appropriate callback
    if (!isTRUE(keep.auto.tracking))
        return(FALSE)
    track.sync(envir=data, trackingEnv=trackingEnv)
    return(TRUE) # to keep this callback active
}
