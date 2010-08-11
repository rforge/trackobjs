track.sync <- function(pos=1, master=c("auto", "envir", "files"), envir=as.environment(pos), trackingEnv=getTrackingEnv(envir), full=TRUE, dryRun=FALSE, taskEnd=FALSE) {
    ## With master="envir", sync the tracking database to the contents of the R environment
    ## This involves 3 things
    ##   (1) start tracking new untracked variables
    ##   (2) for objects that have disappeared from the environment,
    ##       delete them from the tracking database
    ##   (3) check that all variables are activeBindings (to catch cases where
    ##       {rm(x); assign("x", value)} is performed, which leaves tracked
    ##       variables without an active binding.) This is only done as part "full"
    ##
    ## With master="files", sync the R environment to the tracking database in the filesystem,
    ## which is the job of track.rescan(), so call that.

    ## Could call untracked() and track.orphaned() to find these, but faster
    ## to do ls() on the environment, and look at the tracking file map,
    ## and work out how to sync them up.

    ## Want to use this function as a top-level callback to automatically
    ## track new objects and removed deleted objects, so want it to be fast.

    ## Do check for untrackable objects (isReservedName())

    opt <- track.options(trackingEnv=trackingEnv)
    verbose <- dryRun || opt$debug > 0
    master <- match.arg(master)
    if (master=="auto")
        if (opt$readonly)
            master <- "envir"
        else
            stop("must supply argument master='files' or master='envir' when tracking db is attached with readonly=FALSE")
    if (master=="files")
        return(track.rescan(envir=envir, forgetModified=TRUE, level="low"))
    if (opt$readonly && master=="envir" && !isTRUE(full)) {
        return(list(new=character(0), deleted=character(0)))
    }
    if (master=="envir" && opt$readonly) {
        warning("readonly=TRUE will prevent writing anything to files, but will show what will happen")
        dryRun <- TRUE
    }
    autoTrack <- mget(".trackAuto", envir=trackingEnv, ifnotfound=list(list(on=FALSE, last=-1)))[[1]]
    fileMap <- getFileMapObj(trackingEnv)
    all.objs <- .Internal(ls(envir, TRUE))
    untracked <- setdiff(all.objs, names(fileMap))
    reserved <- isReservedName(untracked)
    ## .trackingEnv will always exist -- don't warn about it
    warn.reserved <- setdiff(untracked[reserved], ".trackingEnv")
    if (verbose && length(warn.reserved))
        cat("track.sync: cannot track variables with reserved names: ", paste(untracked[reserved], collapse=", "), "\n", sep="")
    untracked <- untracked[!reserved]
    activeBindings <- sapply(untracked, bindingIsActive, envir)
    if (verbose && any(activeBindings))
        cat("track.sync: cannot track variables that have active bindings: ", paste(untracked[activeBindings], collapse=", "), "\n", sep="")
    untracked <- untracked[!activeBindings]
    if (length(opt$autoTrackExcludeClass)) {
        excludedClass <- sapply(untracked, function(o) any(is.element(class(get(o, envir=envir, inherits=FALSE)), opt$autoTrackExcludeClass)))
        if (any(excludedClass)) {
            if (verbose)
                cat("track.sync: not tracking variables from excluded classes: ",
                    paste(untracked[excludedClass], collapse=", "), "\n", sep="")
            untracked <- untracked[!excludedClass]
        }
    }
    for (re in opt$autoTrackExcludePattern)
        untracked <- grep(re, untracked, invert=TRUE, value=TRUE)
    deleted <- setdiff(names(fileMap), all.objs)
    if (length(untracked)) {
        if (dryRun) {
            cat("track.sync(dryRun): would track ", length(untracked), " untracked variables: ", paste(untracked, collapse=", "), "\n", sep="")
        } else {
            if (opt$debug > 0)
                cat("track.sync: tracking ", length(untracked), " untracked variables: ", paste(untracked, collapse=", "), "\n", sep="")
            track(list=untracked, envir=envir)
        }
    } else {
        if (verbose)
            cat("track.sync: no untracked variables\n")
    }
    if (length(deleted)) {
        if (dryRun) {
            cat("track.sync(dryRun): would remove ", length(deleted), " deleted variables: ", paste(deleted, collapse=", "), "\n", sep="")
        } else {
            if (opt$debug > 0)
                cat("track.sync: removing ", length(deleted), " deleted variables: ", paste(deleted, collapse=", "), "\n", sep="")
            track.remove(list=deleted, envir=envir, force=TRUE)
        }
    } else {
        if (verbose)
            cat("track.sync: no deleted variables\n")
    }
    now <- as.numeric(proc.time()[3])
    if (is.na(full)) {
        if (opt$autoTrackFullSyncWait==0) {
            full <- TRUE
        } else if (opt$autoTrackFullSyncWait>0) {
            if (autoTrack$last < 0 || now - autoTrack$last >= opt$autoTrackFullSyncWait)
                full <- TRUE
        }
        if (is.na(full))
            full <- FALSE
    }
    retrack <- character(0)
    if (full) {
        # find the vars that look like they are tracked but don't have active bindings
        tracked <- intersect(names(fileMap), all.objs)
        reserved <- isReservedName(tracked)
        if (verbose && any(reserved))
            cat("track.sync: cannot track variables with reserved names: ", paste(tracked[reserved], collapse=", "), "\n", sep="")
        tracked <- tracked[!reserved]
        if (length(tracked)) {
            retrack <- tracked[!sapply(tracked, bindingIsActive, envir)]
        }
    }
    if (length(retrack))
        for (re in opt$autoTrackExcludePattern)
            retrack <- grep(re, retrack, invert=TRUE, value=TRUE)

    ## Deal with untracked objects in the tracked env.
    ## Need to write these to files, and replace with active bindings.
    for (objname in retrack) {
        ## get obj from envir, store in file, create active binding
        objval <- get(objname, envir=envir, inherits=FALSE)
        if (any(is.element(class(objval), opt$autoTrackExcludeClass))) {
            if (verbose)
                cat("track.sync", if (dryRun) "(dryRun)", ": var is from excluded class, not tracking: ", objname, "\n", sep="")
            next
        }
        if (verbose)
            cat("track.sync: retracking var: ", objname, "\n", sep="")
        if (dryRun)
            next
        ## Use setTrackedVar to write the object to disk (or merely cache
        ## it in trackingEnv, depending on settings in opt).
        ## setTrackedVar() will assign it in the trackingEnv -- it currently
        ## exists in 'envir'
        setTrackedVar(objname, objval, trackingEnv, opt)
        remove(list=objname, envir=envir)
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
    if (full & !dryRun) {
        ## save the time that we did this full sync
        assign(".trackAuto", list(on=TRUE, last=now), envir=trackingEnv)
    }
    if (taskEnd && opt$cachePolicy=="withinTask") {
        if (dryRun)
            cat("track.sync(dryRun): Would flush all vars\n")
        else
            track.flush(envir=envir, all=TRUE)
    }
    return(invisible(list(new=untracked, removed=deleted)))
}

track.sync.callback <- function(expr, ok, value, visible, data) {
    ## To automatically track new and deleted objects, do
    ##   addTaskCallback(track.sync.callback, data=globalenv())
    ## and
    ##   assign(".trackAuto", list(on=TRUE, last=-1), envir=trackingEnv)
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
    track.sync(envir=data, trackingEnv=trackingEnv, full=NA, master="envir", taskEnd=TRUE)
    return(TRUE) # to keep this callback active
}
