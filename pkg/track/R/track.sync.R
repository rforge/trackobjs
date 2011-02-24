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
    if (verbose)
        cat("track.sync", if (dryRun) "(dryRun)",
            ": syncing tracked env ", envname(envir), "\n", sep="")
    master <- match.arg(master)
    if (master=="auto")
        if (opt$readonly)
            master <- "envir"
        else
            stop("must supply argument master='files' or master='envir' when tracking db is attached with readonly=FALSE")
    if (master=="files")
        return(track.rescan(envir=envir, forgetModified=TRUE, level="low"))

    ## Get info about the state of things
    autoTrack <- mget(".trackAuto", envir=trackingEnv, ifnotfound=list(list(on=FALSE, last=-1)))[[1]]
    fileMap <- getFileMapObj(trackingEnv)
    all.objs <- .Internal(ls(envir, TRUE))
    ## 'untracked' will be the untracked vars that we want to track
    untracked <- setdiff(all.objs, names(fileMap))
    reserved <- isReservedName(untracked)
    ## .trackingEnv will always exist -- don't warn about it
    warn.reserved <- setdiff(untracked[reserved], ".trackingEnv")
    if (verbose && length(warn.reserved))
        cat("track.sync: cannot track variables with reserved names: ", paste(warn.reserved, collapse=", "), "\n", sep="")
    untracked <- untracked[!reserved]
    activeBindings <- sapply(untracked, bindingIsActive, envir)
    if (verbose && any(activeBindings))
        cat("track.sync: cannot track variables that have active bindings: ", paste(untracked[activeBindings], collapse=", "), "\n", sep="")
    untracked <- untracked[!activeBindings]
    if (length(opt$autoTrackExcludeClass)) {
        hasExcludedClass <- sapply(untracked, function(o) any(is.element(class(get(o, envir=envir, inherits=FALSE)), opt$autoTrackExcludeClass)))
        if (any(hasExcludedClass)) {
            if (verbose)
                cat("track.sync: not tracking variables from excluded classes: ",
                    paste(untracked[hasExcludedClass], collapse=", "), "\n", sep="")
            untracked <- untracked[!hasExcludedClass]
        }
    }
    for (re in opt$autoTrackExcludePattern)
        untracked <- grep(re, untracked, invert=TRUE, value=TRUE)
    deleted <- setdiff(names(fileMap), all.objs)

    ## The only thing to do for a readonly env is
    ## to flush cached objects out of memory.
    ## Well..., perhaps we should also check that
    ## no new variables have been created, and if
    ## they have, warn about them.  But, that takes
    ## time, and this function is called after every
    ## top level task...

    ## Deal with new (untracked) and deleted variables
    if (length(untracked)) {
        if (opt$readonly) {
            warning(length(untracked), " variables created in a readonly tracking env, these will not be written out to the files: ",
                    paste(untracked, collapse=", "))
        } else if (dryRun) {
            cat("track.sync(dryRun): would track ", length(untracked), " untracked variables: ", paste(untracked, collapse=", "), "\n", sep="")
        } else {
            if (verbose > 0)
                cat("track.sync: tracking ", length(untracked), " untracked variables: ", paste(untracked, collapse=", "), "\n", sep="")
            track(list=untracked, envir=envir)
            fileMap <- getFileMapObj(trackingEnv)
        }
    } else {
        if (verbose)
            cat("track.sync: no untracked variables\n")
    }
    if (length(deleted)) {
        if (opt$readonly) {
            warning(length(deleted), "variables deleted from a readonly tracking env, these will not be deleted from the files: ",
                    paste(deleted, collapse=", "))
        } else if (dryRun) {
            cat("track.sync(dryRun): would remove ", length(deleted), " deleted variables: ", paste(deleted, collapse=", "), "\n", sep="")
        } else {
            if (verbose > 0)
                cat("track.sync: removing ", length(deleted), " deleted variables: ", paste(deleted, collapse=", "), "\n", sep="")
            track.remove(list=deleted, envir=envir, force=TRUE)
            fileMap <- getFileMapObj(trackingEnv)
        }
    } else {
        if (verbose)
            cat("track.sync: no deleted variables\n")
    }
    now <- as.numeric(proc.time()[3])
    full.orig <- full
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
    if (!opt$readonly) {
        ## Don't look for changes in a readonly db -- takes too long
        ## (there could be changes, and we could warn about them...)
        retrack <- character(0)
        if (full) {
            trace <- is.na(full.orig) && getOption("track.callbacks.trace", FALSE)
            if (trace) {
                cat("track.sync.callback", envname(envir), ": look for vars without active bindings at ", date(), "\n", sep="")
                stime <- proc.time()
            }
            ## Find the vars that look like they are tracked but don't have active bindings
            ## This can be time consuming -- need to call bindingIsActive for each tracked
            ## var.
            tracked <- intersect(names(fileMap), all.objs)
            reserved <- isReservedName(tracked)
            if (verbose && any(reserved))
                cat("track.sync: cannot track variables with reserved names: ", paste(tracked[reserved], collapse=", "), "\n", sep="")
            tracked <- tracked[!reserved]
            if (length(tracked))
                retrack <- tracked[!sapply(tracked, bindingIsActive, envir)]
            if (trace) {
                cat("track.sync.callback: finished looking for vars without active bindings",
                            " (", paste(round(1000*(proc.time()-stime)[1:3]), c("u", "s", "e"), sep="", collapse=" "), " ms)\n", sep="")
            }
        }
        if (length(retrack))
            for (re in opt$autoTrackExcludePattern)
                retrack <- grep(re, retrack, invert=TRUE, value=TRUE)

        ## Deal with untracked objects in the tracked env.
        ## Need to write these to files, and replace with active bindings.
        for (objName in retrack) {
            ## get obj from envir, store in file, create active binding
            objval <- get(objName, envir=envir, inherits=FALSE)
            if (any(is.element(class(objval), opt$autoTrackExcludeClass))) {
                if (verbose)
                    cat("track.sync", if (dryRun) "(dryRun)", ": var is from excluded class, not tracking: ", objName, "\n", sep="")
                next
            }
            if (verbose && !opt$readonly)
                cat("track.sync: retracking var: ", objName, "\n", sep="")
            if (opt$readonly)
                warning("binding for variable ", objName, " was clobbered in a readonly tracking env -- forgetting the new value, restoring the old")
            if (dryRun)
                next
            ## Use setTrackedVar to write the object to disk (or merely cache
            ## it in trackingEnv, depending on settings in opt).
            ## setTrackedVar() will assign it in the trackingEnv -- it currently
            ## exists in 'envir'
            if (!opt$readonly)
                setTrackedVar(objName, objval, trackingEnv, opt)
            remove(list=objName, envir=envir)
            f <- substitute(function(v) {
                if (missing(v))
                    getTrackedVar(x, envir)
                else
                    setTrackedVar(x, v, envir)
            }, list(x=objName, envir=trackingEnv))
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
            makeActiveBinding(objName, env=envir, fun=f)
        }
        ## Do we need to re-read the fileMap?
        if (length(retrack))
            fileMap <- getFileMapObj(trackingEnv)
    }

    ## Which variables are currently cached?
    ## Can't do this until after have checked for untracked vars,
    ## otherwise won't treat those properly.
    ## This code used to call track.flush(envir=envir, all=TRUE)
    ## but that's slow compared to working out flushVars here and
    ## passing the specific vars to track.flush()
    ##
    ## Record variables to flush from cache in flushVars.
    ## Record variables that need saving to disk in saveVars
    ## Note that vars are actually flushed from cache by a
    ## call to track.flush(), which won't flush vars named
    ## in opt$alwaysCache.
    if (taskEnd && opt$cachePolicy=="eotPurge") {
        flushVars <- NULL
        saveVars <- NULL
        unsavedVars <- getUnsavedObj(trackingEnv)
        objSummary <- getObjSummary(trackingEnv, opt=opt)
        if (!is.null(objSummary)) {
            ## which variables are currently cached in memory and are candidate for flushing?
            inmem <- is.element(rownames(objSummary), .Internal(ls(trackingEnv, TRUE)))
            keep1 <- !is.na(objSummary$cache) & (objSummary$cache %in% c("yes", "fixedyes"))
            flushCand <- inmem & !keep1
            if (!any(flushCand)) {
                flushVars <- character(0)
            } else if (length(opt$cacheKeepFun)) {
                ## If there is a cacheKeepFun, see what it says...
                keep <- try(do.call(opt$cacheKeepFun, list(objs=objSummary, inmem=flushCand, envname=envname(envir))), silent=TRUE)
                if (is(keep, "try-error")) {
                    warning("opt$cacheKeepFun stopped with an error: ", keep)
                    keep <- F
                } else if (!is.logical(keep) || length(keep)!=nrow(objSummary) || any(is.na(keep))) {
                    warning("opt$cacheKeepFun did not return a TRUE/FALSE vector of the correct length")
                    keep <- F
                }
                flushVars <- rownames(objSummary)[flushCand & !keep]
                saveVars <- intersect(rownames(objSummary)[flushCand & keep], unsavedVars)
            } else {
                keep <- F
                flushVars <- rownames(objSummary)[flushCand & !keep]
                saveVars <- intersect(rownames(objSummary)[flushCand & keep], unsavedVars)
            }
            if (length(flushVars)) {
                if (length(opt$alwaysCache)) {
                    i <- is.element(flushVars, opt$alwaysCache)
                    if (any(i)) {
                        saveVars <- unique(c(saveVars, intersect(flushVars[i], unsavedVars)))
                        flushVars <- flushVars[!i]
                    }
                }
            }
        } else {
            warning(".trackingSummary does not exist in trackingEnv ", envname(trackingEnv))
            flushVars <- .Internal(ls(trackingEnv, TRUE))
            flushVars <- flushVars[is.element(flushVars, names(fileMap))]
        }
        if (dryRun) {
            cat("track.sync(dryRun): Would flush", length(flushVars), "vars:",
                paste(flushVars, collapse=", "), "\n")
            cat("track.sync(dryRun): Would save", length(saveVars), "vars:",
                paste(saveVars, collapse=", "), "\n")
        } else {
            if (verbose)
                cat("track.sync: flushing ", length(flushVars), " vars with call to track.flush(envir=",
                    envname(envir), ", list=c(", paste("'", flushVars, "'", sep="", collapse=", "), "))\n", sep="")
            if (length(flushVars))
                track.flush(envir=envir, list=flushVars)
            if (length(saveVars))
                track.save(envir=envir, list=saveVars)
        }
    } else {
        if (dryRun) {
            cat("track.sync(dryRun): Would save all vars\n")
        } else {
            if (verbose)
                cat("track.sync: calling track.save(envir=", envname(envir), ")\n", sep="")
            track.save(envir=envir, all=TRUE)
        }
    }
    if (!opt$readonly) {
        ##  write out the object summary if necessary
        summaryChanged <- mget(".trackingSummaryChanged", ifnotfound=list(FALSE), envir=trackingEnv)[[1]]
        if (summaryChanged) {
            if (!exists(".trackingSummary", envir=trackingEnv, inherits=FALSE)) {
                warning("no .trackingSummary in trackng env ", envname(trackingEnv))
            } else {
                dir <- getTrackingDir(trackingEnv)
                file <- file.path(getDataDir(dir), paste(".trackingSummary", opt$RDataSuffix, sep="."))
                if (verbose)
                    cat("track.sync: saving .trackingSummary for envir=", envname(envir), " to ", dir, "\n", sep="")
                save.res <- try(save(list=".trackingSummary", file=file, envir=trackingEnv, compress=FALSE), silent=TRUE)
                if (is(save.res, "try-error"))
                    warning("unable to save .trackingSummary to ", dir)
                else
                    assign(".trackingSummaryChanged", FALSE, envir=trackingEnv)
            }
        }
    }
    if (full & !dryRun) {
        ## save the time that we did this full sync
        autoTrack$last <- now
        assign(".trackAuto", autoTrack, envir=trackingEnv)
    }
    return(invisible(list(new=untracked, removed=deleted)))
}

track.sync.callback <- function(expr, ok, value, visible, data) {
    ## To automatically track new and deleted objects, do
    ##   addTaskCallback(track.sync.callback, data=globalenv())
    ## and
    ##   assign(".trackAuto", list(on=TRUE, last=-1), envir=trackingEnv)
    ## 'data' arg is 'envir' - the tracked env
    trace <- getOption("track.callbacks.trace", FALSE)
    if (trace) {
        cat("track.sync.callback", envname(data), ": entered at ", date(), ", length(sys.calls())=", length(sys.calls()), "\n", sep="")
        stime <- proc.time()
        on.exit(cat("track.sync.callback: exited at ", date(),
                    " (", paste(round(1000*(proc.time()-stime)[1:3]), c("u", "s", "e"), sep="", collapse=" "), " ms)\n", sep=""))
    }
    ## If we are called from a prompt in a browser, don't do anything
    if (length(sys.calls()) > 1)
        return(TRUE)
    trackingEnv <- getTrackingEnv(data, stop.on.not.tracked = FALSE)
    ## trackingEnv will be missing on the callback following the completion
    ## of the command track.stop()
    i <- match(paste("track.auto:", envname(data), sep=""), getTaskCallbackNames())
    if (length(i)>1)
        warning("have more than one callback for ", paste("track.auto:", envname(data), sep=""))
    if (is.null(trackingEnv))
        return(FALSE)
    autoTrack <- mget(".trackAuto", envir=trackingEnv, ifnotfound=list(list(on=FALSE, last=-1)))[[1]]
    ## This is the easist way to remove the callback when it is no longer wanted,
    ## otherwise we have the problem of identifying the appropriate callback
    if (!isTRUE(autoTrack$on))
        return(FALSE)
    ## Don't repeat the work an explicit call to track.sync()
    ## BUT, expr in a callback can be an invalid object and cause
    ## a crash when it is accessed, so don't touch it until R is fixed!
    if (FALSE)
        if (is.call(expr) && as.character(expr[[1]]) == "track.sync")
            return(TRUE)
    res <- try(track.sync(envir=data, trackingEnv=trackingEnv, full=NA, master="envir", taskEnd=TRUE), silent=TRUE)
    if (is(try, "try-error"))
        warning("oops: track.sync() had a problem (use track.auto(FALSE, pos=) to turn off): ", res)
    ## Check that the monitor is alive -- this is a mutual back-scratching exercise
    if (!is.element("track.auto.monitor", getTaskCallbackNames()))
        addTaskCallback(track.auto.monitor, name="track.auto.monitor")
    return(TRUE) # to keep this callback active
}
