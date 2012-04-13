track.rescan <- function(pos=1, envir=as.environment(pos), discardMissing=FALSE,
                         forgetModified=FALSE, level=c("high", "low"), dryRun=FALSE) {
    ## Rescan the tracking dir, so that if anything has changed there,
    ## the current variables on file will be used instead of any cached
    ## in memory.
    ## If we have some modified variables cached in memory but not saved
    ## to disk, this function will stop with an error unless
    ## forgetModified==TRUE.
    ## Variables that have disappeared from the tracking dir (outside of
    ## this session) will disappear from visibility, and variables added
    ## to the tracking dir will become available.
    level <- match.arg(level)
    if (length(pos)>1) {
        if (!missing(envir))
            stop("cannot supply both envir and pos with length > 1")
        for (p in pos)
            track.rescan(p, discardMissing=discardMissing, forgetModified=forgetModified, level=level, dryRun=dryRun)
        return(invisible(NULL))
    }
    unsaved <- track.unsaved(envir=envir)
    if (!forgetModified && length(unsaved))
        stop("env ", envname(envir), " has unsaved variables: ",
             paste("'", unsaved[seq(len=min(3, length(unsaved)))], "'", sep="", collapse=", "),
             if (length(unsaved) > 3) " ...", " (supply forgetModified=TRUE to lose these changes)")
    if (length(unsaved)) {
        if (dryRun)
            cat("track.rescan(dryRun): would forget unsaved variables: ", paste(unsaved, collapse=", "), "\n", sep="")
        else
            track.forget(list=unsaved, envir=envir)
    }
    dir <- find.relative.path(getwd(), track.dir(envir=envir))
    opt <- track.options(envir=envir)
    verbose <- dryRun || opt$debug > 0
    if (level=="high") {
        # High-level rescan is stop (forget everything) and re-start.
        # Check that there won't be any obvious problems re-starting -- check that
        # no variables are masked, etc.  If new variables have been added, these might
        # prevent re-starting if there are name conflicts, but there's not much we can do
        # about that except try and fail.
        status <- track.status(envir=envir, tracked=TRUE, all.names=TRUE)
        if (any(is.element(status$status, c("untrackable", "masked"))))
            stop("will not be able to reattach tracking env because some vars are untrackable or masked (look at output of track.status(envir, tracked=TRUE))")
        envName <- environmentName(envir)
        ## We don't actually detach the environment, so we don't need to
        ## worry about the pos, but work it out in case we decide to code
        ## that in the future.
        if (envName == "R_GlobalEnv")
            pos <- 1
        else
            pos <- match(envName, search())
        if (verbose & dryRun)
            cat("track.rescan: stopping and restart tracking on ", envName, "\n", sep="")
        if (!dryRun) {
            track.stop(envir=envir, detach=FALSE, verbose=TRUE)
            track.start(dir=dir, envir=envir, create=FALSE, verbose=TRUE,
                        discardMissing=discardMissing,
                        readonly=opt$readonly, lockEnv=environmentIsLocked(envir))
        }
        return(invisible(NULL))
    } else {
        if (environmentIsLocked(envir))
            if (dryRun)
                warning("can't actually make changes to this tracked env because it is locked -- can only do track.rescan(level='high')")
            else
                stop("can't actually make changes to this tracked env because it is locked -- do track.rescan(level='high') instead")
        trackingEnv <- getTrackingEnv(envir)
        dir <- getTrackingDir(trackingEnv)
        fileMap <- getFileMapObj(trackingEnv)
        dataDir <- getDataDir(dir)
        all.objs <- ls(envir=envir, all.names=TRUE)
        ## The use case here is that the tracking db on disk has changed
        ## (and the tracking env is readonly), so need to delete/add active bindings
        ## as appropriate.  Here, just delete unneeded active bindings, and let the
        ## code further down take care of adding the new ones.
        fileMapEnv <- fileMap
        fileMap <- readFileMapFile(trackingEnv, getTrackingDir(trackingEnv), assignObj=!dryRun)
        objSummaryEnv <- getObjSummary(trackingEnv, opt=opt)
        objSummaryPath <- file.path(dataDir, paste(".trackingSummary.", opt$RDataSuffix, sep=""))
        ## Read and save the summary from file.
        tmpenv <- new.env(parent=emptyenv())
        if (!file.exists(objSummaryPath))
            stop("file storing object summary doesn't exist: ", objSummaryPath)
        load.res <- try(load(objSummaryPath, envir=tmpenv), silent=TRUE)
        if (is(load.res, "try-error"))
            stop(objSummaryPath, " cannot be loaded -- for recovery see ?track.rebuild (",
                 as.character(load.res), ")")
        if (length(load.res)!=1 || load.res != ".trackingSummary")
            stop(objSummaryPath, " does not contain just '.trackingSummary' -- for recovery see ?track.rebuild")
        ## .trackingSummary has to exist because we just loaded it
        objSummary <- getObjSummary(tmpenv, opt=opt)
        if (!is.data.frame(objSummary))
            stop("'.trackingSummary' from ", objSummaryPath, " is not a data.frame -- see ?track.rebuild")
        ## Use some info from exisiting summary to update the summary read
        ## from file (to retain access counts, etc.)
        retained <- intersect(rownames(objSummary), rownames(objSummaryEnv))
        ## Might want to think about what to do with non-zero SA & SW just read
        ## from the file -- these might reflect another session currently
        ## attached to this DB -- currently ignore those here.
        if (nrow(objSummary))
            objSummary[, c("SA", "SW")] <- 0
        if (length(retained)) {
            if (verbose)
                cat("updating re-read object summary with session data for vars: ",
                    paste(retained, collapse=", "), "\n", sep="")
            if (!dryRun)
                objSummary[retained, c("SA", "SW")] <- objSummaryEnv[retained, c("SA", "SW")]
        }
        ## Delete unneeded active bindings and cached objects
        unneeded <- setdiff(names(fileMapEnv), names(fileMap))
        if (length(unneeded)) {
            unneeded.bindings <- unneeded[sapply(unneeded, exists, envir=envir, inherits=FALSE)]
            unneeded.cached <- unneeded[sapply(unneeded, exists, envir=trackingEnv, inherits=FALSE)]
            if (verbose) {
                if (length(unneeded.bindings))
                    cat("track.rescan: removing unneeded bindings: ", paste(unneeded.bindings, collapse=", "), "\n", sep="")
                if (length(unneeded.cached))
                    cat("track.rescan: removing unneeded cached objects: ", paste(unneeded.cached, collapse=", "), "\n", sep="")
            }
            if (!dryRun) {
                if (length(unneeded.bindings))
                    remove(list=unneeded.bindings, envir=envir)
                if (length(unneeded.cached))
                    remove(list=unneeded.cached, envir=trackingEnv)
            }
        }
        ## Remove all cached objects for variables we still have (because
        ## they might have changed on disk)
        ## Could try to be smart about this here -- would need to look at
        ## modification times on files and in the env copy of objectSummary
        ## to make sure we removed all cached objects where the file might
        ## have changed.
        if (length(fileMap)) {
            cached <- names(fileMap)[sapply(names(fileMap), exists, envir=trackingEnv, inherits=FALSE)]
            if (length(cached)) {
                if (verbose)
                    cat("track.sync: removing cached variables: ", paste(cached, collapse=", "), "\n", sep="")
                if (!dryRun)
                    remove(list=cached, envir=envir)
            }
        }
        #   action=retrack, master=file:  remove current obj from envir, create active binding
        #   action=restore, master=file:  create active binding
        new.vars <- setdiff(names(fileMap), all.objs)
        if (length(new.vars)) {
            if (verbose)
                cat("track.rescanc: creating active bindings for ", length(new.vars), " new variables: ", paste(new.vars, collapse=", "), "\n", sep="")
            if (!dryRun) for (objName in new.vars) {
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
                ##     binding can't find functions from track
                ##   * globalenv() doesn't work because the function in the
                ##     binding can't find non-exported functions from track
                ##   * parent.env(environment(f)) works!
                environment(f) <- parent.env(environment(f))
                makeActiveBinding(objName, env=envir, fun=f)
            }
        }
        if (!dryRun) {
            assign(".trackingSummary", objSummary, envir=trackingEnv)
            assign(".trackingFileMap", fileMap, envir=trackingEnv)
        }
    }
    invisible(NULL)
}
