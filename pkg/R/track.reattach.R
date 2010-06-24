track.restart <- function(pos=1, envir=as.environment(pos), forget.modified=FALSE) {
    ## Rescan the tracking dir, so that if anything has changed there,
    ## the current variables on file will be used instead of any cached
    ## in memory.
    ## If we have some modified variables cached in memory but not saved
    ## to disk, this function will stop with an error unless
    ## forget.modified==TRUE.
    ## Variables that have disappeared from the tracking dir will disappear
    ## from visibility, and variables added to the tracking dir will become
    ## available.
    unsaved <- track.unsaved(envir=envir)
    if (!forget.modified && length(unsaved))
        stop("env ", envname(envir), " has unsaved variables: ",
             paste("'", unsaved[seq(len=min(3, length(unsaved)))], "'", sep="", collapse=", "),
             if (length(unsaved) > 3) " ...", " (supply forget.modified=TRUE to lose these changes)")
    if (length(unsaved))
        track.forget(list=unsaved, envir=envir)
    dir <- track.dir(envir=envir)
    # Could possibly code this more efficiently, but detach/attach is quick and easy to code.
    # Check that there won't be any problems reattaching -- check that
    # no variables are masked, etc.
    status <- track.status(envir=envir, tracked=TRUE)
    if (any(is.element(status$status, c("untrackable", "masked"))))
        stop("will not be able to reattach tracking env because some vars are untrackable or masked (look at output of track.status(envir, tracked=TRUE))")
    track.stop(envir=envir)
    track.start(dir=dir, envir=envir, create=FALSE)
    return(invisible(NULL))
}
