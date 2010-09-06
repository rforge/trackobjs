track.auto <- function(auto=NULL, pos=1, envir=as.environment(pos)) {
    trackingEnv <- getTrackingEnv(envir)
    current <- mget(".trackAuto", ifnotfound=list(list(on=FALSE, last=-1)), envir=trackingEnv)[[1]]
    track.name <- paste("track.auto:", envname(envir), sep="")
    haveCallback <- is.element(track.name, getTaskCallbackNames())
    if (is.null(auto)) {
        if (current$on && !haveCallback)
            warning("should be auto-tracking, but don't have a task callback with name '",
                    track.name, "' (use track.auto(TRUE) to fix)")
        else if (!current$on && haveCallback)
            warning("shouldn't be auto-tracking, but do have a task callback with name '",
                    track.name, "' (use track.auto(FALSE) to remove)")
        return(current$on)
    }
    if (!is.logical(auto))
        stop("auto must be NULL or logical")
    if (!auto && current$on) {
        remove(list=".trackAuto", envir=trackingEnv)[[1]]
        if (haveCallback)
            removeTaskCallback(track.name)
    } else if (auto) {
        if (!haveCallback)
            addTaskCallback(track.sync.callback, data=envir, name=track.name)
        if (!current$on)
            assign(".trackAuto", list(on=TRUE, last=-1), envir=trackingEnv)
    }
    return(invisible(auto))
}
