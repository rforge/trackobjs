track.auto <- function(auto=NULL, pos=1, envir=as.environment(pos)) {
    trackingEnv <- getTrackingEnv(envir)
    current <- mget(".trackAuto", ifnotfound=list(FALSE), envir=trackingEnv)[[1]]
    track.name <- paste("track.auto:", envname(envir), sep="")
    if (is.null(auto)) {
        if (current && !is.element(track.name, getTaskCallbackNames()))
            warning("should be auto-tracking, but don't have a task callback with name '", track.name, "'")
        else if (!current && is.element(track.name, getTaskCallbackNames()))
            warning("shouldn't be auto-tracking, but do have a task callback with name '", track.name, "'")
        return(current)
    }
    if (!is.logical(auto))
        stop("auto must be NULL or logical")
    if (!auto && current) {
        remove(list=".trackAuto", envir=trackingEnv)[[1]]
        if (is.element(track.name, getTaskCallbackNames()))
            removeTaskCallback(track.name)
    } else if (auto && !current) {
        addTaskCallback(track.sync.callback, data=envir, name=track.name)
        assign(".trackAuto", TRUE, envir=trackingEnv)[[1]]
    }
    return(invisible(auto))
}
