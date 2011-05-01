track.auto <- function(auto=NULL, pos=1, envir=as.environment(pos)) {
    trackingEnv <- getTrackingEnv(envir)
    current <- mget(".trackAuto", ifnotfound=list(list(on=FALSE, last=-1)), envir=trackingEnv)[[1]]
    callback.name <- "track.auto"
    haveCallback <- is.element(callback.name, getTaskCallbackNames())
    if (is.null(auto)) {
        if (current$on && !haveCallback)
            warning("should be auto-tracking, but don't have a task callback with name '",
                    callback.name, "' (use track.auto(TRUE) to fix)")
        else if (!current$on && haveCallback)
            warning("shouldn't be auto-tracking, but do have a task callback with name '",
                    callback.name, "' (use track.auto(FALSE) to remove)")
        return(current$on)
    }
    if (!is.logical(auto))
        stop("auto must be NULL or logical")
    if (!auto && current$on) {
        remove(list=".trackAuto", envir=trackingEnv)[[1]]
        if (haveCallback)
            removeTaskCallback(callback.name)
    } else if (auto) {
        if (!haveCallback)
            addTaskCallback(track.sync.callback, data=NULL, name=callback.name)
        if (!current$on)
            assign(".trackAuto", list(on=TRUE, last=-1), envir=trackingEnv)
    }
    return(invisible(auto))
}
