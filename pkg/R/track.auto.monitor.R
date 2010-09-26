track.auto.monitor <- function(expr, ok, vlaue, visible) {
    ## Monitors existence of task callbacks for track.sync and re-adds them if
    ## they have gone missing (they do seem to go missing occasionally -- hard
    ## to diagnose, seems to be associated with file system problems.)
    ## Try to be fast -- only look at environments on the search path that
    ## are candidates for tracking (i.e., not packages).
    envs <- search()
    callback.names <- getTaskCallbackNames()
    envs.look <- grep("^(package:|pkgcode:|Autoloads$)", envs, invert=TRUE)
    for (i in envs.look) {
        if (env.is.tracked(i)) {
            auto.on <- mget(".trackAuto", ifnotfound=list(list(on=FALSE)), envir=getTrackingEnv(as.environment(i)))[[1]]$on
            if (auto.on) {
                callback.name <- paste("track.auto:", envname(as.environment(i)), sep="")
                if (!is.element(callback.name, callback.names)) {
                    cat("Task callback", callback.name, "seems to have disappeared; reinstating...\n")
                    try(track.auto(TRUE, pos=i))
                }
            }
        }
    }
    TRUE
}
