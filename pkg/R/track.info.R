track.info <- function(pos=1, envir=as.environment(pos), all=TRUE) {
    if (all) {
        envirs <- search()
        i <- sapply(envirs, function(envir) env.is.tracked(envir=as.environment(envir)))
        env.list <- lapply(as.list(envirs)[i], as.environment)
        names(env.list) <- envirs[i]
        res <- data.frame(row.names=NULL, env.name=envirs[i], pos=seq(len=length(envirs))[i])
    } else {
        if (!is.environment(envir))
            envir <- as.environment(envir)
        if (missing(pos))
            if (environmentName(envir)=="R_GlobalEnv")
                pos <- 1
            else
                pos <- match(environmentName(envir), search())
        res <- data.frame(row.names=NULL, env.name=environmentName(envir), pos=pos)
        env.list <- structure(list(envir), names=environmentName(envir))
    }
    return(cbind(res, data.frame(row.names=NULL, do.call("rbind", lapply(env.list,
           function(e) c(unlist(track.options(envir=e, c("readonly", "cache"))),
                         auto=mget(".trackAuto", ifnotfound=list(list(on=FALSE)),
                                   envir=getTrackingEnv(e))[[1]]$on,
                         dir=track.dir(envir=e)
                         ))))))
}
