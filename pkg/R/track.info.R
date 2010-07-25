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
    if (length(i <- find(".Last.sys")) > 1 && !isTRUE(options("global.track.options")$inhibit.Last))
        if (i != find("track.start")[1])
            warning("There are more than one .Last.sys() functions on the search path -- the one from trackObjs will is masked and will not run.  This may affect the saving of tracked environments.")
        else
            warning("There are more than one .Last.sys() functions on the search path -- the one from trackObjs masks others and they will not run.")
    return(cbind(res, data.frame(row.names=NULL, do.call("rbind", lapply(env.list,
           function(e) c(unlist(track.options(envir=e, c("readonly", "cache"))),
                         auto=mget(".trackAuto", ifnotfound=list(list(on=FALSE)),
                                   envir=getTrackingEnv(e))[[1]]$on,
                         dir=track.dir(envir=e)
                         ))))))
}
