track.ff <- function(expr, pos=1, envir=as.environment(pos)) {
    ## Simplified from track(), just deal with:
    ##    (1) track.ff(x) where x is an ordinary or ff objects
    ## or (2) track.ff(x <- y) where y is an object or call to ff creator
    trackingEnv <- track:::getTrackingEnv(envir)
    opt <- track.options(trackingEnv=trackingEnv)
    if (opt$readonly)
        stop(track:::envname(trackingEnv), " is readonly")
    ## Evaluate expr if necessary, and convert to list
    qexpr <- substitute(expr)
    if (is.name(qexpr) || is.character(qexpr)) {
        objName <- as.character(qexpr)
        objValExpr <- as.name(objName)
    } else if (mode(qexpr)=="call" && class(qexpr)=="<-") {
        if (!is.name(qexpr[[2]]))
            stop("LHS must be a simple var name in track.ff(LHS <- RHS)")
        objName <- as.character(qexpr[[2]])
        objValExpr <- qexpr[[3]]
    } else {
        stop("argument to track() must be an unquoted variable or an assignment")
    }
    ## Early exit before modifying anything if call is illegal
    haveObjVal <- FALSE
    if (is.call(objValExpr) && is.element(as.character(objValExpr[[1]]), c("ff", "as.ff"))
        && is.element("filename", names(objValExpr)))
        stop("cannot supply filename= to ff() or as.ff() in track.ff(var <- ff())")
    if (is.name(objValExpr)) {
        if (mode(qexpr)=="call" && class(qexpr)=="<-")
            objVal <- eval(objValExpr)
        else
            objVal <- get(as.character(objValExpr), envir=envir, inherit=FALSE)
        haveObjVal <- TRUE
        if (is.ff(objVal))
            stop("nyi: tracking an existing ff object")
        if (!is.vector(objVal) && !is.array(objVal) && !is.data.frame(objVal))
            stop("object to be converted to ff must be vector, matrix, array or dataframe")
    } else if (is.call(objValExpr)) {
        if (!is.element(as.character(objValExpr[[1]]), c("ff", "as.ff"))) {
            objVal <- eval(objValExpr)
            if (is.ff(objVal))
                stop("nyi: tracking an existing ff object")
            haveObjVal <- TRUE
        }
    } else {
        stop("RHS must be a call or a name in track.ff(LHS <- RHS)")
    }
    ## Start modifying things
    ## if the object is not already tracked, track it with value NULL
    if (!file.exists(file.path(track.datadir(envir=envir), "ff"))
        && !dir.create(file.path(track.datadir(envir=envir), "ff")))
        stop("failed to create directory '", file.path(track.datadir(envir=envir), "ff"), "'")
    if (!track:::objIsTracked(objName, envir, trackingEnv)) {
        track:::setTrackedVar(objName, NULL, trackingEnv, opt)
        f <- substitute(function(v) {
            if (missing(v))
                track:::getTrackedVar(x, envir)
            else
                track:::setTrackedVar(x, v, envir)
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
    filename.ff <- file.path(".", track.datadir(envir=envir), "ff",
                             paste(track.filename(list=objName, envir=envir), ".ff", sep=""))
    if (file.exists(filename.ff))
        file.remove(filename.ff)
    if (is.call(objValExpr) && is.element(as.character(objValExpr[[1]]), c("ff", "as.ff"))) {
        if (haveObjVal)
            stop("internal logical error - should not already have objVal")
        ## add a filename argument to the ff call
        i <- length(objValExpr)
        objValExpr[i+1] <- filename.ff
        names(objValExpr)[i+1] <- "filename"
        ## add a finalizer='delete' argument to the ff call
        i <- length(objValExpr)
        objValExpr[i+1] <- "delete"
        names(objValExpr)[i+1] <- "finalizer"
        ## add a finonexit=FALSE argument to the ff call
        i <- length(objValExpr)
        objValExpr[i+1] <- FALSE
        names(objValExpr)[i+1] <- "finonexit"
        objVal <- eval(objValExpr)
    } else {
        if (!haveObjVal)
            stop("internal logical error - should already have objVal")
        objVal <- as.ff(objVal, filename=filename.ff, finalizer="delete", finonexit=FALSE)
    }
    track:::setTrackedVar(objName, objVal, trackingEnv, opt)
    return(invisible(objVal))
}

track.ff.rm <- function(expr, pos=1, envir=as.environment(pos)) {
    ## Functionally the same as track.remove(expr, ...)
    trackingEnv <- track:::getTrackingEnv(envir)
    opt <- track.options(trackingEnv=trackingEnv)
    if (opt$readonly)
        stop(track:::envname(trackingEnv), " is readonly")
    ## Evaluate expr if necessary, and convert to list
    qexpr <- substitute(expr)
    if (is.name(qexpr) || is.character(qexpr)) {
        objName <- as.character(qexpr)
        ## objValExpr <- as.name(objName)
        ## objVal <- get(as.character(objValExpr), envir=envir, inherit=FALSE)
        ## delete(objVal)
        track.remove(list=objName, envir=envir)
    } else {
        stop("argument to track.ff.rm() must be a quoted or unquoted variable name")
    }
    return(invisible(objName))
}

track.ff.remove <- track.ff.rm
track.ff.delete <- track.ff.rm

track.ff.filename <- function(expr, pos=1, envir=as.environment(pos), relative=TRUE) {
    trackingEnv <- track:::getTrackingEnv(envir)
    opt <- track.options(trackingEnv=trackingEnv)
    ## Evaluate expr if necessary, and convert to list
    qexpr <- substitute(expr)
    if (is.name(qexpr) || is.character(qexpr)) {
        objName <- as.character(qexpr)
        objVal <- get(objName, envir=envir)

    } else {
        stop("argument to track.ff.filename() must be a quoted or unquoted variable name")
    }
    if (relative)
        return(track:::find.relative.path(getwd(), filename(objVal)))
    else
        return(filename(objVal))
}
