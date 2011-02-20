track.ff <- function(expr, pos=1, envir=as.environment(pos)) {
    ## simplified from track(), just deal with:
    ##    (1) track.ff(x) where x is an ordinary or ff objects
    ## or (2) track.ff(x <- y) where y is an object or call to ff creator
    ##
    trackingEnv <- getTrackingEnv(envir)
    opt <- track.options(trackingEnv=trackingEnv)
    if (opt$readonly)
        stop(envname(trackingEnv), " is readonly")
    ## evaluate expr if necessary, and convert to list
    qexpr <- substitute(expr)
    if (is.name(qexpr)) {
        objName <- as.character(qexpr)
    } else if (mode(qexpr)=="call" && class(qexpr)=="<-") {
        if (!is.name(qexpr[[2]]))
            stop("LHS must be a simple var name in track(LHS <- RHS)")
        ## need to evaluate and assign
        objName <- as.character(qexpr[[2]])

        ****
        fileMap <- getFileMapObj(trackingEnv)
        fileMapChanged <- FALSE
        isNew <- FALSE
        if (is.na(file <- fileMap[match(objName, names(fileMap))])) {
            file <- makeObjFileName(objName, fileMap)
            fileMap[objName] <- file
            fileMapChanged <- TRUE
            isNew <- TRUE
        }
        if (fileMapChanged) {
            ##  always write a changed file map back out to disk
            writeFileMapFile(fileMap, trackingEnv=trackingEnv, dataDir=getDataDir(dir))
        }
        ****

        objval <- eval(qexpr[[3]], envir=parent.frame(), enclos=parent.frame())
        haveVal <- TRUE
        ## check if the var already exists
        if (exists(objName, envir=envir, inherits=FALSE)) {
            if (objIsTracked(objName, envir, trackingEnv)) {
                setTrackedVar(objName, objval, trackingEnv, opt)
                ## will detect that this var is already tracked below, and won't
                ## do setTrackedVar() again
            } else {
                remove(list=objName, envir=envir)
            }
        }
    } else {
        stop("argument to track() must be an unquoted variable or an assignment")
    }


    list <- c(objName)
    if (length(list)) {
        alreadyTracked <- objIsTracked(list, envir, trackingEnv)
        if (any(alreadyTracked)) {
            # this is a pretty useless warning, so skip it
            # warning("the following objects are already tracked: ",
            #          paste("'", list[which(alreadyTracked)[seq(len=min(3,sum(alreadyTracked)))]], "'", sep="", collapse=", "),
            #                if (sum(alreadyTracked) > 3) ", ...")
            list <- list[!alreadyTracked]
        }
    }
    i <- which(isReservedName(list))
    if (length(i)) {
        warning("cannot track ", length(i), " variables (these are used in implementing tracking or are illegal variable names): ",
                paste("'", list[i], "'", sep="", collapse=", "))
        list <- list[-i]
    }
    all.objs <- .Internal(ls(envir, TRUE))
    for (objName in list) {
        ## Doesn't matter if it already exists....
        ## if (exists(objName, trackingEnv))
        ##     stop("'", objName, "' already exists in trackingEnv ", envname(trackingEnv))
        if (haveVal) {
            ## Nothing to do here -- already have the val in objval
        } else if (!is.element(objName, all.objs)) {
            objval <- NULL
        } else {
            if (bindingIsActive(objName, envir)) {
                warning("cannot track '", objName, "' because it is an active binding")
                next
            }
            objval <- get(objName, envir=envir, inherits=FALSE)
            remove(list=objName, envir=envir)
            ## robustness danger point: can have objval in here, but
            ## assigned in no environment -- might be better to remove
            ## it only after setTrackedVar() has succeeded.  However,
            ## setTrackedVar cannot work if it is still here...
        }
        ## robustness: what to do if the assign inside setTrackedVar fails?
        setTrackedVar(objName, objval, trackingEnv, opt)
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
    return(invisible(list))
}
