track.copy <- function(from, to=1, list=NULL, pattern=NULL, glob=NULL, delete=FALSE, clobber=FALSE, verbose=TRUE) {
    if (!is.numeric(from) && !is.character(from))
        stop("only implemented for numeric or char values for 'from'")
    if (!is.numeric(to) && !is.character(to))
        stop("only implemented for numeric or char values for 'to'")
    trackingEnv.to <- getTrackingEnv(as.environment(to))
    trackingEnv.from <- getTrackingEnv(as.environment(from))
    opt.to <- track.options(trackingEnv=trackingEnv.to)
    opt.from <- track.options(trackingEnv=trackingEnv.from)
    if (opt.to$readonly)
        stop("cannot copy into a readonly tracking env '", to, "'")
    if (opt.from$readonly && delete)
        stop("cannot copy & delete from readonly tracking env '", from, "'")
    all.objs.from <- ls(pos=from, all=TRUE)
    all.objs.from <- all.objs.from[!isReservedName(all.objs.from)]
    all.objs.from <- setdiff(all.objs.from, c(".Last", ".Last.sys"))
    fileMap.from <- getFileMapObj(trackingEnv.from)
    # limit to just tracked objs
    all.objs.from <- intersect(all.objs.from, names(fileMap.from))
    if (is.null(pattern) && is.null(glob) && is.null(list))
        list <- all.objs.from
    if (!is.null(pattern))
        list <- unique(c(list, grep(pattern, all.objs.from, value=TRUE)))
    if (!is.null(glob))
        list <- unique(c(list, grep(glob2rx(glob), all.objs.from, value=TRUE)))
    if (!is.null(list)) {
        if (any(!(list %in% all.objs.from))) {
            warning("objects not present in 'from':", paste(setdiff(list, all.objs.from), collapse=", "))
            list <- setdiff(list, all.objs.from)
        }
    }
    if (length(list)==0) {
        cat("nothing to copy\n")
        return(invisible(list))
    }
    fileMap.to <- getFileMapObj(trackingEnv.to)
    all.objs.to <- ls(pos=to, all=TRUE)
    all.objs.to <- all.objs.to[!isReservedName(all.objs.to)]
    # limit to just tracked objs
    all.objs.to <- intersect(all.objs.to, names(fileMap.to))
    if (!clobber && any(list %in% all.objs.to))
        stop("some objects to be copied already exist in 'to': ", paste(intersect(list, all.objs.to), collapse=", "))
    objSmy.to <- getObjSummary(trackingEnv.to)
    objSmy.from <- getObjSummary(trackingEnv.from)
    dir.to <- getTrackingDir(trackingEnv.to)
    dir.from <- getTrackingDir(trackingEnv.from)
    for (objName in list) {
        if (verbose)
            cat(if (delete) "Moving" else "Copying", " '", objName, "'...\n", sep="")
        fileMap.to.changed <- FALSE
        fileMap.from.i <- match(objName, names(fileMap.from))
        file.from <- fileMap.from[fileMap.from.i]
        if (is.na(file.from)) {
            warning("file for obj '", objName, "' not found...")
            next
        }
        if (is.na(file.to <- fileMap.to[match(objName, names(fileMap.to))])) {
            file.to <- makeObjFileName(objName, fileMap.to)
            fileMap.to[objName] <- file.to
            fileMap.to.Changed <- TRUE
        }
        file.to.abs <- file.path(dir.to, paste(file.to, opt.to$RDataSuffix, sep="."))
        file.from.abs <- file.path(dir.from, paste(file.from, opt.from$RDataSuffix, sep="."))
        if (file.exists(file.to.abs) && !clobber)
            stop("file '", file.to.abs, "' for obj '", objName, "' already exists and clobber=F")
        # move/copy the file
        deleteThis <- FALSE
        if (delete) {
            deleteThis <- TRUE
            ok <- file.rename(file.from.abs, file.to.abs)
            if (!ok) {
                ok <- file.copy(file.from.abs, file.to.abs, overwrite=TRUE)
                if (!ok)
                    stop("could not copy file '", file.to.abs, "' for obj '", objName, "'")
                ok <- file.remove(file.from.abs)
                if (!ok) {
                    warning("could not remove file '", file.from.abs, "' for obj '", objName, "'")
                    deleteThis <- FALSE
                }
            }
        } else {
            ok <- file.copy(file.from.abs, file.to.abs, overwrite=TRUE)
            if (!ok)
                stop("could not copy file '", file.to.abs, "' for obj '", objName, "'")
        }
        #  update & write objSmy.to & write fileMap.to
        if (objName %in% rownames(objSmy.from)) {
            smyRow <- objSmy.from[objName, ]
            if (deleteThis)
                objSmy.from <- objSmy.from[-match(objName, rownames(objSmy.from)), , drop=FALSE]
        } else {
            # shouldn't happen...
            smyRow <- summaryRow(objName, NULL, obj=NULL, file=NULL, change=NULL, time=NULL)
        }
        if (objName %in% rownames(objSmy.to)) {
            objSmy.to[objName, ] <- smyRow
        } else {
            objSmy.to <- rbind(objSmy.to, smyRow)
        }
        assign.res <- try(assign(".trackingSummary", objSmy.to, envir=trackingEnv.to), silent=TRUE)
        if (is(assign.res, "try-error")) {
            stop("unable to assign .trackingSummary back to tracking env on ",
                    envname(trackingEnv.to), ": ", assign.res)
        } else {
            assign(".trackingSummaryChanged", TRUE, envir=trackingEnv.to)
            file <- file.path(getDataDir(dir.to), paste(".trackingSummary", opt.to$RDataSuffix, sep="."))
            save.res <- try(save(list=".trackingSummary", file=file, envir=trackingEnv.to), silent=TRUE)
            if (is(save.res, "try-error"))
                stop("unable to save .trackingSummary to ", dir.to)
            else
                assign(".trackingSummaryChanged", FALSE, envir=trackingEnv.to)
        }
        writeFileMapFile(fileMap.to, trackingEnv=trackingEnv.to, dataDir=getDataDir(dir.to))
        # remove any cached object in the 'to' envir
        if (exists(objName, envir=trackingEnv.to, inherits=FALSE))
            remove(list=objName, envir=trackingEnv.to)
        if (exists(objName, envir=as.environment(to), inherits=FALSE))
            remove(list=objName, pos=to)
        # create an active binding for the 'to' variable
        f <- substitute(function(v) {
            if (missing(v))
                getTrackedVar(x, envir)
            else
                setTrackedVar(x, v, envir)
        }, list(x=objName, envir=trackingEnv.to))
        mode(f) <- "function"
        environment(f) <- parent.env(environment(f))
        makeActiveBinding(objName, env=as.environment(to), fun=f)
        # update fileMap.from & objSmy.from
        if (deleteThis) {
            assign.res <- try(assign(".trackingSummary", objSmy.from, envir=trackingEnv.from), silent=TRUE)
            if (is(assign.res, "try-error")) {
                stop("unable to assign .trackingSummary back to tracking env on ",
                        envname(trackingEnv.from), ": ", assign.res)
            } else {
                assign(".trackingSummaryChanged", TRUE, envir=trackingEnv.from)
                file <- file.path(getDataDir(dir.from), paste(".trackingSummary", opt.from$RDataSuffix, sep="."))
                save.res <- try(save(list=".trackingSummary", file=file, envir=trackingEnv.from), silent=TRUE)
                if (is(save.res, "try-error"))
                    stop("unable to save .trackingSummary to ", dir.from)
                else
                    assign(".trackingSummaryChanged", FALSE, envir=trackingEnv.from)
            }
            writeFileMapFile(fileMap.from, trackingEnv=trackingEnv.from, dataDir=getDataDir(dir.from))
            if (exists(objName, where=from, inherits=FALSE))
                remove(list=objName, pos=from)
            if (exists(objName, envir=trackingEnv.from, inherits=FALSE))
                remove(list=objName, envir=trackingEnv.from)
        }
    }
    return(invisible(list))
}

track.move <- function(from, to=1, list=NULL, pattern=NULL, glob=NULL, delete=TRUE, clobber=FALSE, verbose=TRUE)
    track.copy(from=from, to=to, list=list, pattern=pattern, glob=glob, delete=delete, clobber=clobber, verbose=verbose)
