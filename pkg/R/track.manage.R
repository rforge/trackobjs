track.remove <- function(expr, pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL, all=FALSE, force=FALSE)
    trackedVarOp(if (!missing(expr)) substitute(expr), envir=envir, list=list, pattern=pattern, glob=glob, all=all, op="remove", who="track.remove()", force=force)

track.save <- function(expr, pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL, all=missing(expr) && missing(list) && missing(pattern) && missing(glob))
    trackedVarOp(if (!missing(expr)) substitute(expr), envir=envir, list=list, pattern=pattern, glob=glob, all=all, op="save", resave=FALSE, who="track.save()")

track.resave <- function(expr, pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL, all=missing(expr) && missing(list) && missing(pattern) && missing(glob))
    trackedVarOp(if (!missing(expr)) substitute(expr), envir=envir, list=list, pattern=pattern, glob=glob, all=all, op="save", resave=TRUE, who="track.resave()")

track.flush <- function(expr, pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL, all=missing(expr) && missing(list) && missing(pattern) && missing(glob))
    trackedVarOp(if (!missing(expr)) substitute(expr), envir=envir, list=list, pattern=pattern, glob=glob, all=all, op="flush", who="track.flush()")

track.forget <- function(expr, pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL, all=FALSE)
    trackedVarOp(if (!missing(expr)) substitute(expr), envir=envir, list=list, pattern=pattern, glob=glob, all=all, op="forget", who="track.forget()")

untrack <- function(expr, pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL, all=FALSE, keep.in.db=FALSE)
    trackedVarOp(if (!missing(expr)) substitute(expr), envir=envir, list=list, pattern=pattern, glob=glob, all=all, op=if (keep.in.db) "lift" else "untrack", who="untrack()")

## perform some operation on tracked variables
trackedVarOp <- function(qexpr, pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL, all=FALSE,
                         op=c("save", "flush", "forget", "remove", "untrack", "lift", "orphan"),
                         resave=FALSE, force=FALSE, who="?") {
    op <- match.arg(op)
    trackingEnv <- getTrackingEnv(envir)
    opt <- track.options(trackingEnv=trackingEnv)
    dir <- getTrackingDir(trackingEnv)
    dataDir <- getDataDir(dir)
    fileMap <- getFileMapObj(trackingEnv)
    unsaved <- getUnsavedObj(trackingEnv, NULL)
    objSummary <- getObjSummary(trackingEnv)
    if (!is.null(qexpr)) {
        if (is.name(qexpr)) {
            objname <- as.character(qexpr)
        } else {
            stop("expr argument to ", who, " must be an unquoted variable")
        }
        list <- c(objname, list)
    }
    if (!is.null(pattern) || !is.null(glob) || all) {
        if (!is.null(list))
            stop("cannot use expr= or list= at the same time as pattern=, glob=, or all=TRUE")
        list <- track.status(envir=envir, pattern=pattern, glob=glob, file.status=FALSE,
                             what=if (op=="save") "unsaved" else "tracked", tracked=TRUE, all.names=TRUE)
    }
    all.objs <- .Internal(ls(envir, TRUE))
    if (length(list)) {
        isTracked <- objIsTracked(list, envir, trackingEnv, all.objs)
        if (!force && !all(isTracked)) {
            cat("the following objects are not tracked: ",
                paste("'", list[which(!isTracked)[seq(len=min(3,sum(!isTracked)))]], "'", sep="", collapse=", "),
                if (sum(!isTracked) > 3) ", ...",
                "\n", sep="")
            list <- list[!isTracked]
        }
    }
    i <- which(isReservedName(list))
    if (length(i)) {
        warning("cannot ", op, " ", length(i), " vars (these are used in implementing tracking): ",
                paste("'", list[i], "'", sep="", collapse=", "))
        list <- list[-i]
    }
    quarantine.dir <- file.path(dataDir, "quarantine")
    needSaveFileMap <- FALSE
    needSaveObjSummary <- mget(".trackingSummaryChanged", envir=trackingEnv, ifnotfound=list(FALSE))[[1]]
    for (objname in list) {
        fileMapChanged <- FALSE
        objSummaryChanged <- FALSE
        # if (!exists(objname, envir=envir, inherits=FALSE)) { # don't use exists() because it ...
        if (!force && !is.element(objname, all.objs)) {
            warning("'", objname, "' does not exist in ", envname(envir))
            next
        }
        if (is.element(op, c("untrack", "lift")) && !force && !bindingIsActive(objname, envir)) {
            warning("cannot ", op, " tracked var '", objname, "' because it is not a properly tracked variable (not an active binding in ", envname(envir), ")")
            next
        }
        fileMapPos <- match(objname, names(fileMap))
        if (is.na(fileMapPos)) {
            warning("cannot ", op, " tracked var '", objname, "' because it is not in the file map in ", envname(envir))
            next
        }
        file <- paste(fileMap[fileMapPos], opt$RDataSuffix, sep=".")
        filePath <- file.path(dataDir, file)
        ##
        ## First task is to modify envir if necessary
        ##
        if (op=="remove") {
            ## remove the variable from envir
            if (is.element(objname, all.objs))
                remove(list=objname, envir=envir)
        } else if (is.element(op, c("untrack", "lift"))) {
            ## fetch the value and assign it in envir
            if (exists(objname, envir=trackingEnv, inherits=FALSE)) {
                objval <- get(objname, envir=trackingEnv, inherits=FALSE)
            } else {
                tmpenv <- new.env(parent=emptyenv())
                load.res <- load(filePath, envir=tmpenv)
                if (length(load.res)<1 || load.res[1] != objname) {
                    warning("file '", filePath, "' did not contain obj '", objname,
                            "' as its first object - moving this file to ", quarantine.dir)
                    if (!file.exists(quarantine.dir))
                        dir.create(quarantine.dir)
                    file.rename(filePath, file.path(dataDir, "quarantine", file))
                } else {
                    if (length(load.res)>1) {
                        warning("ignoring other objects in file '", filePath, "'")
                        remove(list=load.res[-1], envir=tmpenv)
                    }
                    objval <- get(objname, envir=tmpenv, inherits=FALSE)
                    remove(list=objname, envir=tmpenv)
                }
            }
            # need to remove the active binding from envir before saving the ordinary variable
            remove(list=objname, envir=envir)
            assign(objname, objval, envir=envir)
        } else if (!is.element(op, c("save", "flush", "forget"))) {
            stop("what ", op, "???")
        }
        ##
        ## Second task is to remove files and modify trackingEnv if necessary
        ##
        if (is.element(op, c("remove", "untrack"))) {
            fileMap <- fileMap[-fileMapPos]
            fileMapChanged <- TRUE
            if (exists(objname, envir=trackingEnv, inherits=FALSE))
                remove(list=objname, envir=trackingEnv)
            if (file.exists(filePath)) {
                rm.res <- try(file.remove(filePath), silent=TRUE)
                if (is(rm.res, "try-error"))
                    warning("could not remove file for tracked var '", objname, "'")
            }
            i <- match(objname, rownames(objSummary))
            if (!is.na(i)) {
                objSummary <- objSummary[-i,,drop=FALSE]
                objSummaryChanged <- TRUE
            }
        } else if (is.element(op, c("save", "flush", "forget", "lift"))) {
            if (is.element(op, c("flush", "save", "lift")) && exists(objname, envir=trackingEnv, inherits=FALSE)
                && (resave || is.element(objname, unsaved))) {
                save.res <- try(save(list=objname, envir=trackingEnv, file=filePath), silent=TRUE)
                if (is(save.res, "try-error"))
                    stop("could not save '", objname, "' in ", filePath, ": fix file problem and try again")
            }
            if (is.element(op, c("flush", "forget", "lift")) && exists(objname, envir=trackingEnv, inherits=FALSE))
                remove(list=objname, envir=trackingEnv)
        } else {
            stop("what ", op, "???")
        }
        ## every operation we do has the effect of unsetting the 'unsaved' bit
        if (!is.na(i <- match(objname, unsaved))) {
            unsaved <- unsaved[-i]
            assign(".trackingUnsaved", unsaved, envir=trackingEnv)
        }
        if (fileMapChanged) {
            needSaveFileMap <- TRUE
            assign(".trackingFileMap", fileMap, envir=trackingEnv)
        }
        if (objSummaryChanged) {
            needSaveObjSummary <- TRUE
            assign(".trackingSummary", objSummary, envir=trackingEnv)
        }
    }
    save1.res <- save2.res <- NULL
    ## Note that we never write the "unsaved" list out to a file -- it just stays in memory
    if ((needSaveFileMap || resave) && !opt$readonly)
        writeFileMapFile(fileMap, trackingEnv, dataDir, FALSE)
    if ((needSaveObjSummary || resave) && !opt$readonly) {
        assign(".trackingSummaryChanged", TRUE, envir=trackingEnv)
        save2.res <- try(save(list=".trackingSummary", envir=trackingEnv, file=file.path(dataDir, paste(".trackingSummary", opt$RDataSuffix, sep="."))), silent=TRUE)
        if (!is(save2.res, "try-error"))
            assign(".trackingSummaryChanged", FALSE, envir=trackingEnv)
    }
    if (is(save1.res, "try-error") || is(save2.res, "try-error"))
        stop("unable to save some tracking info in ", file.path(dataDir), ": fix problem and run track.resave()")
    return(invisible(list))
}
