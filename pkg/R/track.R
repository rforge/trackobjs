## Design:
##
## There is a trackingEnv variable in the tracked
## environment (e.g., .GlobalEnv)
##
## Tracked vars are active bindings, whose function refers to their
## name and trackingEnv
##
## The trackingEnv has several pieces of metadata:
##   * .trackingSummary
##   * .trackingFileMap (contains all objects)
##   * .trackingUnsaved (contains names of unsaved objects, only used if writeToDisk==FALSE)
##   * .trackOptions (list(writeToDisk=TRUE/FALSE, cache=TRUE/FALSE)) (default TRUE/TRUE)
##   * .trackingDir (absolute path -- don't depend on getwd() -- can be changed
## The first two are stored on disk, and written back out whenever
## they are changed.
## .trackingUnsaved is only stored in memory.
## When the database is initially read, .trackingFileMap and .trackingSummary
## might be created.  Note that object names are stored in RData files,
## so we can recreate these objects if they are missing, and check
## consistency when we read them.
##
## Functions: ( * = to be written)
##   track.start(dir=trackingDir)
##   track.dir()
##   track.stop()
##   track(var) track(var <- value) track(list=...) track(all=T)
##   track.status(): return a list of track env, track dir, and tracked, untracked, and untrackable vars
##   untrack(var) untrack(list=) untrack(all=T)
##   track.remove(var) track.remove(list=) track.remove(all=T) (call this track.remove() ?)
##   track.summary()
##
##   tracked(): return all tracked variables
##   untracked(): return all untracked, but trackable variables
##   untrackable(): return all untrackable variables
##   track.unsaved(): return all tracked but unsaved variables
##   track.orphaned(): return variables that exist in the tracking dir, but have no binding
##   track.masked(): return variables that exist in the tracking dir, but have an ordinary binding
##
##   track.flush(var, list, all): write unsaved variables to disk, and remove from memory
##   track.save(var, list, all, book): write unsaved variables to disk
##   track.forget(var, list, all): delete cached versions without saving to file
##   track.rescan(): reload info from disk (forget all cached vars, might remove some no-longer existing tracked vars)
##   track.rebuild(dir=trackingDir)
##
## Don't need to use delayedAssign if use makeActiveBinding
## vars that have a binding, and no corresponding var in trackingEnv
## means the var must be out on disk.  However, can still use
## the same structure as a ddp from g.data().
##
## database of object data is stored in .trackingSummary
## contains the following columns:
##   name
##   class
##   mode
##   extent
##   length
##   size
##   time: modified
##   time: created
##   time: accessed
##   ES: sessions alive
##   SA: num accesses this session
##   SW: num writes this session
##   PA: total accesses before this session
##   PW: total writes before this session
##
## Also need to have mapping of object names -> file names:
## store this in .trackingFileMap.
## Any names that contain upper case letters or don't conform
## to starting with alphanum and then continuing with alphanum . _
## are given a filename like .XXXX where XXXX is a number.
##
## When reconstructing trackingSummary, use file.info() to get file
## creation, modification and last access times.
##
## Rules for names of RData files:
##    'simple' names satisfy the following conditions:
##       * contain lowercase letters, digits, '.', "_"
##       * start with a lowercase letter
##       * are 55 characters or less
##       * are not one of 'con', 'prn', 'aux', 'nul', 'com1' .. 'com9', 'lpt1' .. 'lpt9'
##    * when an object has a filename that does not begin with a '_',
##      the filename and the object name must be simple and identical
##    * objects that do not have a simple name must be stored in
##      files with names beginning with a '_'

env.is.tracked <- function(pos=1, envir=as.environment(pos)) {
    return(exists(".trackingEnv", envir=envir, inherits=FALSE))
}

getDataDir <- function(trackingDir) {
    ## If we wanted to store saved RData files in a subdirectory called 'data',
    ## we would use this here:
    ## return(file.path(dir, "data"))
    trackingDir
}

tracked.envs <- function(envirs=search()) {
    ## returns environment names, not environments!
    i <- sapply(envirs, function(envir) env.is.tracked(envir=as.environment(envir)))
    envirs[i]
}

track <- function(expr, pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL, exclude=TRUE) {
    trackingEnv <- getTrackingEnv(envir)
    opt <- track.options(trackingEnv=trackingEnv)
    haveVal <- FALSE
    if (!missing(expr)) {
        ## evaluate expr if necessary, and convert to list
        qexpr <- substitute(expr)
        if (is.name(qexpr)) {
            objname <- as.character(qexpr)
        } else if (mode(qexpr)=="call" && class(qexpr)=="<-") {
            if (length(list))
                stop("cannot use both assignment expr and list at the same time in track(LHS <- RHS, list=...)")
            if (!is.name(qexpr[[2]]))
                stop("LHS must be a simple var name in track(LHS <- RHS)")
            ## need to evaluate and assign
            objname <- as.character(qexpr[[2]])
            objval <- eval(qexpr[[3]], envir=parent.frame(), enclos=parent.frame())
            haveVal <- TRUE
            ## check if the var already exists
            if (exists(objname, envir=envir, inherits=FALSE)) {
                if (objIsTracked(objname, envir, trackingEnv)) {
                    setTrackedVar(objname, objval, trackingEnv, opt)
                    ## will detect that this var is already tracked below, and won't
                    ## do setTrackedVar() again
                } else {
                    remove(list=objname, envir=envir)
                }
            }
        } else {
            stop("argument to track() must be an unquoted variable or an assignment")
        }
        list <- c(objname, list)
    } else if (is.null(list)) {
        list <- untracked(envir=envir, pattern=pattern, glob=glob)
        if (isTRUE(exclude))
            exclude <- opt$autoTrackExcludePattern
        if (identical(exclude, FALSE))
            exclude <- NULL
        for (re in exclude)
            list <- grep(re, list, invert=TRUE, value=TRUE)
    }
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
    for (objname in list) {
        ## Doesn't matter if it already exists....
        ## if (exists(objname, trackingEnv))
        ##     stop("'", objname, "' already exists in trackingEnv ", envname(trackingEnv))
        if (haveVal) {
            ## Nothing to do here -- already have the val in objval
        } else if (!is.element(objname, all.objs)) {
            objval <- NULL
        } else {
            if (bindingIsActive(objname, envir)) {
                warning("cannot track '", objname, "' because it is an active binding")
                next
            }
            objval <- get(objname, envir=envir, inherits=FALSE)
            remove(list=objname, envir=envir)
            ## robustness danger point: can have objval in here, but
            ## assigned in no environment -- might be better to remove
            ## it only after setTrackedVar() has succeeded.  However,
            ## setTrackedVar cannot work if it is still here...
        }
        ## robustness: what to do if the assign inside setTrackedVar fails?
        setTrackedVar(objname, objval, trackingEnv, opt)
        f <- substitute(function(v) {
            if (missing(v))
                getTrackedVar(x, envir)
            else
                setTrackedVar(x, v, envir)
        }, list(x=objname, envir=trackingEnv))
        mode(f) <- "function"
        ## Need to replace the environment of f, otherwise it is this
        ## function, which can contain a copy of objval, which can
        ## use up lots of memory!
        ## Need to be careful with the choice of env to set here:
        ##   * emptyenv() doesn't work because then the binding can't find
        ##     any defns
        ##   * baseenv() doesn't work because then the function in the
        ##     binding can't find functions from trackObjs
        ##   * globalenv() doesn't work because the function in the
        ##     binding can't find non-exported functions from trackObjs
        ##   * parent.env(environment(f)) works!
        environment(f) <- parent.env(environment(f))
        makeActiveBinding(objname, env=envir, fun=f)
    }
    return(invisible(list))
}

# Create or update a row for the summary data frame
summaryRow <- function(name, sumRow=NULL, obj=NULL, file=NULL, change=FALSE, times=NULL, accessed=TRUE) {
    tt <- Sys.time()
    new <- FALSE
    if (is.null(sumRow)) {
        new <- TRUE
        sumRow <- data.frame(row.names=name, class="", mode="", extent="",
                             length=length(obj), size=as.double(object.size(obj)),
                             modified=tt, created=tt, accessed=tt,
                             A=as.integer(1), ES=as.integer(1), SA=as.integer(0),
                             SW=as.integer(0), PA=as.integer(0), PW=as.integer(0),
                             stringsAsFactors=FALSE)
        if (!is.null(file) && file.exists(file)
            && !is(info <- try(file.info(file), silent=TRUE), "try-error")) {
            if (length(info$mtime)==1 && !is.na(info$mtime))
                sumRow$modified <- info$mtime
            if (length(info$ctime)==1 && !is.na(info$ctime))
                sumRow$created <- info$ctime
            if (length(info$atime)==1 && !is.na(info$atime))
                sumRow$accessed <- info$atime
        }
    }

    if (change || new) {
        cl <- class(obj)
        # neither the first or last element of the returned value of 'class' is
        # clearly the most useful class to note, so include all classes
        # class of an object returned by 'glm()': "glm" "lm"
        # class of an object returned by Sys.time(): "POSIXt"  "POSIXct"
        sumRow$class <- if (length(cl)==0) "?" else paste(cl, collapse=",")
        sumRow$mode <- mode(obj)
        l <- try(length(obj), silent=TRUE)
        if (is(l, "try-error"))
            sumRow$length <- NA
        else
            sumRow$length <- as.integer(l)
        d <- try(dim(obj), silent=TRUE)
        if (is(d, "try-error"))
            sumRow$extent <- "(error)"
        else if (is.numeric(d))
            sumRow$extent <- paste("[", paste(d, collapse="x"), "]", sep="")
        else
            sumRow$extent <- paste("[", format(sumRow$length), "]", sep="")
        if (is.list(obj))
            sumRow$extent <- paste("[", sumRow$extent, "]", sep="")
        sumRow$size <- object.size(obj)
        if (accessed)
            sumRow$modified <- tt
        sumRow$SW <- sumRow$SW + as.integer(1)
    } else if (accessed) {
        sumRow$accessed <- tt
        sumRow$SA <- sumRow$SA + as.integer(1)
    }
    if (!is.null(times)) {
        sumRow$modified <- times$mtime
        sumRow$created <- times$ctime
        sumRow$accessed <- times$atime
    }

    return(sumRow)
}

isReservedName <- function(objname)
    ## ".trackingEnv" is a reserved name to allow for storing the
    ## tracking env as an object in the tracked environment instead
    ## of an attribute on the tracked environment.
    return((regexpr("[\n\r]", objname) > 0)
           | is.element(objname, c(".trackingEnv", ".trackingDir", ".trackingFileMap", ".trackingUnsaved", ".trackingSummary", ".trackingSummaryChanged", ".trackingOptions", ".trackAuto")))

objIsTracked <- function(objnames, envir, trackingEnv, all.objs=.Internal(ls(envir, TRUE))) {
    if (length(objnames)==0)
        return(logical(0))
    fileMap <- getFileMapObj(trackingEnv)
    return(sapply(objnames, function(objname) {
        ## an object is already tracked if the following 2 conditions are met:
        ##   - it exists as an activing binding in envir
        ##   - there is an entry in the fileMap in the trackingEnv
        ## Don't use exists() because it gets the object
        ## if (!exists(objname, envir=envir, inherits=FALSE))
        if (!is.element(objname, all.objs))
            return(FALSE)
        if (!bindingIsActive(objname, envir))
            return(FALSE)
        if (!is.element(objname, names(fileMap)))
            return(FALSE)
        return(TRUE)
    }))
}

getTrackingDir <- function(trackingEnv) {
    dir <- NULL
    ## avoid partial name matching for attributes...
    dir <- mget(".trackingDir", ifnotfound=list(NULL), envir=trackingEnv)[[1]]
    if (is.null(dir))
        stop("trackingEnv ", envname(trackingEnv), " has no '.trackingDir' variable")
    if (!is.character(dir) || length(dir)!=1)
        stop("variable '.trackingDir' in trackingEnv ", envname(trackingEnv),
             " must be a length one character vector")
    return(dir)
}

setTrackingEnv <- function(trackedEnv, trackingEnv, readonly=FALSE) {
    ## This function should remove the tracking env when trackingEnv=NULL
    if (is.null(trackingEnv) && !readonly) {
        if (exists(".trackingEnv", envir=trackedEnv, inherits=FALSE))
            remove(list=".trackingEnv", envir=trackedEnv)
    } else {
        assign(".trackingEnv", trackingEnv, envir=trackedEnv)
    }
    invisible(NULL)
}

getTrackingEnv <- function(trackedEnv, stop.on.not.tracked = TRUE) {
    env <- mget(".trackingEnv", ifnotfound=list(NULL), envir=trackedEnv)[[1]]
    if (is.null(env))
        if (stop.on.not.tracked)
            stop("env ", envname(trackedEnv), " is not tracked (has no '.trackingEnv' variable)")
        else
            return(NULL)
    if (!is.environment(env))
        stop("variable '.trackingEnv' in env ", envname(trackedEnv),
             " is not an environment")
    return(env)
}

getFileMapObj <- function(trackingEnv) {
    fileMap <- mget(".trackingFileMap", envir=trackingEnv, ifnotfound=list(NULL))[[1]]
    if (is.null(fileMap) || !is.character(fileMap))
        stop("no usable .trackingFileMap object in tracking env ", envname(trackingEnv), " - recommend using track.rebuild()")
    return(fileMap)
}

writeFileMapFile <- function(fileMap, trackingEnv, dataDir, assignObj=TRUE) {
    if (assignObj && is(try(assign(".trackingFileMap", fileMap, envir=trackingEnv)), "try-error"))
        warning("failed to assign '.trackingFileMap' in ", envname(trackingEnv))
    if (length(fileMap)) {
        i <- order(names(fileMap))
        fileData <- paste(fileMap[i], ":", names(fileMap)[i], sep="")
    } else {
        fileData <- character(0)
    }
    ## open in binary mode so that we use just "\n" as a separator
    open.res <- (con <- file(file.path(dataDir, "filemap.txt"), open="wb"))
    if (is(open.res, "try-error")) {
        warning("failed to open ", file.path(dataDir, "filemap.txt"), " for writing: try to fix problem, then do 'track.resave()'")
        return(FALSE)
    }
    on.exit(close(con))
    save.res <- try(writeLines(text=fileData, con=con, sep="\n"))
    if (is(save.res, "try-error")) {
        warning("failed to save filemap.txt: try to fix problem, then do 'track.resave()'")
        return(FALSE)
    }
    return(TRUE)
}

readFileMapFile <- function(trackingEnv, dataDir, assignObj) {
    ## open in binary mode so that we use just "\n" as a separator
    open.res <- (con <- file(file.path(dataDir, "filemap.txt"), open="rb"))
    if (is(open.res, "try-error"))
        stop("failed to open \"", file.path(dataDir, "filemap.txt"), "\" for reading: try using track.rebuild()")
    on.exit(close(con))
    fileData <- try(readLines(con=con, n=-1))
    if (is(fileData, "try-error"))
        stop("failed to read file map data from \"", file.path(dataDir, "filemap.txt"), "\": try using track.rebuild()")
    ## Remove Windows line termination
    fileData <- gsub("\r", "", fileData)
    i <- regexpr(":", fileData, fixed=TRUE)
    if (any(i < 1))
        stop("file map contains invalid data (need a ':' in each line): \"", file.path(dataDir, "filemap.txt"), "\": try using track.rebuild()")
    fileMap <- substring(fileData, 1, i-1)
    names(fileMap) <- substring(fileData, i+1)
    if (assignObj && is(try(assign(".trackingFileMap", fileMap, envir=trackingEnv)), "try-error"))
        warning("failed to assign '.trackingFileMap' in ", envname(trackingEnv))
    return(fileMap)
}

getObjSummary <- function(trackingEnv) {
    objSummary <- mget(".trackingSummary", envir=trackingEnv, ifnotfound=list(NULL))[[1]]
    if (is.null(objSummary) || !is.data.frame(objSummary))
        stop("no usable .trackingSummary object in tracking env ", envname(trackingEnv), " - recommend using track.rebuild()")
    return(objSummary)
}

getUnsavedObj <- function(trackingEnv, notfound=character(0))
    mget(".trackingUnsaved", envir=trackingEnv, ifnotfound=list(notfound))[[1]]

envname <- function(envir) {
    # Produce a 1-line name for the environment.
    # Use the name it has on the search() list, if possible.
    # This is simpler now that R has the environmentName() function
    n <- environmentName(envir)
    if (n!="")
        return(paste("<env ", n, ">", sep=""))
    return(capture.output(print(envir))[1])
}

notyetdone <- function(msg) cat("Not yet done: ", msg, "\n", sep="")

track.dir <- function(pos=1, envir=as.environment(pos), data=FALSE) {
    trackingEnv <- getTrackingEnv(envir)
    ## fileMap <- getFileMapObj(trackingEnv)
    ## unsaved <- getUnsavedObj(trackingEnv)
    dir <- getTrackingDir(trackingEnv)
    if (data)
        dir <- getDataDir(dir)
    return(dir)
}

isSimpleName <- function(objname) {
    return(nchar(objname)<=55
           && regexpr("^[[:lower:]][._[:digit:][:lower:]]*$", objname, perl=TRUE)==1
           && regexpr("^(prn|aux|con|nul|com[1-9]|lpt[1-9])(\\.|$)", objname)<0)
}

makeObjFileName <- function(objname, fileNames) {
    ## check if we can use objname as a filename
    if (isSimpleName(objname))
        return(objname)
    ## fileNames is a list of vectors of file names that are already used.
    ## Generate a filename of the form _NNN (NNN is a number without a leading zero)
    ## work out what numbers have been used
    if (is.list(fileNames))
        used <- unlist(use.names=FALSE, lapply(fileNames, function(x) as.numeric(substring(grep("^_[1-9][0-9]*$", x, value=T), 2))))
    else
        used <- as.numeric(substring(grep("^_[1-9][0-9]*$", fileNames, value=T), 2))
    used <- unique(used)
    used <- sort(c(0, used, length(used)+2))
    ## find the first gap
    i <- used[c(diff(used)!=1, TRUE)][1] + 1
    if (is.element(i, used))
        stop("algorithm failure!")
    file <- paste("_", i, sep="")
    return(file)
}

setTrackedVar <- function(objName, value, trackingEnv, opt=track.options(trackingEnv=trackingEnv), times=NULL, file=NULL, doAssign=TRUE) {
    if (opt$readonly)
        stop("variable '", objName, "' cannot be changed -- it is in a readonly tracking environment")
    ## Set the tracked var, and write it to disk if required
    if (opt$debug)
        cat("setting tracked var '", objName, "' in ", envname(trackingEnv), "\n", sep="")
    ## Need to assign it, because save() requires an object in an env.
    ## Maybe we could skip this step when cache=FALSE, but then
    ## we'll probably need more special case coding for the save().
    ## robustness: what to do if the assign fails?
    if (doAssign)
        assign(objName, value, envir=trackingEnv)
    ## Find the directory where we are saving, and create subdirs if necessary
    dir <- getTrackingDir(trackingEnv)
    for (d in c(dir, getDataDir(dir)))
        if (!file.exists(d))
            dir.create(d)
    ## Work out the name of the file to use for this var
    if (is.null(file)) {
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
    }
    fullFile <- NULL
    if (opt$writeToDisk) {
        ##  the value of 'file' is the base of the filename -- work out the full pathname
        fullFile <- file.path(getDataDir(dir), paste(file, opt$RDataSuffix, sep="."))
        if (opt$debug)
            cat("saving '", objName, "' to file ", fullFile, "\n", sep="")
        save.res <- try(save(list=objName, file=fullFile, envir=trackingEnv), silent=TRUE)
        if (!is(save.res, "try-error")) {
            if (!opt$cache)
                remove(list=objName, envir=trackingEnv)
            unsaved <- getUnsavedObj(trackingEnv)
            if (length(unsaved) && is.element(objName, unsaved))
                if (length(unsaved)>1)
                    assign(".trackingUnsaved", setdiff(unsaved, objName), envir=trackingEnv)
                else
                    remove(".trackingUnsaved", envir=trackingEnv)
        } else {
            stop("failed to save obj '", objName, "' in file '", fullFile,
                 "' (", as.character(save.res), ") - '", objName,
                 "' is currently in env ", envname(trackingEnv), " but is not saved to disk",
                 " (suggestion: fix disk problems then do track.save())")
        }
    } else {
        ## mark the object as unsaved
        unsaved <- getUnsavedObj(trackingEnv)
        if (!is.element(objName, unsaved))
            assign(".trackingUnsaved", sort(c(objName, unsaved)), envir=trackingEnv)
    }
    if (opt$maintainSummary) {
        objSummary <- get(".trackingSummary", envir=trackingEnv, inherits=FALSE)
        if (!is.data.frame(objSummary)) {
            warning(".trackingSummary in ", envname(trackingEnv), " is not a data.frame: not updating summary; run track.rebuild()")
        } else {
            if (is.element(objName, rownames(objSummary))) {
                sumRow <- summaryRow(objName, sumRow=objSummary[objName, , drop=F], obj=value,
                                        file=NULL, change=TRUE, times=times)
            } else {
                ## Don't have a row in the summary for this object.
                if (!isNew) {
                    if (is.null(fullFile))
                        fullFile <- file.path(getDataDir(dir), paste(file, opt$RDataSuffix, sep="."))
                    if (!file.exists(fullFile)) {
                        warning("file '", fullFile, "' does not exist (for obj '", objName, "')")
                        fullFile <- NULL
                    }
                }
                ## summaryRow will get times from the file if we supply it -- only
                ## want to do this in the exceptional case that this object was not
                ## new, but didn't have a summary row.
                sumRow <- summaryRow(objName, obj=value, file=if (!isNew) fullFile else NULL,
                                     change=TRUE, times=times)
                ## If this is not a new object, record that the times are not accurrate.
                if (!isNew)
                    sumRow$A <- 0
            }
            objSummary[objName, ] <- sumRow
            assign.res <- try(assign(".trackingSummary", objSummary, envir=trackingEnv))
            if (is(assign.res, "try-error")) {
                warning("unable to assign .trackingSummary back to tracking env on ", envname(trackingEnv))
            } else {
                assign(".trackingSummaryChanged", TRUE, envir=trackingEnv)
                file <- file.path(getDataDir(dir), paste(".trackingSummary", opt$RDataSuffix, sep="."))
                save.res <- try(save(list=".trackingSummary", file=file, envir=trackingEnv), silent=TRUE)
                if (is(save.res, "try-error"))
                    warning("unable to save .trackingSummary to ", dir)
                else
                    assign(".trackingSummaryChanged", FALSE, envir=trackingEnv)
            }
        }
    }
    ## return the value of the variable
    return(invisible(value))
}

getTrackedVar <- function(objName, trackingEnv, opt=track.options(trackingEnv=trackingEnv)) {
    ##  get the tracked var if it exists, or load it from disk if it doesn't
    if (!is.character(objName) || length(objName)!=1)
        stop("objName must be a length one character vector")
    if (opt$debug)
        cat("getting tracked var '", objName, "' from ", envname(trackingEnv), "\n", sep="")
    if (exists(objName, envir=trackingEnv, inherits=FALSE)) {
        value <- get(objName, envir=trackingEnv, inherits=FALSE)
        dir <- NULL
        fullFile <- NULL
    } else {
        fileMap <- getFileMapObj(trackingEnv)
        file <- fileMap[match(objName, names(fileMap))]
        if (is.na(file))
            stop("'", objName, "' does not exist in tracking env ", envname(trackingEnv),
                 " (has no entry in .trackingFileMap)")
        dir <- getTrackingDir(trackingEnv)
        fullFile <- file.path(getDataDir(dir), paste(file, opt$RDataSuffix, sep="."))
        if (!file.exists(fullFile))
            stop("file '", fullFile, "' does not exist (for obj '", objName, "')")
        tmpenv <- new.env(parent=emptyenv())
        ## hopefully, no actual copies are made while loading the object into tmpenv,
        ## then copying it to envir and returning it as the value of this function
        load.res <- load(fullFile, envir=tmpenv)
        if (length(load.res)<1 || load.res[1] != objName)
            stop("file '", fullFile, "' did not contain obj '", objName, "' as its first object")
        if (length(load.res)>1)
            warning("ignoring ", length(load.res)-1, " other objects in file '", fullFile, "'")
        value <- get(objName, envir=tmpenv, inherits=FALSE)
        if (opt$cache)
            assign(objName, value, envir=trackingEnv)
    }
    ##  update the object table (object characteristics, accesses)
    if (opt$maintainSummary && opt$recordAccesses) {
        objSummary <- get(".trackingSummary", envir=trackingEnv, inherits=FALSE)
        if (!is.data.frame(objSummary)) {
            warning(".trackingSummary in ", envname(trackingEnv), " is not a data.frame: not updating objSummary; run track.rebuild()")
        } else {
            if (is.element(objName, rownames(objSummary))) {
                sumRow <- summaryRow(objName, sumRow=objSummary[objName, , drop=F], obj=value,
                                        file=NULL, change=FALSE, times=NULL)
            } else {
                if (is.null(fullFile)) {
                    fileMap <- getFileMapObj(trackingEnv)
                    file <- fileMap[match(objName, names(fileMap))]
                    if (is.na(file)) {
                        warning("'", objName, "' does not exist in tracking env ", envname(trackingEnv),
                             " (has no entry in .trackingFileMap)")
                    } else {
                        dir <- getTrackingDir(trackingEnv)
                        fullFile <- file.path(getDataDir(dir), paste(file, opt$RDataSuffix, sep="."))
                        if (!file.exists(fullFile)) {
                            warning("file '", fullFile, "' does not exist (for obj '", objName, "')")
                            fullFile <- NULL
                        }
                    }
                }
                sumRow <- summaryRow(objName, obj=value, file=fullFile, change=FALSE, times=NULL)
                ## Since this is not a new object, but this we are creating a new summary
                ## row for it, record that the times are not accurrate
                sumRow$A <- 0
            }
            objSummary[objName, ] <- sumRow
            assign.res <- try(assign(".trackingSummary", objSummary, envir=trackingEnv))
            if (is(assign.res, "try-error")) {
                warning("unable to assign .trackingSummary back to tracking env on ", envname(trackingEnv))
            } else {
                ## only makes sense to save() .trackingSummary if we were able to assign it
                assign(".trackingSummaryChanged", TRUE, envir=trackingEnv)
                if (opt$alwaysSaveSummary) {
                    if (is.null(dir))
                        dir <- getTrackingDir(trackingEnv)
                    file <- file.path(getDataDir(dir), paste(".trackingSummary", opt$RDataSuffix, sep="."))
                    save.res <- try(save(list=".trackingSummary", file=file, envir=trackingEnv), silent=TRUE)
                    if (is(save.res, "try-error"))
                        warning("unable to save .trackingSummary to ", dir)
                    else
                        assign(".trackingSummaryChanged", FALSE, envir=trackingEnv)
                }
            }
        }
    }
    return(value)
}

track.load <- function(files, pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL, cache=FALSE, clobber=FALSE, time.of.file=TRUE, warn=TRUE) {
    ## load some or all variables from some RData files into a tracked environment
    trackingEnv <- getTrackingEnv(envir)
    opt <- track.options(trackingEnv=trackingEnv)
    if (!is.logical(cache) || is.na(cache))
        stop("'cache' must be TRUE or FALSE")
    opt$cache <- cache
    tmpenv <- new.env(hash=TRUE, parent=emptyenv())
    all.loaded <- character(0)
    all.skipped <- character(0)
    all.wanted <- list
    for (file in files) {
        if (!file.exists(file)) {
            warning("skipping ", file, ": file does not exist")
            next
        }
        load.res <- try(load(file=file, envir=tmpenv), silent=TRUE)
        if (is(load.res, "try-error")) {
            warning("skipping ", file, ": could not read saved objects: ", as.character(load.res))
            next
        }
        info <- if (time.of.file) file.info(file) else NULL
        list <- all.wanted
        if (is.null(list))
            list <- load.res
        else
            list <- intersect(load.res, list)
        i <- isReservedName(list)
        if (any(i))
            list <- list[!i]
        if (!is.null(glob))
            pattern <- glob2rx(glob)
        if (!is.null(pattern))
            list <- grep(pattern, list, value=TRUE)
        all.objs <- .Internal(ls(envir, TRUE))
        i <- is.element(list, all.objs) | duplicated(list)
        if (any(i)) {
            if (!clobber) {
                warning("skipping ", sum(i), " variable(s) because these exist in ", envname(envir),
                        " and clobber=FALSE: ",
                        paste("'", list[i][seq(len=min(3, sum(i)))], "'", sep=""), if (sum(i)>3) "...", "\n")
                list <- list[!i]
            } else {
                warning("clobbering ", sum(i), " existing variable(s) in ", envname(envir),
                        " and clobber=FALSE: ",
                        paste("'", list[i][seq(len=min(3, sum(i)))], "'", sep=""), if (sum(i)>3) "...", "\n")
            }
        }
        for (objName in list) {
            objval <- get(objName, envir=tmpenv)
            setTrackedVar(objName, objval, trackingEnv, opt, info)
            ## always remove the object and reassign the binding
            if (is.element(objName, all.objs))
                remove(list=objName, envir=envir)
            f <- substitute(function(v) {
                if (missing(v))
                    getTrackedVar(x, envir)
                else
                    setTrackedVar(x, v, envir)
            }, list(x=objName, envir=trackingEnv))
            mode(f) <- "function"
            environment(f) <- parent.env(environment(f))
            makeActiveBinding(objName, env=envir, fun=f)
        }
        all.loaded <- c(all.loaded, list)
        all.skipped <- c(all.skipped, setdiff(load.res, list))
    }
    if (any(!is.element(all.wanted, all.loaded)))
        warning("the following requested objects were not found: ", paste(setdiff(all.wanted, all.loaded), collapse=", "))

    return(list(loaded=all.loaded, skipped=all.skipped))
}

if (F) {
create.fake.Sys.time2 <- function() {
    ## The Sys.time() function created by this function doesn't get
    ## called by functions in a different environment.
    ## Create a fake Sys.time() function that just counts 1 second forward
    ## from a fixed starting time each time it is called.
    if (!is.element("fake.Sys.time", search())) {
        f <- local({time.counter <- as.POSIXct("2001/01/01 09:00:00", tz="GMT")
                     Sys.time <- function() {
                         time.counter <<- time.counter + 1
                         return(time.counter)
                     }})
        attach(environment(f), name="fake.Sys.time", warn.conflicts=FALSE)
    }
    invisible(NULL)
}
}

create.fake.Sys.time <- function(offset) {
    ## Create a fake Sys.time() function that just counts 1 second forward
    ## from a fixed starting time each time it is called.
    if (!exists("Sys.time.counter", envir=baseenv(), inherits=FALSE)) {
        ## Assign a new Sys.time() function in the base environment.
        unlockBinding("Sys.time", baseenv())
        if (!exists("Sys.time.orig", envir=baseenv(), inherits=FALSE))
            assign("Sys.time.orig", get("Sys.time", envir=baseenv()), envir=baseenv())
        assign("Sys.time", function() {
            Sys.time.counter <- get("Sys.time.counter", envir=baseenv()) + 1
            assign("Sys.time.counter", Sys.time.counter, envir=baseenv())
            return(Sys.time.counter)
        }, envir=baseenv())
    }
    assign("Sys.time.counter", as.POSIXct("2001/01/01 09:00:00", tz="GMT")+offset, envir=baseenv())
    invisible(NULL)
}

if (F) {
    ## Code in here was an alternate, worse way to override Sys.time() for testing purposes
    ## (worse because it introduced permanent overhead for Sys.time(), even in normal operation)
call.Sys.time <- function() Sys.time()

Sys.time <- function() {
    if (!is.element("Sys.time.control", search()))
        create.fake.Sys.time(FALSE)
    if (get("use.fake.Sys.time", pos="Sys.time.control")) {
        Sys.time.counter <- get("Sys.time.counter", pos="Sys.time.control")
        assign("Sys.time.counter", Sys.time.counter + 1, pos="Sys.time.control")
        return(Sys.time.counter)
    } else {
        return(base::Sys.time())
    }
}

toggle.fake.Sys.time <- function()
    return(invisible(use.fake.Sys.time <<- !use.fake.Sys.time))

create.fake.Sys.time <- function(use.fake=TRUE, offset=0) {
    if (!is.element("Sys.time.control", search()))
        attach(what=new.env(), name="Sys.time.control")
    assign("Sys.time.counter", as.POSIXct("2001/01/01 09:00:00", tz="GMT")+offset, pos="Sys.time.control")
    assign("use.fake.Sys.time", use.fake, pos="Sys.time.control")
    return(invisible(use.fake))
}
}
