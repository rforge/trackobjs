track.start <- function(dir="rdatadir", pos=1, envir=as.environment(pos),
                        create=TRUE, clobber=c("no", "files", "variables"),
                        cache=NULL, options=NULL, RDataSuffix=NULL, auto=NULL,
                        readonly=FALSE) {
    ## Start tracking the specified environment to a directory
    clobber <- match.arg(clobber)
    if (env.is.tracked(envir))
        stop("env ", envname(envir), " is already tracked by dir '",
             get(".trackingDir", envir=getTrackingEnv(envir), inherits=FALSE), "'")
    dir <- getAbsolutePath(dir)
    ## Working out the options values to use is a little tricky.
    ## This is the priority:
    ## (1) values in the 'options' argument ('cache' override options$cache)
    ## (2) values in a saved .trackOptions object (if it exists)
    ## (3) getOption("global.track.options") (user modifiable)
    ## (4) standard package defaults (not user modifiable)
    ##
    ## Create the tracking env, but don't assign it to envir until all is OK
    trackingEnv <- new.env(hash=TRUE, parent=emptyenv())
    assign(".trackingDir", dir, envir=trackingEnv)
    dataDir <- getDataDir(dir)
    ## The trickiest option to work out is the RData file suffix, because
    ## we have to know it to find a saved .trackingOptions object, but this
    ## object may not be there.  So, first look for .trackingOptions.*, then
    ## then .trackingSummary.*, then data files.
    gopt <- getOption("global.track.options")
    if (length(gopt$RDataSuffixes)==0)
        gopt$RDataSuffixes <- c("rda", "RData")
    if (!is.character(gopt$RDataSuffixes))
        stop('getOption("global.track.options")$RDataSuffixes must be character data')
    if (any(!regexpr("^[[:alnum:]]+$", gopt$RDataSuffixes)))
        stop('getOption("global.track.options")$RDataSuffixes must consist of alpha-numeric characters only')
    if (!is.null(RDataSuffix)) {
        if (!is.character(RDataSuffix) || length(RDataSuffix)!=1)
            stop('RDataSuffix must be character data of length 1')
        if (any(!regexpr("^[[:alnum:]]+$", RDataSuffix)))
            stop('RDataSuffix must consist of alpha-numeric characters only')
    }
    if (length(gopt$RDataSuffixes)==1)
        suffixRegExp <- gopt$RDataSuffixes
    else
        suffixRegExp <- paste("(", paste(gopt$RDataSuffixes, collapse="|", sep=""), ")", sep="")
    if (is.null(auto))
        if (length(gopt$autoTrack))
            auto <- gopt$autoTrack
        else
            auto <- TRUE
    optionsPath <- NULL
    if (file.exists(file.path(dataDir))) {
        ## Try to work out the suffix being used
        ## First look for .trackingOptions file
        suffix <- NULL
        x <- list.files(path=dataDir, pattern=paste("^\\.trackingOptions\\.", suffixRegExp, "$", sep=""), all.files=TRUE)
        if (length(x)>1)
            stop("have multiple options files in '", dataDir, "': ", paste(x, collapse=", "))
        if (length(x)==1) {
            optionsPath <- file.path(dataDir, x)
            suffix <- sub(".*\\.", "", x)
        }
        if (is.null(suffix)) {
            ## next look for .trackingSummary file
            x <- list.files(path=dataDir, pattern=paste("^\\.trackingSummary\\.", suffixRegExp, "$", sep=""), all.files=TRUE)
            if (length(x)>1)
                stop("have multiple summary files in '", dataDir, "': ", paste(x, collapse=", "))
            if (length(x)==1)
                suffix <- sub(".*\\.", "", x)
        }
        if (is.null(suffix)) {
            ## next look for any files with possible RData suffix
            x <- list.files(path=dataDir, pattern=paste("^.*\\.", suffixRegExp, "$", sep=""), all.files=TRUE)
            if (length(x)>0) {
                suffix <- unique(sub(".*\\.", "", x))
                if (length(suffix)>1)
                    stop("have files with multiple RData suffixes in '", dataDir, "': ", paste(x, collapse=", "))
            } else {
                if (!is.null(RDataSuffix))
                    suffix <- RDataSuffix
                else
                    suffix <- gopt$RDataSuffixes[1]
            }
        }
    } else {
        if (is.null(RDataSuffix))
            suffix <- gopt$RDataSuffixes[1]
        else
            suffix <- RDataSuffix
    }
    if (!is.element(suffix, gopt$RDataSuffixes))
        stop("internal error: ended up with an illegal suffix?? (", suffix, ")")
    if (!is.null(RDataSuffix) && RDataSuffix != suffix)
        stop("suffix in use '", suffix, "' differs from supplied RDataSuffix ('", RDataSuffix, "')")

    ## Preprocess the options (standardize, get defaults).
    if (!is.null(cache)) {
        if (is.null(options))
            options <- list()
        if (is.logical(cache) && length(cache)==1 && !is.na(cache))
            options$cache <- cache
        else
            stop("'cache' argument must be TRUE or FALSE")
    }
    old.options <- list()
    if (!is.null(optionsPath)) {
        tmpenv <- new.env(parent=emptyenv())
        load.res <- try(load(optionsPath, envir=tmpenv), silent=TRUE)
        if (is(load.res, "try-error"))
            warning(optionsPath, " cannot be loaded -- ignoring it; for recovery see ?track.rebuild (",
                 as.character(load.res), ")")
        if (length(load.res)!=1 || load.res != ".trackingOptions") {
            warning(optionsPath, " does not contain just '.trackingOptions' -- ignoring it; for recovery see ?track.rebuild")
        } else {
            ## .trackingOptions has to exist because we just loaded it
            old.options <- get(".trackingOptions", envir=tmpenv, inherits=FALSE)
            if (!is.list(old.options)) {
                warning("'.trackingOptions' from ", optionsPath, " is not a list -- ignoring it; see ?track.rebuild")
                old.options <- list()
            }
        }
    }
    if (length(old.options)==0) {
        # Couldn't read any options from the file, initialize them
        # from remaining gopt.
        gopt$autoTrack <- NULL
        gopt$RDataSuffixes <- NULL
        old.options <- gopt
    }
    opt <- track.options(options, envir=NULL, only.preprocess=TRUE, old.options=old.options)
    if (!is.null(readonly))
        opt$readonly <- readonly
    assign(".trackingOptions", opt, envir=trackingEnv)
    fileMapPath <- file.path(dataDir, "filemap.txt")
    fileMapCreated <- FALSE
    objSummaryPath <- file.path(dataDir, paste(".trackingSummary.", opt$RDataSuffix, sep=""))
    ## Create a default empty objSummary -- an existing one will replace this
    objSummary <- summaryRow(name="")[0,]
    if (!file.exists(file.path(dataDir))) {
        if (!create)
            stop("dir \"", dir, "\" does not exist (supply create=TRUE to create it)")
        else if (is(dir.create(file.path(dataDir), recursive=TRUE), "try-error"))
            stop("could not creating tracking dir")
        fileMap <- character(0)
        assign(".trackingFileMap", fileMap, envir=trackingEnv)
        fileMapCreated <- TRUE
        assign(".trackingSummary", objSummary, envir=trackingEnv)
    } else {
        ## Try to read the summary first, because if there is a problem with fileMap,
        ## we may want to erase the summary
        tmpenv <- new.env(parent=emptyenv())
        if (file.exists(objSummaryPath)) {
            load.res <- try(load(objSummaryPath, envir=tmpenv), silent=TRUE)
            if (is(load.res, "try-error"))
                stop(objSummaryPath, " cannot be loaded -- for recovery see ?track.rebuild (",
                     as.character(load.res), ")")
            if (length(load.res)!=1 || load.res != ".trackingSummary")
                stop(objSummaryPath, " does not contain just '.trackingSummary' -- for recovery see ?track.rebuild")
            ## .trackingSummary has to exist because we just loaded it
            objSummary <- get(".trackingSummary", envir=tmpenv, inherits=FALSE)
            if (!is.data.frame(objSummary))
                stop("'.trackingSummary' from ", objSummaryPath, " is not a data.frame -- see ?track.rebuild")
            assign(".trackingSummary", objSummary, envir=trackingEnv)
        }
        ## Need to confirm that there are no clashes between variables already
        ## stored in the tracking dir and variables in envir
        if (file.exists(fileMapPath)) {
            fileMap <- readFileMapFile(trackingEnv, dataDir, TRUE)
            alreadyExists <- logical(0)
            if (length(fileMap))
                alreadyExists <- sapply(names(fileMap), exists, envir=envir, inherits=FALSE)
            alreadyExists <- names(fileMap)[alreadyExists]
            ## opt$clobberVars is processed before the clobber= argument
            clobberFirst <- intersect(opt$clobberVars, alreadyExists)
            if (length(clobberFirst)) {
                alreadyExists <- setdiff(alreadyExists, clobberFirst)
                remove(list=clobberFirst, envir=envir)
            }
            if (length(alreadyExists)) {
                if (clobber=="no") {
                    assign(".trackAlreadyExists", alreadyExists, envir=envir)
                    stop("cannot start tracking to dir \"", dir, "\" because it contains ",
                         length(alreadyExists), " vars that currently exist in ", envname(envir),
                         ", e.g.: ", paste("'", alreadyExists[seq(len=min(3,length(alreadyExists)))], "'", sep="", collapse=", "),
                         if (length(alreadyExists)>3) ", ...",
                         " (try track.start(..., clobber='files') or track.start(..., clobber='vars') to clobber one or the other")
                } else if (clobber=="files") {
                    if (opt$readonly) {
                        warning("will not clobber files corresponding to existing variables because readonly=TRUE: ", paste(alreadyExists, collapse=", "))
                    } else {
                        for (varName in alreadyExists) {
                            file <- fileMap[match(varName, names(fileMap))]
                            file.remove(file.path(dataDir, paste(file, opt$RDataSuffix, sep=".")))
                            value <- get(varName, envir=envir, inherits=FALSE)
                            setTrackedVar(varName, value, trackingEnv, opt=replace(opt, "maintainSummary", FALSE), file=file)
                            remove(list=varName, envir=envir, inherits=FALSE)

                        }
                    }
                } else if (clobber=="variables") {
                    remove(list=alreadyExists, envir=envir)
                }
            }
        } else {
            ## if there are in .rda files in this directory, need to rebuild the fileMap
            files <- list.files(path=file.path(dataDir), pattern=paste(".*\\.", opt$RDataSuffix, "$", sep=""), all.files=TRUE)
            files <- setdiff(files, paste(".trackingSummary.", opt$RDataSuffix, sep=""))
            if (length(files)==0) {
                ## No files: start with a new fileMap and objSummary
                fileMap <- character(0)
                fileMapCreated <- TRUE
                assign(".trackingFileMap", fileMap, envir=trackingEnv)
            } else {
                stop("tracking dir \"", dir, "\" has some data files in it, but has no filemap.txt file -- see ?track.rebuild")
            }
        }
        if (nrow(objSummary)) {
            ## update the prior-reads and prior-writes, and existing session counts
            objSummary[,"ES"] <- objSummary[,"ES"] + 1
            objSummary[,"PA"] <- objSummary[,"PA"] + objSummary[,"SA"]
            objSummary[,"PW"] <- objSummary[,"PW"] + objSummary[,"SW"]
            objSummary[,"SA"] <- 0
            objSummary[,"SW"] <- 0
        }
    }
    ## Do we need to save the fileMap ? (only if changed)
    if (fileMapCreated && !opt$readonly)
        writeFileMapFile(fileMap, trackingEnv, dataDir, TRUE)
    ## We always need to save the summary...
    assign(".trackingSummary", objSummary, envir=trackingEnv)
    assign(".trackingSummaryChanged", TRUE, envir=trackingEnv)
    if (!opt$readonly) {
        save.res <- try(save(list=".trackingSummary", envir=trackingEnv, file=objSummaryPath))
        if (is(save.res, "try-error"))
            stop("could not save '.trackingSummary' in ", objSummaryPath, ": fix file problem and try again")
    }
    assign(".trackingSummaryChanged", FALSE, envir=trackingEnv)
    if (dir!=dataDir && !file.exists(dfn)) {
        ## Only use a "DESCRIPTION" file when we use a 'data' subdirectory
        ## in the tracking dir.
        dfn <- file.path(dir, "DESCRIPTION")
        write.res <- try(cat(trackObjs.package.desc(basename(dir)), "\n", sep="\n", file=dfn), silent=TRUE)
        if (is(write.res, "try-error"))
            warning("had problem writing ", dfn, " (", as.character(write.res), ")")
    }
    ## load bindings for the vars already in the tracking dir
    for (objname in names(fileMap)) {
        f <- substitute(function(v) {
            if (missing(v))
                getTrackedVar(x, envir)
            else
                setTrackedVar(x, v, envir)
        }, list(x=objname, envir=trackingEnv))
        mode(f) <- "function"
        environment(f) <- parent.env(environment(f))
        makeActiveBinding(objname, env=envir, fun=f)
    }
    setTrackingEnv(trackedEnv=envir, trackingEnv=trackingEnv)
    if (auto) {
        addTaskCallback(track.sync.callback, data=envir, name=paste("track.auto:", envname(envir), sep=""))
        assign(".trackAuto", list(on=TRUE, last=-1), envir=trackingEnv)
    }
    if (opt$readonly && environmentName(envir) != "R_GlobalEnv")
        lockEnvironment(envir)
    return(invisible(NULL))
}

trackObjs.package.desc <- function(pkg)
    c(paste("Package:",pkg), "Version: 1.0", paste("Date:",date()),
      "Title: Tracked R Objects", "Author: trackObjs package", "Maintainer: trackObjs package",
      "Description: package of saved objects created by trackObjs package", "License: None specified")
