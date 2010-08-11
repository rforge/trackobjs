track.options <- function(..., pos=1, envir=as.environment(pos), save=FALSE, clear=FALSE, delete=FALSE, trackingEnv, only.preprocess=FALSE, old.options=list()) {
    ## This function probably tries to do too many things (e.g., in using arg
    ## combinations only.preprocess, trackingEnv, save, old.options, ...)
    ## It would probably be better rewritten into several functions, with
    ##   track.options(..., pos, envir, save) as the user-visible function.
    ##
    ## only.preprocess: return a complete list of new options, with values as specified as arguments,
    ##   and others as defaults
    ## valid options are:
    ##   summaryTimes: logical, or integer value 0,1,2,3
    ##   summaryAccess: logical, or integer value 0,1,2,3,4
    ##   cache: logical (default TRUE) (keep written objects in memory?)
    ##   cachePolicy: char vector (default "withinTask") (when to keep objects in memory)
    ##   writeToDisk: logical (default TRUE) (always write changed objects to disk?)
    ##   useDisk: logical (default TRUE) if FALSE, don't write anything
    ##   recordAccesses: logical (default TRUE) if TRUE, record time & number of get()'s
    ##   maintainSummary: logical (default TRUE) if TRUE, record time & number of accesses
    ##   alwaysSaveSummary: logical (default TRUE) if TRUE, always save the summary on any change
    ##   RDataSuffix: character (default "rda")
    ##   debug: integer (default 0) if > 0, print some diagnostic debugging messages
    ##   readonly: logical (default FALSE for track.start(), TRUE for track.attach())
    ##   autoTrackExcludePattern: vector of strings: regular expressions describing which variables not
    ##      to auto-track (default "^\\.track", "^.required$")
    ##   autoTrackExcludeClass: vector of strings: class names for objects that should
    ##      not be auto-tracked (default "RODBC")
    ##   autoTrackFullSyncWait: wait this many seconds between doing a full sync
    ##   clobberVars: vector of string specifying variables to be clobbered silently when attaching a tracking db
    trackingEnvSupplied <- !missing(trackingEnv) && !is.null(trackingEnv)
    if (only.preprocess) {
        currentOptions <- old.options
    } else {
        if (length(old.options)!=0)
            stop("can only supply old.options when only.preprocess=TRUE")
        if (!trackingEnvSupplied)
            trackingEnv <- getTrackingEnv(envir)
        currentOptions <- mget(".trackingOptions", envir=trackingEnv, ifnotfound=list(list()))[[1]]
        if (length(currentOptions)==0 && exists(".trackingDir", envir=trackingEnv, inherits=FALSE)) {
            ## Read the options from file if we have a trackingDir on envir and no trackingEnv was supplied
            ## I'm not sure if this code ever gets exercised ... the only case I can think of right
            ## now is where .trackingOptions went missing from trackingEnv for some reason.
            ## Try to read them from disk
            dir <- getTrackingDir(envir)
            dataDir <- getDataDir(dir)
            ## we need to work out the RData suffix here -- it's a bit tricky
            ## collect the possibilities and look for them
            gopt <- getOption("global.track.options")
            if (length(gopt$RDataSuffixes)==0)
                gopt$RDataSuffixes <- c("rda", "RData")
            if (!is.character(gopt$RDataSuffixes))
                stop('getOption("global.track.options")$RDataSuffixes must be character data')
            if (any(!regexpr("^[[:alnum:]]+$", gopt$RDataSuffixes)))
                stop('getOption("global.track.options")$RDataSuffixes must consist of alpha-numeric characters only')
            if (length(gopt$RDataSuffixes)==1)
                suffixRegExp <- gopt$RDataSuffixes
            else
                suffixRegExp <- paste("(", paste(gopt$RDataSuffixes, collapse="|", sep=""), ")", sep="")
            suffix <- NULL
            x <- list.files(path=dataDir, pattern=paste("^\\.trackingOptions\\.", suffixRegExp, "$", sep=""), all.files=TRUE)
            if (length(x)>1)
                stop("have multiple options files in '", dataDir, "': ", paste(x, collapse=", "))
            if (length(x)==1) {
                suffix <- sub(".*\\.", "", x)
                file <- file.path(dataDir, paste(".trackingOptions", suffix, sep="."))
                if (!file.exists(file))
                    stop("weird: thought I had the options file, but it doesn't exist...; ", file, "; ", x)
                tmpenv <- new.env(parent=emptyenv())
                load.res <- try(load(file=file, envir=tmpenv))
                if (is(load.res, "try-error") || length(load.res)!=1 || load.res!=".trackingOptions") {
                    warning(file, " does not contain a .trackingOptions object -- ignoring it and using system defaults")
                } else {
                    currentOptions <- get(".trackingOptions", envir=tmpenv, inherits=FALSE)
                }
            } else {
                if (trackingEnvSupplied)
                    warning("no .trackingOptions in ", envname(trackingEnv),
                            " and there is no saved options file -- using system defaults")
            }
        }
    }
    if (length(currentOptions)==0) ## in case someone supplied old.options=NULL
        currentOptions <- list()
    values <- list(...)
    ## if we were called like track.options(NULL), make this like track.options()
    if (length(values)==1 && is.null(values[[1]]))
        values <- list()
    if (length(values)==1 && is.list(values[[1]]))
        values <- values[[1]]
    optionNames <- c("cache", "cachePolicy", "writeToDisk", "maintainSummary", "alwaysSaveSummary",
                     "useDisk", "recordAccesses", "summaryTimes", "summaryAccess",
                     "RDataSuffix", "debug", "autoTrackExcludePattern", "autoTrackExcludeClass",
                     "autoTrackFullSyncWait", "clobberVars", "readonly")
    if (!is.null(names(values))) {
        ## Attempt to set some of the options (including saving to file)
        ## and return the old values.
        ## First retrieve the old values
        query.values <- names(values)
        set.values <- TRUE
        ## No longer a problem:
        ## can't supply trackingEnv and set option values because we wouldn't know where to write the file
        ## if (trackingEnvSupplied)
        ##    stop("cannot supply trackingEnv and set option values")
    } else if (save) {
        query.values <- optionNames
        set.values <- TRUE
    } else {
        ## no names means a query -- expect all char data
        if (length(values)==0) {
            query.values <- optionNames
        } else {
            if (!all(sapply(values, is.character)))
                stop("in a query, all args must be character data")
            query.values <- unlist(values, use.names=FALSE)
        }
        set.values <- FALSE
    }

    if (!all(is.element(query.values, optionNames)))
        stop("unknown option names: ", paste("'", setdiff(values, optionNames), "'", sep="", collapse=", "))

    ## See if we need to repair any missing options (shouldn't need to do this)
    ## This is where to set defaults
    if (set.values || only.preprocess)
        need.value <- setdiff(optionNames, names(currentOptions))
    else
        need.value <- setdiff(query.values, names(currentOptions))
    if (length(need.value)) {
        names(need.value) <- need.value
        repaired <- lapply(need.value, function(x)
                           switch(x, cache=TRUE, cachePolicy="withinTask",
                                  readonly=FALSE, writeToDisk=TRUE,
                                  maintainSummary=TRUE, alwaysSaveSummary=FALSE,
                                  useDisk=TRUE, recordAccesses=TRUE,
                                  summaryTimes=1, summaryAccess=1, RDataSuffix="rda",
                                  debug=0, autoTrackExcludePattern=c("^\\.track", "^\\.required"),
                                  autoTrackExcludeClass=c("RODBC"),
                                  autoTrackFullSyncWait=15, clobberVars=".Random.seed"))
        currentOptions <- c(currentOptions, repaired)
    }
    option.values <- currentOptions[query.values]
    if (set.values) {
        new.values <- currentOptions
        for (opt in names(values)) {
            single <- TRUE
            if (opt=="cache") {
                if (!is.logical(values[[opt]]))
                    values[[opt]] <- as.logical(values[[opt]])
            } else if (opt=="cachePolicy") {
                if (!is.character(values[[opt]]))
                    values[[opt]] <- as.character(values[[opt]])
            } else if (opt=="readonly") {
                if (!is.logical(values[[opt]]))
                    values[[opt]] <- as.logical(values[[opt]])
            } else if (opt=="writeToDisk") {
                if (!is.logical(values[[opt]]))
                    values[[opt]] <- as.logical(values[[opt]])
            } else if (opt=="useDisk") {
                if (!is.logical(values[[opt]]))
                    values[[opt]] <- as.logical(values[[opt]])
            } else if (opt=="recordAccesses") {
                if (!is.logical(values[[opt]]))
                    values[[opt]] <- as.logical(values[[opt]])
            } else if (opt=="maintainSummary") {
                if (!is.logical(values[[opt]]))
                    values[[opt]] <- as.logical(values[[opt]])
            } else if (opt=="alwaysSaveSummary") {
                if (!is.logical(values[[opt]]))
                    values[[opt]] <- as.logical(values[[opt]])
            } else if (opt=="summaryTimes") {
                if (!is.integer(values[[opt]]))
                    values[[opt]] <- as.integer(values[[opt]])
            } else if (opt=="summaryAccess") {
                if (!is.integer(values[[opt]]))
                    values[[opt]] <- as.integer(values[[opt]])
            } else if (opt=="RDataSuffix") {
                if (!is.character(values[[opt]]))
                    values[[opt]] <- as.character(values[[opt]])
            } else if (opt=="debug") {
                if (!is.integer(values[[opt]]))
                    values[[opt]] <- as.integer(values[[opt]])
            } else if (opt=="autoTrackExcludePattern" || opt=="autoTrackExcludeClass") {
                single <- FALSE
                if (!is.character(values[[opt]]))
                    values[[opt]] <- as.character(values[[opt]])
            } else if (opt=="autoTrackFullSyncWait") {
                if (!is.numeric(values[[opt]]))
                    values[[opt]] <- as.numeric(values[[opt]])
            } else if (opt=="clobberVars") {
                single <- FALSE
                if (!is.character(values[[opt]]))
                    values[[opt]] <- as.character(values[[opt]])
            } else {
                stop("unrecognized option name '", opt, "'")
            }
            if (any(is.na(values[[opt]])))
                stop("cannot set option ", opt, " to an NA value")
            if (single && length(values[[opt]])!=1)
                stop("option ", opt, " must have a value of length 1")
            ## Now, how we put the value in depends on whethr it can have single or multiple values
            if (single || clear)
                new.values[[opt]] <- values[[opt]]
            else if (delete)
                new.values[[opt]] <- setdiff(new.values[[opt]], values[[opt]])
            else
                new.values[[opt]] <- unique(c(new.values[[opt]], values[[opt]]))
        }

        if (only.preprocess)
            return(new.values)
        if (!new.values$readonly && environmentIsLocked(envir))
            stop("cannot make a readonly tracked environment writable (because cannot unlock a locked environment) -- to make it writeable, use track.detach() followed by track.attach(readonly=FALSE)")
        assign(".trackingOptions", new.values, envir=trackingEnv)
    }
    if (save && !only.preprocess && !identical(currentOptions$useDisk, FALSE)) {
        ## write them to disk -- use the old value of options$useDisk
        ## because otherwise we will never save the change of this
        dir <- getTrackingDir(trackingEnv)
        file <- file.path(getDataDir(dir), paste(".trackingOptions", currentOptions$RDataSuffix, sep="."))
        ## if we did change any options, they will have been saved in .trackingOptions in trackingEnv
        save.res <- try(save(list=".trackingOptions", file=file, envir=trackingEnv))
        if (is(save.res, "try-error"))
            stop("unable to save .trackingOptions in ", file)
    }
    ## Want to return the old values
    if (set.values)
        return(invisible(option.values))
    else
        return(option.values)
}

