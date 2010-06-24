
track.summary <- function(expr, pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL,
                          times=track.options("summaryTimes", envir=envir)[[1]],
                          access=track.options("summaryAccess", envir=envir)[[1]],
                          size=TRUE) {
    if (is.logical(times))
        times <- if (times) 3 else 0
    if (!is.numeric(times))
        stop("'times' must be logical or numeric")
    trackingEnv <- getTrackingEnv(envir)
    if (!exists(".trackingSummary", envir=trackingEnv, inherits=FALSE))
        stop("there is no .trackingSummary in the tracking env on ", envname(envir))
    # get the summary, and prune the list of objects down to the ones specified or matching the pattern
    objSummary <- get(".trackingSummary", envir=trackingEnv, inherits=FALSE)
    if (!is.data.frame(objSummary))
        stop(".trackingSummary in ", envname(trackingEnv), " is not a data.frame")
    objs <- rownames(objSummary)
    qexpr <- if (!missing(expr)) substitute(expr) else NULL
    if (!is.null(qexpr)) {
        if (is.name(qexpr)) {
            objname <- as.character(qexpr)
        } else {
            stop("expr argument must be an unquoted variable")
        }
        list <- c(objname, list)
        if (!is.null(glob) || !is.null(pattern))
            stop("must specify EITHER expr and/or list, OR pattern or glob")
        vars.how <- " specified"
        if (length(setdiff(list, objs)))
            warning("some specified vars are not in the summary: ", paste("'", setdiff(list, objs), "'", sep="", collapse=", "))
    } else {
        vars.how <- ""
        if (!is.null(glob))
            pattern <- glob2rx(glob)
        if (!is.null(pattern)) {
            objs <- grep(pattern, objs, value=TRUE)
            vars.how <- " matching"
        }
    }
    if (!is.null(list))
        objs <- intersect(objs, list)

    # Get an independent idea of what vars are currently tracked.
    tracked.actual <- unique(track.status(envir=envir, qexpr=qexpr, list=list, pattern=pattern, glob=glob, file.status=FALSE, what="tracked"))
    # Might it be better to have a "status" column in the returned summary
    # than print out a warning message here?
    if (length(setdiff(tracked.actual, objs)))
        warning("some", vars.how, " vars appear to be tracked but are not in the summary: ", paste("'", setdiff(tracked.actual, objs), "'", sep="", collapse=", "))
    if (length(setdiff(objs, tracked.actual)))
        warning("some", vars.how, " vars are in the summary but not tracked (info in summary may be wrong for the visible instances of these vars): ", paste("'", setdiff(objs, tracked.actual), "'", sep="", collapse=", "))
    objSummary <- objSummary[objs, ,drop=FALSE]
    colsWanted <- c("class", "mode", "extent", "length")
    # size is system dependent, so make it easy to exclude
    if (size)
        colsWanted <- c(colsWanted, "size")
    if (times > 0)
        colsWanted <- c(colsWanted, "modified")
    if (times > 1)
        colsWanted <- c(colsWanted, "created")
    if (times > 2)
        colsWanted <- c(colsWanted, "accessed")
    if (access == 1)
        colsWanted <- c(colsWanted, c("TA", "TW"))
    else if (access == 2)
        colsWanted <- c(colsWanted, c("ES", "SA", "SW", "PA", "PW"))
    else if (access >= 3)
        colsWanted <- c(colsWanted, c("ES", "SA", "SW", "PA", "PW", "TA", "TW"))
    if (any(is.element(colsWanted, c("TA", "TW"))))
        objSummary <- cbind(objSummary, TA=objSummary$SA + objSummary$PA, TW=objSummary$SW + objSummary$PW)
    return(objSummary[order(rownames(objSummary)), colsWanted])
}
