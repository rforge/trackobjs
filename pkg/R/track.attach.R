track.attach <- function(dir, pos=2, name=NULL, create=FALSE, readonly=!create, lockEnv=FALSE, verbose=TRUE, auto=NULL) {
    ## if (missing(dir) && missing(pos) && is.null(name) && !readonly && !lockEnv)
    ##    return(track.start())
    if (pos < 2)
        stop("pos must be >= 2")
    if (file.exists(file.path(dir, "filemap.txt"))) {
        trackingDir <- dir
        if (basename(dir)=="rdatadir")
            dir <- dirname(dir)
    } else if (file.exists(file.path(dir, "rdatadir", "filemap.txt"))) {
        trackingDir <- file.path(dir, "rdatadir")
    } else {
        if (!create)
            stop("tracking db does not exist in '", dir, "' or '", file.path(dir, "rdatadir"),
                 "' and cannot create because create=FALSE")
        else if (readonly)
            stop("tracking db does not exist in '", dir, "' or '", file.path(dir, "rdatadir"),
                 "' and cannot create because readonly=TRUE")
        trackingDir <- dir
    }
    if (is.null(name))
        name <- getAbsolutePath(dir)
    attach(what=NULL, pos=pos, name=name)
    assign(".trackingCreated", TRUE, pos=pos)
    if (verbose)
        cat("Attaching tracking db in '", dir, "' to env in pos ", pos,
            if (readonly) " (readonly)" else " (writable)", "\n", sep="")
    return(track.start(trackingDir, pos=pos, readonly=readonly, create=create, lockEnv=lockEnv, check.Last=FALSE, verbose=FALSE, auto=auto))
}
