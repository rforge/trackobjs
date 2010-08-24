track.attach <- function(dir, pos=2, name=NULL, create=FALSE, readonly=!create, lockEnv=FALSE, verbose=TRUE) {
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
        if (!create || readonly)
            stop("dir '", dir, "' does not exist and create=FALSE or readonly=TRUE")
        trackingDir <- dir
    }
    if (is.null(name))
        name <- getAbsolutePath(dir)
    attach(what=NULL, pos=pos, name=name)
    # track.start() will print out a message
    # cat("Attaching to tracking db in '", name, "'\n", sep="")
    return(track.start(trackingDir, pos=pos, readonly=readonly, create=create, lockEnv=lockEnv, check.Last=FALSE, verbose=verbose))
}
