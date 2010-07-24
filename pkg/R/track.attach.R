track.attach <- function(dir, pos=2, name=NULL, readonly=TRUE, create=FALSE, lockEnv=FALSE) {
    if (pos < 2)
        stop("pos must be >= 2")
    if (!file.exists(dir))
        if (create) {
            dir.create(dir)
            cat("", file=file.path(dir, "filemap.txt"))
        } else {
            stop("dir '", dir, "' does not exist")
        }
    if (file.exists(file.path(dir, "filemap.txt"))) {
        trackingDir <- dir
        if (basename(dir)=="rdatadir")
            dir <- dirname(dir)
    } else {
        if (file.exists(file.path(dir, "rdatadir", "filemap.txt"))) {
            trackingDir <- file.path(dir, "rdatadir")
        } else {
            stop("can't find a tracking database: files '", file.path(dir, "filemap.txt"),
                 "' and '", file.path(dir, "rdatadir", "filemap.txt"), "' do not exist")
        }
    }
    if (is.null(name))
        name <- getAbsolutePath(dir)
    attach(what=NULL, pos=pos, name=name)
    return(track.start(trackingDir, pos=pos, readonly=readonly, lockEnv=lockEnv))
}
