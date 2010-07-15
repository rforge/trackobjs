track.detach <- function(pos=NULL, name=NULL) {
    if (is.null(pos) == is.null(name))
        stop("must specify one of pos or name")
    if (is.null(pos)) {
        pos <- match(name, search())
        if (is.na(pos))
            stop("'", name, "' is not a name on the search list")
    }
    if (length(pos)!=1 || pos < 2)
        stop("pos must have one value >= 2")
    if (!env.is.tracked(pos=pos))
        stop("environment at pos=", pos, " is not tracked")
    track.sync(pos=pos, forceFull=TRUE)
    track.stop(pos=pos)
    detach(pos=pos, unload=TRUE)
    return(invisible(NULL))
}
