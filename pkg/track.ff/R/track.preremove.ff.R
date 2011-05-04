track.preremove.ff <- function(obj, objName, envir, ...) {
    filename.ff <- file.path(".", track.datadir(envir=envir), "ff",
                             paste(track.filename(list=objName, envir=envir), ".ff", sep=""))
    if (file.exists(filename.ff))
        file.remove(filename.ff)
}
