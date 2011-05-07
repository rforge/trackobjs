track.preremove.ff <- function(obj, objName, envir, ...) {
    # want to delete the resource used, leave deleting the R object the caller
    delete(obj)

    # Doesn't work to remove the file directly -- the ff object might have the file open, which
    # can prevent its removal.
    # filename.ff <- file.path(".", track.datadir(envir=envir), "ff",
    #                          paste(track.filename(list=objName, envir=envir), ".ff", sep=""))
    # if (file.exists(filename.ff))
    #     file.remove(filename.ff)
}
