track.history.start <- function(file=NULL, width=NULL, style=NULL, times=NULL, message="Session start") {
    if (!is.null(file))
        options(incr.hist.file=file)
    if (!is.null(width))
        options(incr.hist.width=width)
    if (!is.null(style))
        options(incr.hist.style=style)
    if (!is.null(times))
        options(incr.hist.times=times)
    while (is.element("track.history.writer", getTaskCallbackNames()))
        removeTaskCallback("track.history.writer")
    res <- addTaskCallback(track.history.writer, name="track.history.writer")
    # write a startup comment to the file
    file <- getOption("incr.hist.file")
    if (is.null(file) || nchar(file)==0)
        file <- Sys.getenv("R_INCR_HIST_FILE")
    if (is.null(file) || nchar(file)==0)
        file <- ".Rincr_history"
    times <- getOption("incr.hist.times")
    if (is.null(times) || nchar(times)==0)
        times <- Sys.getenv("R_INCR_HIST_TIMES")
    if (is.null(times) || nchar(times)==0)
        times <- TRUE
    times <- as.logical(times)
    cat("##------*", message, "at", date(), "*------##\n", file=file, append=TRUE)
    invisible(res)
}
track.history.stop <- function() {
    if (is.element("track.history.writer", getTaskCallbackNames()))
        removeTaskCallback("track.history.writer")
    invisible(NULL)
}

track.history.load <- function(times=FALSE) {
    file <- getOption("incr.hist.file")
    if (is.null(file) || nchar(file)==0)
        file <- Sys.getenv("R_INCR_HIST_FILE")
    if (is.null(file) || nchar(file)==0)
        file <- ".Rincr_history"
    if (file.exists(file)) {
        cat("Loading incremental history file", file, "\n")
        ## Specify namespace utils to make this function work
        ## when called from .Rprofile
        if (times) {
            utils:::loadhistory(file)
        } else {
            file2 <- tempfile(file)
            writeLines(grep("^##------ .* ------##$",
                            readLines(file, -1), invert=TRUE, value=TRUE), con=file2)
            utils:::loadhistory(file2)
        }
    } else {
        cat("Incremental history file", file, "does not exist\n")
    }
}

track.history.writer <- function(expr, value, ok, visible) {
    file <- getOption("incr.hist.file")
    if (is.null(file) || nchar(file)==0)
        file <- Sys.getenv("R_INCR_HIST_FILE")
    if (is.null(file) || nchar(file)==0)
        file <- ".Rincr_history"
    style <- getOption("incr.hist.style")
    if (is.null(style) || nchar(style)==0)
        style <- Sys.getenv("R_INCR_HIST_STYLE")
    if (is.null(style) || nchar(style)==0)
        style <- "full"
    times <- getOption("incr.hist.times")
    if (is.null(times) || nchar(times)==0)
        times <- Sys.getenv("R_INCR_HIST_TIMES")
    if (is.null(times) || nchar(times)==0)
        times <- TRUE
    times <- as.logical(times)
    ## cat("Writing history to file", file, "using style=", style, "\n")
    if (style=="fast") {
        width <- getOption("incr.hist.width")
        if (is.null(width) || nchar(width)==0)
            width <- Sys.getenv("R_INCR_HIST_WIDTH")
        if (is.null(width) || nchar(width)==0)
            width <- 120
        cat(c(if (times) paste("##------", date(), "------##"),
              deparse(expr, width)), sep="\n", file=file, append=TRUE)
    } else {
        ## slow style
        file1 <- tempfile("Rrawhist")
        savehistory(file1)
        timestamp(quiet=TRUE)
        rawhist <- readLines(file1)
        unlink(file1)
        stamp.lines <- max(grep("^##------.*------##$", rawhist))
        if (is.na(stamp.lines))
            stamp.lines <- 1
        else if (!times)
            stamp.lines <- stamp.lines + 1
        if (stamp.lines <= length(rawhist))
            cat(rawhist[seq(stamp.lines, length(rawhist))], sep="\n", file=file, append=TRUE)
    }
    TRUE
}
