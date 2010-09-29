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
    options(incr.hist.active=TRUE)
    # write a startup comment to the file
    file <- getOption("incr.hist.file")
    if (is.null(file) || nchar(file)==0)
        file <- Sys.getenv("R_INCR_HIST_FILE")
    if (is.null(file) || nchar(file)==0)
        file <- ".Rincr_history"
    cat("##------*", message, "at", date(), "*------##\n", file=file, append=TRUE)
    invisible(res)
}

track.history.stop <- function() {
    if (is.element("track.history.writer", getTaskCallbackNames()))
        removeTaskCallback("track.history.writer")
    options(incr.hist.active=FALSE)
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
    method <- "last" # set the internal algorithm
    ## Always write time stamps to the incremental history file -- but
    ## do it in a way that prevents the time stamps from appearing in
    ## the interactive history.
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
    ## "full" style is nicer because it gets the commands
    ## as typed (incl formatting & comments), while "fast" style gets
    ## the deparsed form of the parse expression.
    ## But, haven't yet programmed a way of identifying the full
    ## command without inserting time stamps into the history, so
    ## keep the "fast" style until figure that out.
    ## DOUBLE BUT: there is a bug in R-2.11.1 that makes R crash
    ## if this function is called back after sourcing an empty
    ## file (because it leaves expr=0x0 in the C-code.)
    ## So, turn the default back to "full"
    if (style=="fast") {
        ## Fast style deparse last expr.
        ## This will cause R-2.11.1 to crash after sourcing an empty file!!
        width <- getOption("incr.hist.width")
        if (is.null(width) || nchar(width)==0)
            width <- Sys.getenv("R_INCR_HIST_WIDTH")
        if (is.null(width) || nchar(width)==0)
            width <- 120
        cat(c(if (times) paste("##------", date(), "------##"),
              deparse(expr, width)), sep="\n", file=file, append=TRUE)
    } else {
        ## Slow style -- write raw history out to a file, then read it
        ## back in and identify the last command, which can be multi-line.
        file1 <- tempfile("Rrawhist")
        savehistory(file1)
        ## There are multiple ways of doing this:
        ## (1) use timestamps - makes things easy.  However what's not nice
        ## about timestamp() is that the time stamps appear within the
        ## interactive history in the R session, which is just annoying.
        ## (2) remember the previous command, and look for that.
        ## (3) look for the last complete R expression in the file.
        ## (4) look for the last expression in the file that matches
        ## the value of the expr arg to this function.
        ## For the moment, use timestamp().
        if (method=="timestamps")
            timestamp(quiet=TRUE)
        ## cat(c(if (times) paste("##------", date(), "------##"),
        ##      deparse(expr, width)), sep="\n", file=file, append=TRUE)
        rawhist <- readLines(file1)
        unlink(file1)
        if (method=="timestamps") {
            posns <- grep("^##------.*------##$", rawhist)
            stamp.lines <- NA
            if (length(posns))
                stamp.lines <- max(posns)
            if (is.na(stamp.lines))
                stamp.lines <- 1
            else if (!times)
                stamp.lines <- stamp.lines + 1
            if (stamp.lines <= length(rawhist))
                cat(rawhist[seq(stamp.lines, length(rawhist))], sep="\n", file=file, append=TRUE)
        } else {
            ## Use the last lines to limit the possible starts
            last <- history.last.lines$get()
            lines <- find.expr.lines.following(rawhist, last)
            if (length(lines)) {
                cat(c(if (times) paste("##------", date(), "------##"),
                      lines), sep="\n", file=file, append=TRUE)
                history.last.lines$set(lines)
            }
        }
    }
    TRUE
}

find.expr.lines.following <- function (rawhist, last) {
   pstarts <- integer(0)
   if (length(last)) {
       last.starts <- which(last[1] == rawhist)
       pstarts <- rep(NA, length(last.starts))
       if (length(last) > 1) {
           for (i in seq(along = last.starts)) {
               if (last.starts[i] + length(last) < length(rawhist) &&
                 all(last == rawhist[seq(last.starts[i], len = length(last))]))
                 pstarts[i] <- last.starts[i] + length(last)
           }
       }
       else {
           pstarts <- last.starts + 1
           pstarts[pstarts > length(rawhist)] <- NA
       }
       pstarts <- pstarts[!is.na(pstarts)]
   }
   for (i in rev(pstarts)) {
       lines <- rawhist[seq(from = i, to = length(rawhist))]
       if (!is(try(parse(text = lines), silent = TRUE), "try-error"))
           return(lines)
   }
   if (length(pstarts))
       return(rawhist[seq(from=max(pstarts), to=length(rawhist))])
   # There was no match - look for the most recent full expression in the last 20 lines
   pstarts <- seq(to = length(rawhist), len = min(20, length(rawhist)))
   for (i in rev(pstarts)) {
       lines <- rawhist[seq(from = i, to = length(rawhist))]
       if (!is(try(parse(text = lines), silent = TRUE), "try-error"))
           return(lines)
   }
   # If we can't find any parsable expression in a tail of the history file,
   # and out last history entry written doesn't match the history file,
   # then just return the last line of the history file.
   # A condition under which we would get here is that there was a single
   # command longer than the current history length, so that none of the
   # previous command is in the history, and the history is not parsable.
   # This would only happen under cutting-and-pasting large chunks of code,
   # in which case the user probably wouldn't care about the history
   # anyway (and in any case, the ordinary history mechanism is not keeping
   # it.)
   return(rawhist[length(rawhist)])
}

## Set up a function closure to remember the last history lines,
## along the lines of demo("closure").
## Use it like this:
## > history.last.lines$set("foo")
## > history.last.lines$get()
## [1] "foo"
history.last.lines <- (function(last=NULL) list(set=function(lines) last <<- lines,
                                               get=function() last))()

