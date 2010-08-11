track.Last.sys <- function() {
    if (!isTRUE(options("global.track.options")$inhibit.Last))
        track.stop(all=TRUE, sessionEnd=TRUE)
}
.Last.sys <- track.Last.sys
