\name{track.performance}
\alias{track.performance}
\docType{package}
\title{
  Performance tuning with track.
}
\description{
  Performance tuning with track involves trading off memory use for
  faster access times to objects.  Access time is fastest when all
  objects are cached in memory, but memory can be exhausted if this is
  done.  Memory use is minimized when objects are not cached in memory
  at all, but then a file must be read or written each time an object is
  referenced, and the whole file must be read or written even if only a
  small part of the object is actually used or changed.

  The default mode of operation of track balances memory use and
  access times by keeping objects in memory for the duration of a
  top-level task, and flushing them out at the end of the task.

  Three options (see \link{track.options}) control performance:
  \itemize{
    \item{cache}: \code{TRUE}/\code{FALSE} should variables be cached at all?
    \item{cachePolicy}: \code{tltPurge}/\code{none} higher level
    policy for maintaining cache
    \item{writeToDisk}: \code{TRUE}/\code{FALSE} should objects be
    written after a change?
  }

  Useful possible combinations of settings are: \itemize{
  \item{cache=TRUE, cachePolicy="tltPurge", writeToDisk=TRUE}:
  (DEFAULT) keep objects in memory for the duration of a task; flush
  and/or write to disk at the end of task
  \item{cache=TRUE, cachePolicy="none", writeToDisk=TRUE}:
  keep all objects in memory until removed with
  \code{\link{track.flush}}; write changed objects to disk immediately
  \item{cache=TRUE, cachePolicy="none", writeToDisk=FALSE}:
  keep all objects in memory until removed with
  \code{\link{track.flush}}; don't automatically write changed objects
  to disk (use \code{\link{track.save}})
  \item{cache=FALSE, cachePolicy="none", writeToDisk=TRUE}:
  keep no objects in memory; write changed objects to disk immediately
}

Performance tuning is a possible area of future development of the
track package, at as version 0.9-9, the defaults settings of
\code{cache=TRUE}, \code{cachePolicy="tltPurge"}, and
\code{writeToDisk=TRUE} work well.  However, smarter caching based on
access patterns to objects is certainly possible.
}
\seealso{ \link[=track-package]{Overview} and \link[=track.design]{design} of the \code{track} package. }
\keyword{ data }
