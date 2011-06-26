\name{track.sync}
\alias{track.sync}
\title{
  Synchronize the tracking database stored in files on disk to reflect
  changed, new, or deleted objects in R
}
\description{
  Synchronize the tracking database to reflect new and/or deleted
  objects.  This function is intended to be called by a task callback so
  that the tracking database keeps up with new and deleted object
  automatically.  The appropriate task callback is installed by
  \code{track.start(..., auto = TRUE)}.

  This function differs from \code{\link{track.rescan}()} in that
  \code{track.rescan()} updates R view of the database to agree with
  changes on disk, while \code{track.sync()} is primarily intended to
  go the other way (to make the disk database agree with R).
}
\usage{
track.sync(pos = 1, master=c("auto", "envir", "files"), envir = as.environment(pos),
           trackingEnv = getTrackingEnv(envir), full = TRUE,
           dryRun = FALSE, taskEnd = FALSE)
}
\arguments{
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{master}{ What to treat as the master for the synchronization.
    For a readonly tracked environment, the default \code{auto} will
    default to \code{"files"} (the only sensible interpretation).
    Otherwise, must be supplied (to avoid accidents with users mis-remembering the default.)
    When \code{master="envir"}, changes are propagated from the
    R environment to the file system.
    When \code{master="files"}, the R environment is made to reflect the
    file system (differences could result from changes to the R
    environment, or from changes to the file system, e.g., by another R
    process changing the database).}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
  of specifying the environment being tracked, but should be rarely needed.}
  \item{trackingEnv}{The environment that contains data for the tracking database.}
  \item{full}{If \code{TRUE}, do a full check, which involves
    checking that all apparently tracked variables do in fact have an
    active binding.  If \code{NA}, only do a full check if more than
    \code{track.options("autoTrackFullSyncWait")} seconds have passed
    since the last full check (because this check can be slow when there
    are many variables in the environment.}
  \item{dryRun}{If \code{TRUE}, no changes are made to either the file
    system or to the R environment, but changes that would be made are
    printed out.  Note that a change to a file for a tracked variable
    will not be detect.}
  \item{taskEnd}{Should be \code{TRUE} when called at the end of a
    top-level command task (i.e., when called by the task callback
    handler.)}
}
\details{
    Synchronizing the tracking database with the contents of the
    environment involves three tasks:
    \enumerate{
      \item start tracking new untracked variables
      \item for objects that have disappeared from the environment,
      delete them from the tracking database
      \item check that all apparently tracked variables do in fact have an
    active binding
  }

  Currently, this function will not correctly handle the case where
  \code{master="files"} and where objects are cached and an underlying
  file is changed.
}
\value{
  Returns an invisible list with the following components:
  \item{new}{character vector of names of new variables}
  \item{removed}{character vector of names of variables that were removed}
}
\note{
  The check that all apparently tracked variables have an active binding
  currently only checks that the variable has an active binding -- there is no
  way (at the R level) to check that the active binding is the correct one.
}
\author{
  Tony Plate <tplate@acm.org>
}
\seealso{ \code{\link{track.rescan}} for rescanning a tracked database
  after the files on disk have changed (this is usually only used for
  tracked environment attached at a position 2 or greater.)

  \link[=track-package]{Overview} and \link[=track.design]{design} of
  the \code{track} package.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
