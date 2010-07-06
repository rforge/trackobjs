\name{track.sync}
\alias{track.sync}
\title{
  Synchronize the tracking database to reflect new and/or deleted objects
}
\description{
  Synchronize the tracking database to reflect new and/or deleted
  objects.  This function is intended to be called by a task callback so
  that the tracking database keeps up with new and deleted object
  automatically.  The appropriate task callback is installed by
  \code{track.start(..., auto = TRUE)}.
}
\usage{
track.sync(pos = 1, envir = as.environment(pos),
           trackingEnv = getTrackingEnv(envir), forceFull = TRUE)
}
\arguments{
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
  of specifying the environment being tracked, but should be rarely needed.}
  \item{trackingEnv}{The environment that contains data for the tracking database.}
  \item{forceFull}{If \code{TRUE}, do a full check, which involves
    checking that all apparently tracked variables do in fact have an
    active binding.}
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
}
\value{
  Returns an invisible list with the following components:
  \item{new}{character vector of names of new variables}
  \item{removed}{character vector of names of variables that were removed}
}
\note{
  The check that all apparently tracked variables have an active binding
  currently only checks that there is an active binding -- there is no
  way (at the R level) to check that the active binding is the correct one.
}
\author{
  Tony Plate <tplate@acm.org>
}
\seealso{ \link[=trackObjs-package]{Overview} and \link[=track.design]{design} of the \code{trackObjs} package. }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
