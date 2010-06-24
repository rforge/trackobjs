\name{track.options}
\alias{track.options}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set and get tracking options on a tracked environment}
\description{
  Set and get tracking options on a tracked environment.  One
  independent set of tracking options exists for each tracked environment.
}
\usage{
track.options(..., pos = 1, envir = as.environment(pos), save = FALSE,
        trackingEnv, only.preprocess = FALSE, old.options = list())
}

\arguments{
  \item{\dots}{ Either option names as character data, or specifications
    for setting options, as named arguments, or in
    a named list }
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
  of specifying the environment being tracked, but should be rarely needed.}
  \item{save}{If \code{TRUE}, current options are saved to disk and will
    be used in future.  Note that all current options settings are saved,
    not just the new settings made in this call.}
  \item{trackingEnv}{ The hidden environment in which tracked objects
    are stored.  It is not necessary to supply this in normal use. }
  \item{only.preprocess}{ If \code{TRUE}, process any options
    specifications and return the full list of option settings with
    the values as specified, and defaults for all othe roptions.  Stored
    options are neither accessed nor changed.  Intended for internal use.}
  \item{old.options}{A list of old options to use, can only be suppled
    when \code{only.preprocess=TRUE}.  Intended for internal use.}
}
\details{
  Valid option names and values are as follows:
  \describe{
    \item{summaryTimes:}{logical, or integer value 0,1,2,3}
    \item{summaryAccess:}{logical, or integer value 0,1,2,3,4}
    \item{cache:}{logical (default TRUE) (keep written objects in memory?)}
    \item{writeToDisk:}{logical (default TRUE) (always write changed objects to disk?)}
    \item{useDisk:}{logical (default TRUE) if FALSE, don't write anything
    -- not implemented yet}
    \item{recordAccesses:}{logical (default TRUE) if TRUE, record
      counts and times for access ("get") operations on tracked variables}
    \item{maintainSummary:}{logical (default TRUE) if TRUE, record time & number of accesses}
    \item{alwaysSaveSummary:}{logical (default TRUE) if TRUE, always save
    the summary on any change}
    \item{RDataSuffix:}{character (default \code{"rda"}) suffix to use for files
      containing saved R objects}
}

  The option settings are saved as a list in an object called
  \code{.trackingOptions} in the tracking environment (with a copy
  mirrored to a file in the tracking dir if \code{save=TRUE}.)

  The options can be used to tune performance to resource availability (time &
  memory) and robustness in the face of machine or user error.  Some
  possible settings are:

  \describe{
    \item{maximize robustness and speed:}{ \code{cache=TRUE} and
      \code{writeToDisk=TRUE} (the default): always write an object to disk when it is
      changed, and keep a copy in memory, so that an object only needs
      to be read once}
    \item{minimize memory usage and maximize robustness:}{
      \code{writeToDisk=TRUE}, \code{cache=FALSE}: always write an
      object to disk when it is changed, and don't keep a copy in memory
      -- need to read from disk whenever the object is referred to}
    \item{maximize speed:}{ \code{writeToDisk=FALSE},
      \code{cache=TRUE}: don't write the object to disk - just keep
      a copy in memory after it is first accessed and only write it when
      \code{track.stop()} or
      one of \code{track.save()} or its friends is called.  This
      combination less robust because changed variables can be lost if R crashes,
      or the user quits R without remembering to call
      \code{track.stop()}.  This mode of operation is like the
      \code{g.data} package, but with automatically keeping track of
      which variables have been changed and need to be written to disk
      (and the writing of changed variables with one call to
      \code{track.save()} or \code{track.stop()}).}
  }
  The combination \code{writeToDisk=FALSE} and
  \code{cache=FALSE} is possible, but is unlikely to be desirable
  -- this will keep changed objects in memory, but will not keep
  merely fetched objects in memory.

  The options \code{maintainSummary}, \code{recordAccesses}, and
  \code{alwaysSaveSummary} control when the object summary is updated
  and when it is saved to disk (the default is for it to be updated and
  saved to disk for every read and write access to an object, whether or
  not the object is cached in memory).
}
\value{
  The value returned is a list of option values.  If options were
  specified as arguments, the old values of those options are returned
  (unless \code{only.preprocess=TRUE} was supplied).  If no options were
  specified as arguments, the full list of current option values is returned.
}
\author{Tony Plate <tplate@acm.org>}
\seealso{ \link[=trackObjs-package]{Overview} and \link[=track.design]{design} of the \code{trackObjs} package. }
\examples{
library(trackObjs)
unlink("tmp1", recursive=TRUE)
track.start("tmp1")
track(x <- 33)
X <- array(1:24, dim=2:4)
track.status()
track.options(cache=TRUE, writeToDisk=FALSE) # change for just this session
# different ways of retrieving option values
track.options(c("cache", "writeToDisk"))
track.options("cache", "writeToDisk")
track.options("cache")
track.options()
track(X)
# see the effect of the changed options on the status of X (X is not saved to disk)
track.status()
X[1,1,1] <- 0
track.status()
track.flush()
track.status()
track.stop()
track.start("tmp1")
# note that options previously changed are back at defaults (because default
# to track.options() is save=FALSE
track.options(c("cache", "writeToDisk"))
track.options(cache=TRUE, writeToDisk=FALSE, save=TRUE) # change the options on disk
track.options(c("cache", "writeToDisk"))
track.stop()
track.start("tmp1")
# now options previously changed are remembered (because track.options(..., save=TRUE) was used)
track.options(c("cache", "writeToDisk"))
track.stop()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
