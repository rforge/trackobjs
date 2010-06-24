\name{track.setup}
\alias{track.start}
\alias{track.stop}
\alias{track.restart}
\alias{env.is.tracked}
\alias{tracked.envs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Setup and stop tracking }
\description{
  Functions to setup and stop tracking.
}
\usage{
track.start(dir="rdatadir", pos = 1, envir = as.environment(pos), create = TRUE,
        clobber = c("no", "files", "variables"), cache=NULL, options=NULL,
        RDataSuffix=NULL, auto = NULL)
track.stop(pos = 1, envir = as.environment(pos), all=FALSE, stop.on.error=FALSE)
env.is.tracked(pos = 1, envir = as.environment(pos))
tracked.envs(envirs=search())
track.restart(pos = 1, envir = as.environment(pos), forget.modified = FALSE)
}

\arguments{
  \item{dir}{ The directory where tracking data is stored }
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
  of specifying the environment being tracked, but should be rarely needed.}
  \item{create}{ If \code{TRUE}, create the tracking directory if it
    doesn't exist }
  \item{clobber}{ Controls action taken when there are objects of the
    same name in the
    tracking directory and in \code{envir}: \code{"no"} means stop; \code{"files"} means use the
    version from the tracking directory; and \code{"variables"} means
    use the version in \code{envir} (and overwrite the version in the
    tracking directory)}
  \item{cache}{ Should objects be keep cached in memory?  Default is \code{FALSE}.  This option is a shorthand way of supplying \code{options=list(cache=TRUE)}.}
  \item{options}{ Option values (as a list) to be used for this tracking database.  See \code{track.options()}.}
  \item{all}{If \code{TRUE}, all tracked environment are unlinked}
  \item{auto}{If \code{TRUE}, automatically track new variables and
  deleted variables in the environment (through use of a task
  callback).  If \code{auto==NULL}, take the value from
  \code{getOptions("global.track.options")$autoTrack}, and if that is
  \code{NULL}, use \code{TRUE}}
  \item{stop.on.error}{If \code{FALSE}, failures to unlink a tracking
    environment are ignored, though a warning message is printed}
  \item{envirs}{A list or vector of objects that can be interpreted as
    environments by \code{as.environment}}
  \item{forget.modified}{ If \code{TRUE}, discard copies that are
    modified and in memory }
  \item{RDataSuffix}{The suffix to use for \code{RData} files.  This should not normally need to be specified.}
}
\details{
  \describe{
    \item{\code{track.start}:}{ start tracking \code{envir}.  If the
      tracking directory already exists, objects in it will be made
      accessible, otherwise it will be created (unless \code{create=FALSE}).}
    \item{\code{track.stop}:}{ stop tracking \code{envir}.  Tracked
      variables will become unavailable (any unsaved values will be
      saved to files first.)}
    \item{\code{track.restart}:}{ Rescan the tracking dir, so that if anything has changed there,
the current variables on file will be used instead of any cached
in memory.
If we have some modified variables cached in memory but not saved
to disk, this function will stop with an error unless
\code{forget.modified==TRUE}.
Variables that have disappeared from the tracking dir will disappear
from visibility, and variables added to the tracking dir will become
available.
}
}
}
\value{
    \item{track.start, track.stop, track.restart:}{all return \code{invisible(NULL)} (this
      may change if it becomes clear what useful return values would be)}
    \item{env.is.tracked:}{returns \code{TRUE} or \code{FALSE}}
    \item{tracked.envs:}{with no arguments, it returns the names of
      tracked environment that are on the search list.  If given an
      argument that is a vector of environments (or environment names),
      it returns the subset of that vector that are tracked environments.}
}
\author{Tony Plate <tplate@acm.org>}
\seealso{ \link[=trackObjs-package]{Overview} and \link[=track.design]{design} of the \code{trackObjs} package. }
\examples{
library(trackObjs)
unlink("tmp1", recursive=TRUE)
track.start("tmp1")
x <- 33
X <- array(1:24, dim=2:4)
Y <- list(a=1:3,b=2)
X[2] <- -1
track.summary(time=0, access=1, size=FALSE)
env.is.tracked(pos=1)
env.is.tracked(pos=2)
ls(all=TRUE)
track.stop()
ls(all=TRUE)
track.start("tmp1")
ls(all=TRUE)
track.summary(time=0, access=1, size=FALSE)
track.stop()
unlink("tmp1", recursive=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
