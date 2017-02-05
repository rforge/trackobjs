\name{track.setup}
\alias{track.start}
\alias{track.stop}
\alias{track.rescan}
\alias{track.Last}
% \alias{.Last}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Setup and stop tracking }
\description{
  Functions to setup and stop tracking, and resync to a changed disk db
}
\section{Simple Usage}{
These functions have many arguments providing much control over
tracking, but the arguments used in simple usage are:
  \preformatted{
track.start()
track.start(dir = "rdatadir")
track.stop(pos = 1, all = FALSE)
track.rescan(pos = 2)
}
}
\usage{
track.start(dir="rdatadir", pos = 1, envir = as.environment(pos),
        forceDirName = FALSE, create = TRUE,
        clobber = c("no", "files", "variables", "vars", "var"),
        discardMissing = FALSE,
        cache = NULL, cachePolicy = NULL, options = NULL,
        RDataSuffix = NULL, auto = NULL, readonly = FALSE,
        lockEnv = FALSE, check.Last = TRUE, autoCheckSize = 1e6, verbose = TRUE)
track.stop(pos = 1, envir = as.environment(pos), all = FALSE,
        stop.on.error = FALSE, keepVars = FALSE, sessionEnd = FALSE,
        verbose = TRUE, detach = TRUE, callFrom = NULL)
track.rescan(pos = 1, envir = as.environment(pos), discardMissing = FALSE,
        forgetModified = FALSE, level = c("low", "high"), dryRun = FALSE,
        verbose = TRUE)
track.Last()
% .Last()
}

\arguments{
  \item{dir}{ The directory where tracking data is stored }
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
    of specifying the environment being tracked, but should be rarely
    needed.}
  \item{forceDirName}{If \code{FALSE}, add a \code{rdatadir} component
  to the end of the directory path if one is not already there.  If
  \code{TRUE}, use the supplied dir without modification.}
  \item{create}{ If \code{TRUE}, create the tracking directory if it
    doesn't exist }
  \item{clobber}{ Controls action taken when there are objects of the
    same name in the
    tracking directory and in \code{envir}: \code{no} means stop (unless
  objects are have the same values and are small); \code{files} means use the
    version from the tracking directory; and \code{variables},
  \code{vars} or \code{var} means
    use the version in \code{envir} (and overwrite the version in the
    tracking directory).  See also argument \code{autoCheckSize}.}
  \item{discardMissing}{ Discard all information about objects whose save
    file is missing?}
  \item{cache}{ Should objects be keep cached in memory?  Default is \code{TRUE}.  This option is a shorthand way of supplying \code{options=list(cache=...)}.}
  \item{cachePolicy}{ Policy for keeping cached objects in memory.
  Default is \code{tltPurge}, for which cached objects are removed
  from memory at the end of a top-level task.  This option is a shorthand way of supplying \code{options=list(cachePolicy=...)}.}
  \item{options}{ Option values (as a list) to be used for this tracking database.  See \code{track.options()}.}
  \item{all}{If \code{TRUE}, all tracked environment are unlinked}
  \item{auto}{If \code{TRUE}, automatically track new variables and
  deleted variables in the environment (through use of a task
  callback).  If \code{auto==NULL}, take the value from
  \code{getOptions("global.track.options")$autoTrack}, and if that is
  \code{NULL}, use \code{TRUE}}
  \item{readonly}{
    Logical flag indicating whether the tracking db should be attached in
    a readonly mode.  The global environment (\code{pos=1} in the search
    path) cannot be tracked in a readonly mode.
  }
  \item{stop.on.error}{If \code{FALSE}, failures to unlink a tracking
    environment are ignored, though a warning message is printed}
  \item{keepVars}{If \code{FALSE}, all tracked variables are removed and
    will be no longer accessible.  If \code{TRUE}, tracked variables will
    be left as ordinary variables in the environment, as well as remaining
    in files.}
  \item{detach}{ If \code{TRUE}, the environment attached to the search
  path (in a position other than 2) will be detached after stopping
  tracking, IF it was created by \code{track.attach()} and if there are no
  variables left remaining in the environment after removing
  all tracked variables.  If \code{detach="force"}, the attached
  environment will be removed even if there are variables remaining in
  it (though not if it was not created by \code{track.attach}).}
  \item{callFrom}{A character string used in a message saying where
  \code{track.stop()} was called from.}
  \item{forgetModified}{ If \code{TRUE}, discard the versions of objects that are
    modified and in memory }
  \item{RDataSuffix}{The suffix to use for \code{RData} files.  This
    should not normally need to be specified.}

  \item{lockEnv}{Should the environment be locked for a readonly
  tracking environment? The default is \code{FALSE} because locking the
  environment is irreversible, and it prevents rescanning or caching
  (because can't delete or add bindings)}

  \item{check.Last}{By default, a warning is issued if the \code{.Last}
  function in the \code{track} package is masked by any other
  \code{.Last} function.  Supplying \code{check.Last=FALSE}
  inhibits this check and warning.}

  \item{autoCheckSize}{If there are objects with the same name in an
  existing tracking db being attached, and the environment, and \code{clobber='no'},
  \code{track.start()} will check whether the objects are the same
  (using \code{identical()}).  If they are the same,
  \code{track.start()} will proceed.  This check is only performed if
  the total size of the objects, and the files, is less than
  \code{autoCheckSize} bytes.
  }

\item{level}{Should the rescan be done at a high or low level: a high
  level just stops tracking and restarts it; a low level tries to
  individually bring the environment in line with the file database.}
  \item{dryRun}{If \code{TRUE}, no changes are actualy made, but
  messages are printed describing what changes would be made.}
  \item{sessionEnd}{If \code{TRUE}, this is a call at the end of a
    session and no recovery from errors is possible -- just try to
    as best as can to save objects as appropriate.}
  \item{verbose}{print a message about what directory is being tracked?}
}
\details{
  \describe{
    \item{\code{track.start}:}{ start tracking \code{envir}.  If the
      tracking directory already exists, objects in it will be made
      accessible, otherwise it will be created (unless \code{create=FALSE}).}

    \item{\code{track.stop}:}{ stop tracking \code{envir} (default is
      the global environment).  Unsaved
      values will be saved to files first.  Tracked variables will
      become unavailable unless \code{keepVars=TRUE} is supplied. If no
      arguments are supplied, stops tracking the global environment
      (pos=1).
      (In standard use, there is not a problem with only calling
      \code{track.stop()} prior
      to quitting R, thinking that it will cleanup all tracked
      environments, because tracked envs at positions other than 1 will
      be attached readonly.)}

\item{\code{track.rescan}:}{ Rescan the tracking dir, so that if anything has changed there,
the current variables on file will be used instead of any cached
in memory.
If we have some modified variables cached in memory but not saved
to disk, this function will stop with an error unless
\code{forgetModified==TRUE}.
Variables that have disappeared from the tracking dir will disappear
from visibility, and variables added to the tracking dir will become
available.
}
    \item{.Last:}{\code{track.start()} or \code{track.attach()} set
      \code{.Last} in the global env to have the value
      \code{track.Last}, provided \code{.Last} does not already exist
      there.  \code{.Last} will be called
      at the end of an R session, before the remaining variables in the
      global environment are saved to \code{.RData}.  \code{track.Last}
      stops tracking all tracking db's, and removes tracked vars from
      their environments.
}
}
}
\value{
    \item{track.start, track.stop, track.rescan:}{all return \code{invisible(NULL)} (this
      may change if it becomes clear what useful return values would be)}
    \item{track.Last:}{calls \code{track.stop(all=TRUE)} to ensure
      that all tracking information and objects are written to files,
   and removes tracked variables from the environment.}
}
\author{Tony Plate \email{tplate@acm.org}}
\seealso{
  \link[=track-package]{Overview} and \link[=track.design]{design} of
  the \code{track} package.
}
\examples{
##############################################################
# Warning: running this example will cause variables currently
# in the R global environment to be written to .RData files
# in a tracking database on the filesystem under R's temporary
# directory, and will cause the variables to be removed temporarily
# from the R global environment.
# It is recommended to run this example with a fresh R session
# with no important variables in the global environment.
##############################################################

library(track)
track.start(dir=file.path(tempdir(), 'rdatadir9'))
x <- 33
X <- array(1:24, dim=2:4)
Y <- list(a=1:3,b=2)
X[2] <- -1
track.datadir(relative=TRUE)
track.filename(list=c("x", "X"))
track.summary(time=0, access=1, size=FALSE)
env.is.tracked(pos=1)
env.is.tracked(pos=2)
ls(all=TRUE)
track.stop(pos=1)
ls(all=TRUE)
track.start(dir=file.path(tempdir(), 'rdatadir9'))
ls(all=TRUE)
track.summary(time=0, access=1, size=FALSE)
# Would normally not call track.stop(), but do so here to clean up after
# running this example.
track.stop(pos=1, keepVars=TRUE)
}
\keyword{ data }
