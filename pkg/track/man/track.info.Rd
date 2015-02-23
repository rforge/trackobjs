\name{track.info}
\alias{track.filename}
\alias{track.datadir}
\alias{track.info}
\alias{track.mem}
\alias{env.is.tracked}
\alias{tracked.envs}
\title{Return filenames and directories for tracked variables.}
\description{Return filenames and directories for tracked variables.}
\usage{
track.filename(expr, list = character(0), pos = 1,
               envir = as.environment(pos), suffix = FALSE)
track.datadir(pos = 1, envir = as.environment(pos), relative = TRUE)
track.info(pos = NULL, envir = as.environment(pos), all=is.null(pos))
track.mem(pos = NULL, envir = as.environment(pos), all=is.null(pos))
env.is.tracked(pos = 1, envir = as.environment(pos))
tracked.envs(envirs=search())
}
\arguments{
  \item{expr}{ An unquoted variable name }
  \item{list}{ A character vector of variable names }
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
    of specifying the environment being tracked, but should be rarely needed.}
  \item{suffix}{: Return the filename with the RData suffix (extension)
    (taken from \code{track.options("RDataSuffix")})}
  \item{relative}{: Return a path relative to the current working
    directory, or an absolute path?}
  \item{all}{ Return info about all tracked environments?}
  \item{envirs}{A list or vector of objects that can be interpreted as
    environments by \code{as.environment}}
}
\value{
  \describe{
    \item{track.filename()}{ returns the filenames for tracked variables.
      These names are guaranteed to be distinct for distinct variables.}
    \item{track.datadir()}{ returns the directory in which RData files
      for tracked variables are stored.}
    \item{\code{track.info}:}{ returns a dataframe of information
      (directory, readonly/writable status) about
      environments currently tracked.}
    \item{\code{track.mem}:}{ returns a dataframe of information (number
      of objects, memory usage) about
      environments currently tracked.}
    \item{env.is.tracked:}{returns \code{TRUE} or \code{FALSE}}
    \item{tracked.envs:}{with no arguments, it returns the names of
      tracked environment that are on the search list.  If given an
      argument that is a vector of environments (or environment names),
      it returns the subset of that vector that are tracked environments.}
    }
}
\note{
  The \code{track} package stores RData files in the directory returned
  by \code{track.datadir()}.  It is not advisable to write other RData
  files to that directory.  Filenames for variables may change when an
  object is deleted and then recreated.

  A warning message like "env R_GlobalEnv (pos 1 on search list) appears
  to be an inactive tracked environment, saved from another session and
  loaded here inappropriately" indicates that the environment has some
  but not all of the structure of a tracked environment.  In particular,
  the variable \code{.trackingEnv} exists in it, but does not seem to be
  connected properly.  Some of the bindings may be active bindings, but
  they may have come disconnected from the tracking environment.  The
  most common way that this kind of situation can arise is from doing
  \code{save.image()} before \code{track.stop()}, and then reloading the
  saved image (e.g., when restarting R).
  To fix this situation, do the following:
  \enumerate{
    \item \code{rm(.trackingEnv, pos=1)}
    \item \code{names(which(!sapply(ls(pos=1), bindingIsActive,
      as.environment(1))))} # to see which variables have active bindings
    \item \code{x1 <- x} # for each variable x that has an active binding
    and that you want to save
    \item \code{rm(x, pos=1)}
    \item \code{save.image()} # to overwrite the old saved .RData file
  (only works with position 1)
  }
  If the inactive tracked environment is at a position other than 1 on the
  search list, substitute the appropriate position for 1 in the above.

}
\author{Tony Plate \email{tplate@acm.org}}
\seealso{ \link[=track-package]{Overview} and \link[=track.design]{design} of the \code{track} package. }
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
track.start(dir=file.path(tempdir(), 'rdatadir4'))
x <- 33
X <- array(1:24, dim=2:4)
Y <- list(a=1:3,b=2)
X[2] <- -1
track.datadir(relative=TRUE)
track.datadir(relative=FALSE)
track.filename(list=c("x", "X"))
track.info()
track.mem()
env.is.tracked(pos=1)
env.is.tracked(pos=2)
# Would normally not call track.stop(), but do so here to clean up after
# running this example.
track.stop(pos=1, keepVars=TRUE)
}
\keyword{ data }
