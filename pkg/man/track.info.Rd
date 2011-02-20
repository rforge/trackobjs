\name{track.info}
\alias{track.filename}
\alias{track.datadir}
\alias{track.info}
\alias{env.is.tracked}
\alias{tracked.envs}
\title{Return filenames and directories for tracked variables.}
\description{Return filenames and directories for tracked variables.}
\usage{
track.filename(expr, list = character(0), pos = 1, envir = as.environment(pos), suffix = FALSE)
track.datadir(pos = 1, envir = as.environment(pos), relative = TRUE)
track.info(pos = 1, envir = as.environment(pos), all=TRUE)
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
    \item{\code{track.info}:}{ returns a dataframe of information about
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
}
\author{Tony Plate \email{tplate@acm.org}}
\seealso{ \link[=track-package]{Overview} and \link[=track.design]{design} of the \code{track} package. }
\examples{
library(track)
unlink("tmp1", recursive=TRUE)
track.start("tmp1")
x <- 33
X <- array(1:24, dim=2:4)
Y <- list(a=1:3,b=2)
X[2] <- -1
track.datadir(relative=TRUE)
track.filename(list=c("x", "X"))
env.is.tracked(pos=1)
env.is.tracked(pos=2)
track.stop(pos=1)
unlink("tmp1", recursive=TRUE)
}
\keyword{ data }
