\name{track.ff}
\alias{track.ff}
\alias{track.ff.filename}
\alias{track.ff.rm}
\alias{track.ff.remove}
\alias{track.ff.delete}
\title{
  Manage \code{ff} objects in a tracking data base
}
\description{
  Create or remove \code{ff} objects in a tracking data base.
  \code{track.ff.remove} and \code{track.ff.delete} are synonyms for  \code{track.ff.rm}.
}
\usage{
track.ff(expr, pos = 1, envir = as.environment(pos))
track.ff.rm(expr, pos = 1, envir = as.environment(pos))
track.ff.filename(expr, pos = 1, envir = as.environment(pos), relative = TRUE)
track.ff.remove(expr, pos = 1, envir = as.environment(pos))
track.ff.delete(expr, pos = 1, envir = as.environment(pos))
}
\arguments{
  \item{expr}{
    For \code{track.ff}, either the name of an existing non-ff object or
    an expression that evaluates to one, or an assignment expression
    whose RHS is a call to \code{ff()} or \code{as.ff()}.
  }
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
    of specifying the environment being tracked, but should be rarely needed.}
  \item{relative}{ Should the filename be returned as an absolute or
    relative path?}
}
\details{

The functions in \code{track.ff} create or remove ff objects in a
tracked environment.  The ff objects are made permanent.  Currently, the
underlying data files of existing temporary (or permanent) ff objects
cannot be moved into the ff data storage in tracked environment (merely
because this has not been implemented, not because of any particular
difficulty.)  The data files backing the ff objects are stored in the
\code{ff} subdirectory of the tracking database.

The functions in \code{track.ff} do not interfere or intervene in the
creation or use of \code{ff} objects other than by use of the
\code{track.ff} functions.
}

\note{

  The \code{track} package does have some minor special treatment for
  \code{ff} objects: by default it does not flush them from memory.
  This is because when \code{ff} objects are flushed from memory, the
  \code{ff} objects are treated as though they are being deleted, and
  C-level destructors are called.
  
}

\value{
  \describe{
    \item{track.ff()}{invisibly returns the ff object}
    \item{track.ff.filename()}{returns the name of the file containing the
      data of the ff object (including the path to the file)}
    \item{track.ff.rm()}{invisibly returns the name of the ff object removed}
  }
}
\author{Tony Plate \email{tplate@acm.org}}
\seealso{
  The \link[ff:ff]{ff} package. and the \link[track:track-package]{track} package.
}
\examples{
library(ff)
library(track)
library(track.ff)
unlink("tmp1", recursive=TRUE)
track.start("tmp1")
y <- 33
# convert existing objects to ff objects
x1 <- matrix(1:6, nrow=3, dimnames=list(LETTERS[1:3], letters[24:25]))
track.ff(x1)
x2 <- matrix(1:9, nrow=3, dimnames=list(LETTERS[1:3], letters[24:26]))
track.ff(x2 <- as.ff(x2))
# various ways of creating new ff object
track.ff(x3 <- matrix(1:12, nrow=3, dimnames=list(LETTERS[1:3], letters[22:25])))
track.ff(x4 <- ff(1:12, dim=3:4, dimnames=list(LETTERS[1:3], letters[22:25])))
track.stop()
unlink("tmp1", recursive=TRUE)
}
\keyword{ data }
\keyword{ database }
\keyword{ utilities }
