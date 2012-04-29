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
track.ff(expr, pos = 1, envir = as.environment(pos), varname = NULL)
track.ff.rm(expr, pos = 1, envir = as.environment(pos), list = NULL)
track.ff.remove(expr, pos = 1, envir = as.environment(pos), list = NULL)
track.ff.delete(expr, pos = 1, envir = as.environment(pos), list = NULL)
track.ff.filename(expr, pos = 1, envir = as.environment(pos), relative = TRUE, list = NULL)
}
\arguments{
  \item{expr}{
    For \code{track.ff}, either the name of an existing non-ff object or
    an expression that evaluates to one, or an assignment expression
    whose RHS is a call to \code{ff()} or \code{as.ff()}, or, when
    \code{varname} is also supplied, just a call to \code{ff()} or \code{as.ff()}.
  }
  \item{varname}{The name of the ff object to create.}
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
    of specifying the environment being tracked, but should be rarely needed.}
  \item{relative}{ Should the filename be returned as an absolute or
    relative path?}
  \item{list}{A vector object names to operate one (must be of length 1
    for \code{track.ff.filename()}.}
}
\details{

The functions in \code{track.ff} create or remove ff objects in a
tracked environment, storing the \code{ff} data file in the subdirectory
\code{ff} in the tracking directory (i.e., by default
\code{rdatadir/ff}).  \code{track.ff()} makes ff objects permanent, by
virtue of telling \code{ff()} an explicit directory and filename for the
data.  Currently, the underlying data files of existing temporary (or
permanent) ff objects cannot be moved into the ff data storage in
tracked environment (merely because this has not been implemented, not
because of any particular difficulty.)  The data files backing the ff
objects are stored in the \code{ff} subdirectory of the tracking
database.

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

  For \code{ff} objects created not using \code{track.ff()}, a warning
  about opening the datafile will usually be given the first time the
  \code{ff} object is used after being created.  This warning happens
  because after it is created, the auto-track functionality moves the
  \code{ff} R object from the environment where it was created to the
  tracking environment, and assigns an active binding to provide access
  to the object.  This sequence of events is interpreted by the
  \code{ff} system as deletion of the \code{ff} R object, and the data
  file connection is closed.  Other than producing output, this warning
  is harmless.

  To completely prevent the \code{track} package from touching \code{ff}
  objects, do \code{track.options(autoTrackExcludeClass="ff")}.

}
\section{warning}{

  The \code{track.copy()} and \code{track.move()} functions in the
  \code{track} package do not know about the data file associated with
  \code{ff} objects and will leave it where it is (as of package version
  \code{track-1.0.3}).  The \code{ff} objects should continue to work as
  long as the data files remain available.  It is intended to change
  this behavior to move the \code{ff} data files along with the tracking
  files.

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
track.ff(varname='x5', ff(1:12, dim=4:3, dimnames=list(LETTERS[1:4], letters[23:25])))
track.stop()
unlink("tmp1", recursive=TRUE)
}
\keyword{ data }
\keyword{ database }
\keyword{ utilities }
