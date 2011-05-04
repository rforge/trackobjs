\name{track.preremove.ff}
\alias{track.preremove.ff}
\title{
  Remove the file associated with the tracked ff object.
}
\description{
  This function is not intended to be called by users.
  It exists so that the ff file can be removed automatically when a tracked
  ff object is removed.
}
\usage{
\method{track.preremove}{ff}(obj, objName, envir, \dots)
}
\arguments{
  \item{obj}{
    The R object that will be removed.
}
  \item{objName}{
    The name of the R object that will be removed.
}
  \item{envir}{
    The environment in which the object exists (i.e., the tracked
    environment, not the tracking
    environment.)
}
\item{\dots}{
  Other arguments.
}
}
\details{

  A \code{track.preremove()} method should clean up other resources
  (e.g., files) associated with \code{obj}.  The \code{track.remove()}
  first calls \code{track.preremove()} and then will removes \code{obj}
  from the system after the call to \code{track.preremove()} has
  returned.

}
\value{
  Return values are ignored.
}
\author{
  Tony Plate \email{tplate@acm.org}
}
\seealso{
  \link{track.remove}
}
\keyword{ data }
