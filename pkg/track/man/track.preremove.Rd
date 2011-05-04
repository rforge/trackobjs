\name{track.preremove}
\alias{track.preremove}
\title{
  Remove other resources associated with an object.
}
\description{
  Remove other resources associated with an object prior to its removal
  by the tracking system.  This S3 generic function exists so that
  methods can be specified for cleaning up particular objects.
}
\usage{
track.preremove(obj, objName, envir, ...)
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
