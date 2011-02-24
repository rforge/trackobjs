\name{track.auto}
\alias{track.auto}

\title{
  Query or set the status of automated tracking
}
\description{
  Query or set the status of automated tracking
}
\usage{
track.auto(auto = NULL, pos = 1, envir = as.environment(pos))
}
\arguments{
  \item{auto}{
    \code{NULL} the default (to query the status of automatic tracking),
  or logical (\code{TRUE} or \code{FALSE}) to set it.
}
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
  of specifying the environment being tracked, but should be rarely needed.}
}
\value{
  A logical value, indicating whether automatic tracking is on or off.
}
\author{
  Tony Plate \email{tplate@acm.org}
}
\seealso{
\link{track.start}
}
\examples{
\dontrun{
track.auto() # query the status of auto-tracking
track.auto(FALSE) # turn auto-tracking off
track.auto(TRUE) # turn auto-tracking on
}
}
\keyword{ data }
