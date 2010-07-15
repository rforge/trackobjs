\name{track.attach}
\alias{track.attach}
\alias{track.detach}
\title{
  Attach a tracking database to the search path.
}
\description{
  Attach a tracking database to the search path at a position other than
  1.  Variables in the tracking database are made available through a
  new environment attached at \code{pos} on the search path.
}
\usage{
track.attach(dir, pos = 2, name = NULL)
track.detach(pos = NULL, name = NULL)
}
\arguments{
  \item{dir}{
    The directory where the tracking database resides.
}
\item{pos}{
  The position on the search path to attach the new environment at, or
  where it is currently attached.
}
\item{name}{
  The name to use on the search path for the new environment.
}
}
\details{
  \code{track.attach} attaches a new environment to the search path at the specified
  position and variables in the tracking database are made available in
  it through lazy loading.

  \code{track.detach} syncs all variables to files and detaches the
  environment from the search list.
}
\value{
NULL
}
\author{
  Tony Plate \email{tplate@acm.org}
}
\seealso{
  \code{\link{track.start}}
}
\examples{
\dontrun{
track.attach("path/to/tracking-database", pos=2, name="trackdb")
track.detach(pos=2)
}
}
\keyword{ data }
