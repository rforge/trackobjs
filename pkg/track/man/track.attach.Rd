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
track.attach(dir, pos = 2, name = NULL,
             create = FALSE, readonly = !create,
             lockEnv = FALSE, verbose = TRUE, auto = NULL)
track.detach(pos = NULL, name = NULL, detach = TRUE)
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
  \item{create}{
    Should the tracking database be created if it does not exist?
}
\item{readonly}{
  Logical flag indicating whether the tracking db should be attached in
  a readonly mode.  The global environment (\code{pos=1} in the search
  path) cannot be tracked in a readonly mode.
}
  \item{lockEnv}{Should the environment be locked for a readonly
  tracking environment? The default is \code{FALSE} because locking the
  environment is irreversible, and it prevents rescanning or caching
  (because can't delete or add bindings)}
  \item{verbose}{print a message about what directory is being tracked?}
  \item{detach}{ If \code{TRUE}, the environment attached to the search
  path (in a position other than 2) will be detached after stopping
  tracking, IF it was created by \code{track.attach()} and if there are no
  variables left remaining in the environment after removing
  all tracked variables.  If \code{detach="force"}, the attached
  environment will be removed even if there are variables remaining in
  it (though not if it was not created by \code{track.attach}).}
  \item{auto}{
    Should auto-tracking be used? (see \code{\link{track.start}}).
}
}
\details{
  \code{track.attach} attaches a new environment to the search path at the specified
  position and variables in the tracking database are made available in
  it through lazy loading.  Using \code{readonly==TRUE} ensure that no
  changes at all are made to the tracking database, and the environment
  is locked to ensure that variables cannot be created or deleted.

  \code{track.detach} syncs all variables to files and detaches the
  environment from the search list if it is empty.  See
  \code{\link{track.stop}} for conditions under which the
  environment may not be detached.
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
