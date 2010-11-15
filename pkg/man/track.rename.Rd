\name{track.rename}
\alias{track.rename}
\title{
  Rename variables in a tracked environment
}
\description{
  Rename variables in a tracked environment
}
\usage{
track.rename(old, new, pos = 1, envir = as.environment(pos), clobber = FALSE, verbose = TRUE)
}
\arguments{
  \item{old}{
    character vector of old names
}
  \item{new}{
    character vector of new names (must be same length as 'old')
}
  \item{pos}{
    The position on the search list of the tracked db.  Can be numeric or
    the name as returned by \code{search()}.
}
\item{envir}{
  The tracked environment.
}
\item{clobber}{
  Should existing variables be overwritten?
}
  \item{verbose}{
Write out what is being done?
}
}
\details{
  Variables retain their tracked/untracked status (with the exception
  that a tracked variable can go from being tracked to being untracked
  if the new name matches the tracking option \code{autoTrackExcludePattern}.)

  Renaming works correctly when old and new names overlap, e.g.,
  \code{track.rename(c("x","y"), c("y","x"))} will swap the values of
  \code{x} and \code{y}.
}
\value{
  A list containing the old and new names:
\item{old}{a character vector of names}
\item{new}{a character vector of names}
}
\author{
  Tony Plate \email{tplate@acm.org}
}
\examples{
invisible(if (file.exists("tmp21")) unlink("tmp21", recursive=TRUE))
track.start(dir="tmp21")
a <- 1
b <- rep(2, 2)
track.rename(c("a", "b"), c("b", "a"), clobber=TRUE)
c(a, b)
track.stop()
invisible(if (file.exists("tmp21")) unlink("tmp21", recursive=TRUE))
}
\keyword{ data }
