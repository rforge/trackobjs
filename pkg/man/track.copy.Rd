\name{track.copy}
\alias{track.copy}
\alias{track.move}
\title{
  Copy or move objects from one tracking db to another
}
\description{
  Copy or move objects from one tracking db to another
}
\usage{
track.copy(from, to = 1, list = NULL, pattern = NULL, glob = NULL, delete = FALSE, clobber = FALSE, verbose = TRUE)
track.move(from, to = 1, list = NULL, pattern = NULL, glob = NULL, delete = TRUE, clobber = FALSE, verbose = TRUE)
}
\arguments{
  \item{from}{
    The position on the search list of the source db
}
  \item{to}{
    The position on the search list of the destination db
}
\item{list}{
  A character vector of the objects to copy or move
}
  \item{pattern}{
  A regular expression specifying the objects to copy or move
}
  \item{glob}{
  A glob specifying the objects to copy or move
}
\item{delete}{
  Should the objects in the source db be removed?
}
  \item{clobber}{
  Should the objects in the destination db be overwritten?
}
\item{verbose}{
  Write out what is being done?
}
}
\details{
}
\value{
  A character vector of the objects copied or moved.
}
\author{
  Tony Plate <tplate@acm.org>
}
\seealso{
}
\examples{
}
\keyword{ data }
