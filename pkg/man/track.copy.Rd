\name{track.copy}
\alias{track.copy}
\alias{track.move}
\title{
  Copy or move objects from one tracking db to another
}
\description{
  Copy or move objects from one tracking db to another.
  \code{track.move} calls \code{track.copy} with a default of
  \code{delete=TRUE} to remove the source object.
}
\usage{
track.copy(from, to = 1, list = NULL, pattern = NULL, glob = NULL, delete = FALSE, clobber = FALSE, verbose = TRUE)
track.move(from, to = 1, list = NULL, pattern = NULL, glob = NULL, delete = TRUE, clobber = FALSE, verbose = TRUE)
}
\arguments{
  \item{from}{
    The position on the search list of the source db.  Can be numeric or
    the name as returned by \code{search()}.
}
  \item{to}{
    The position on the search list of the destination db.  Can be numeric or
    the name as returned by \code{search()}.
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
  Should the objects in the source db be removed? The default is
  \code{FALSE} for \code{track.copy} and \code{TRUE} for \code{track.move}.
}
  \item{clobber}{
  Should the objects in the destination db be overwritten?
}
\item{verbose}{
  If \code{TRUE}, write out what is being done.
}
}
\details{
  \code{track.copy} copies objects by copying their underlying files from
  the source tracking db to the destination tracking db.  It also copies
  the object summary (preserving modification times etc.)  Objects are
  not loaded in R when copied, and if a copied object is cached in the
  source or destination db, the cached copy will be removed.

  The readonly status of both tracking dbs will be respected -- the
  function will stop with an error before doing anything if a readonly
  flag conflicts with what it needs to do.

  The metadata in source and destination tracking dbs (i.e., the file
  map and the object summary) are update after each object is copied.
  Thus, if an error occurs during copying, e.g., due to filesystem
  permissions or lack of space, the source and destination dbs will be
  left in a consistent state.
}
\value{
  A character vector of the objects copied or moved.
}
\author{
  Tony Plate <tplate@acm.org>
}
\examples{
track.start(dir="tmp1")
track.attach(dir="tmp2", pos=2, create=TRUE)
assign("x1", 1, pos=2)
assign("x2", 2, pos=2)
assign("y3", 3, pos=2)
assign("y4", 4, pos=2)
assign("z5", 5)
track.status(1)
track.status(2)
track.copy(from=2, pat="^x")
ls(1)
ls(2)
track.move(from=2, pat="^y")
track.move(from=1, to=2, pat="^z")
ls(1)
ls(2)
c(x1, x2, y3, y4)
track.move(from=2, pat="^x", clobber=TRUE)
ls(1)
ls(2)
c(x1, x2, y3, y4)
track.status(1)
track.status(2)
track.detach(2)
track.stop()
}
\keyword{ data }
