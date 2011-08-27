\name{track.copy}
\alias{track.copy}
\alias{track.move}
\title{
  Copy or move objects from one tracking db to another
}
\description{
  Copy or move objects from one tracking db to another.
}
\usage{
track.copy(from, to = 1, list = NULL, pattern = NULL, glob = NULL, delete = FALSE,
           clobber = FALSE, skipExisting = FALSE, verbose = TRUE, do.untrackable = FALSE)
track.move(from, to = 1, list = NULL, pattern = NULL, glob = NULL, delete = TRUE,
           clobber = FALSE, skipExisting = FALSE, verbose = TRUE, do.untrackable = FALSE)
}
\arguments{
  \item{from}{
    The position on the search list of the source db.  Can be numeric or
    the name as returned by \code{search()}.  Can be a vector, in which
    case the action is repeated for each element of \code{from}.
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
\item{skipExisting}{
  If \code{TRUE}, skip objects where an object of the same name already exists in the destination db .
}
\item{verbose}{
  If \code{TRUE}, write out what is being done.
}
\item{do.untrackable}{
  If \code{TRUE}, also copy untrackable objects.  Not yet implemented.
}
}
\details{

  \code{track.copy} copies objects by copying their underlying files
  from the source tracking db to the destination tracking db (behavior
  for untracked or untrackable objects is different -- see below).  It
  also copies the object summary (preserving modification times etc.)
  Objects are not loaded in R when copied, and if a copied object is
  cached in the source or destination db, the cached copy will be
  removed.

  The readonly status of both tracking dbs will be respected -- the
  function will stop with an error before doing anything if a readonly
  flag conflicts with what it needs to do.

  The metadata in source and destination tracking dbs (i.e., the file
  map and the object summary) are updated after each object is copied.
  Thus, if an error occurs during copying, e.g., due to filesystem
  permissions or lack of space, the source and destination dbs will be
  left in a consistent state.

  \code{track.move} calls \code{track.copy} with a default of
  \code{delete=TRUE} to remove the source object.

  \strong{NB: \code{do.untrackable} is not yet implemented:
  untracked/untrackable objects are ignored.}
  By default, untrackable objects in the source tracking db will not be
  copied or moved, and a warning will be given.  The rules for
  untrackability are those of the destination db, as specified by the
  tracking options \code{autoTrackExcludePattern} and
  \code{autoTrackExcludeClass} in that db.  If
  \code{do.untrackable=TRUE}, untrackable objects will be copied or
  moved (with \code{delete} still controlling whether the original is
  left or removed.)  Untrackable objects that are copied are left as an
  ordinary untracked object in the destination db.

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
