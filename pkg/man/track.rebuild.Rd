\name{track.rebuild}
\alias{track.rebuild}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Rebuild database information for tracked objects }
\description{
  Rebuild database information (the file map, and/or the object summary)
  for objects in an active tracking environment, or for saved objects
  in a tracking directory.
}
\usage{
track.rebuild(pos = 1, envir = as.environment(pos), dir = NULL,
        fix = FALSE, level=c("missing", "all"), trust=c("unspecified", "environment", "db"),
        verbose=1, RDataSuffix=NULL, dry.run=TRUE, replace.wd=TRUE,
        use.file.times=TRUE)
}

\arguments{
  \item{pos}{ The position of the environment to which the tracking database to rebuild is linked (for active tracking databases. }
  \item{envir}{ The tracked environment for which to rebuild the tracking database (for active tracking databases. }
  \item{dir}{ The directory where the tracking database is stored (for tracking databases not currently active.}
  \item{fix}{ If \code{TRUE}, try to fix various problems such as
    illegal file names by renaming files or moving unusable
    RData files to a 'quarantine' directory.  If \code{fix=TRUE} and
    \code{dry.run=TRUE}, changes will be reported but not made.  }
  \item{level}{If \code{"missing"}, rebuild only missing information, if
    \code{"all"}, rebuild information for all objects.}
  \item{trust}{When rebuilding an active tracking database, and there
    are conflicts between the data in the environment versus that in
    files, which should be trusted?}
  \item{verbose}{Controls number of messages about progress}
  \item{RDataSuffix}{What suffix should be used for RData files?
    (Usually worked out automatically.)}
  \item{dry.run}{If \code{TRUE}, no data objects in R or files on disk
    are changed: the return value is a list containing the rebuilt file map and
    the rebuilt object summary.  If \code{FALSE}, objects in R
    environments and files on disk can be changed.  The default is
    \code{TRUE} to guard against changing things when it would have been
    better not to have changed things, but this default may change in future.}
  \item{replace.wd}{If \code{TRUE}, file paths in diagnostic output are
    printed starting with "./" where possible -- this makes comparisons
    of test output more portable.}
  \item{use.file.times}{If \code{TRUE}, file creation, modification and
    access times are used for objects that were not found in
    existing summary objects.  If \code{FALSE}, the current time is
    used, which can be useful for testing purposes.}
}

\details{
  The file map and/or the object summary are rebuilt.  If
  \code{level=="all"}, all RData files will be read, which could take a
  long time if there are many files.  If \code{level=="missing"}, RData
  files will be read only where there is missing information.

  If there are incompatible RData files in the directory (e.g., illegal
  or duplicated object names, or multiple objects), \code{track.rebuild} will stop with an
  error unless \code{fix==TRUE}, in which case the incompatible
  RData files will either be renamed or moved to a \code{quarantine}
  subdirectory.  In the case of duplicated object names, the second
  object encountered will be moved.

}

\value{
  The value returned is a list with between two and four components:
  \item{fileMap}{The mapping from object names to files (excluding the suffix)}
  \item{summary}{The dataframe that summarizes objects}
  \item{unsaved}{(optional) The names of variables that are not saved to
    disk.  This component is only present if it is non-empty.}
  \item{masked}{(optinal) The names of tracked variables that are masked
    by other variables in the tracked environment.
    This component is only present if it is non-empty.}
  The returned value is visible if \code{dry.run=TRUE} and invisible otherwise.
}

\author{Tony Plate <tplate@acm.org>}
\seealso{ \link[=trackObjs-package]{Overview} and \link[=track.design]{design} of the \code{trackObjs} package. }
\examples{
# Rebuild a damaged tracking database
library(trackObjs)
unlink("tmp1", recursive=TRUE)
# build a tracking dir populated with some variables
track.start("tmp1")
track(x <- 33)
track(X <- array(1:24, dim=2:4))
track(Y <- list(a=1:3,b=2))
X[2] <- -1
track(abc <- "def")
track(def <- list(1,2,3))
invisible(Y); invisible(abc); invisible(abc); invisible(abc)
track.summary()
track.stop()
# damage the database (remove the filemap)
unlink("tmp1/filemap.txt")
# and rebuild
track.rebuild(dir="tmp1", verbose=2, dry.run=FALSE, fix=TRUE)
track.start("tmp1")
track.summary()
track.status()
track.stop()
unlink("tmp1", recursive=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
