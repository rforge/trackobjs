\name{track.plugin.lru}
\alias{track.plugin.lru}
\alias{track.plugin}
\alias{track.plugins}
\alias{track.cache}

\title{
  Plugins for cache policies in the track package.
}
\description{
  Plugins for cache policies in the track package specify what objects should be
  keep in memory at the end of each top level command.
}
\usage{
track.plugin.lru(objs, inmem, envname)
}
\arguments{
  \item{objs}{: the full object summary dataframe.  A subset of this data
  frame is returned by \code{track.summary()}; invoke \code{track.summary(times=3, access=3,
  size=T, all=T)} to get the full data frame.  The names of the objects are in the rownames
  of the dataframe.}
  \item{inmem}{: a logical vector with length equal to the number of rows
  in \code{objs}.  It will have value \code{TRUE} where the
  corresponding object is in memory, and \code{FALSE} otherwise.}
  \item{envname}{: a single string containing the name of the tracking
  environment, in a form like \code{<env R_GlobalEnv>}.}
}
\details{

\code{track} contains an experimental feature that allows
users to supply their own plugin functions that specify
cache rules.  Currently, the plugin function can specify
whether or not an object will be flushed from memory at the
end of a top-level command.

\code{track.plugin.lru()} implements a simple least-recently-used discard policy.
To use is, supply it to \code{track.options()}:
\preformatted{
  track.options(cacheKeepFun=track.plugin.lru, save=TRUE)
}

Here is another example of a very simple cache plugin function: this one
keeps in memory variables whose names begin with the letter 'x'.

\preformatted{
plugin <- function(objs, inmem, envname) {
    keep <- regexpr("^x", rownames(objs))>0
    # browser() # uncomment for debugging & development
    return(keep)
}
}

To use this plugin function, supply it to \code{track.options()}:
\preformatted{
  track.options(cacheKeepFun=plugin, save=TRUE)
}

}
\value{
A plugin function must return a logical vector the same length as
\code{inmem}, with \code{TRUE} values where the corresponding objects
should be kept in memory.
}
\note{To flush cached tracked objects from memory, use \code{\link{track.flush}()}.}
\author{
Tony Plate \email{tplate@acm.org}
}
\keyword{ data }
