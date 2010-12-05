\name{track.options}
\alias{track.options}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set and get tracking options on a tracked environment}
\description{
  Set and get tracking options on a tracked environment.  Each tracked
  environment has its own set of tracking options exists which can be
  changed indpendently.   Global default values can be set in \code{options("global.track.options")}.
}
\usage{
track.options(..., pos = 1, envir = as.environment(pos),
              values=list(...), save = FALSE, clear=FALSE, delete=FALSE,
              trackingEnv, only.preprocess = FALSE, old.options = list())
}

\arguments{
  \item{\dots}{ Either option names as character data, or specifications
    for setting options, as named arguments, or in
    a named list }
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
  of specifying the environment being tracked, but should be rarely
  needed.}
  \item{values}{A named list of option values to set.
    \code{track.options(readonly=T)} is equivalent to
    \code{track.options(values=list(readonly=TRUE))}}
  \item{save}{If \code{TRUE}, current options are saved to disk and will
    be used in future.  Note that all current options settings are saved,
    not just the new settings made in this call.}
  \item{clear}{If \code{TRUE}, and the option can have multiple values
  (e.g., \code{autoTrackExcludeClass}),
  the current values are cleared prior to using the supplied values.  The
  default behavior, with \code{clear=FALSE} and \code{delete=FALSE} is
  to add supplied values to multi-valued options, and to replace the
  value for single-valued options. }
  \item{delete}{If \code{TRUE}, and the option can have multiple values,
  the supplied values are removed from the current values (if they are not in
  the current values, they are silently ignored.)}
  \item{trackingEnv}{ The hidden environment in which tracked objects
    are stored.  It is not necessary to supply this in normal use. }
  \item{only.preprocess}{ If \code{TRUE}, process any options
    specifications and return the full list of option settings with
    the values as specified, and defaults for all othe roptions.  Stored
    options are neither accessed nor changed.  Intended for internal use.}
  \item{old.options}{A list of old options to use, can only be suppled
    when \code{only.preprocess=TRUE}.  Intended for internal use.}
}
\details{
  Valid option names and values are as follows:
  \describe{
    \item{readonly:}{logical (default TRUE for \code{track.attach()} and FALSE
  for \code{track.start()}) should any changes be allowed to the files?
  Note that this option is a function of how a tracking database is
  accessed -- it is not a property of the database itself.  To
  unconditionally protect a tracking database from modification, use file permissions.}
    \item{summaryTimes:}{logical, or integer value 0,1,2,3 (see
  \code{\link{track.summary}()} for the effect of these settings}
    \item{summaryAccess:}{logical, or integer value 0,1,2,3,4}
    \item{cache:}{logical (default TRUE): keep written objects in memory?}
    \item{writeToDisk:}{logical (default TRUE): always write changed
      objects to disk?  If \code{TRUE}, when objects are written to disk depends on \code{cachePolicy}:
      \code{cachePolicy="none"}: write objects immediately on a change;
      \code{cachePolicy="eotPurge"}: write changed objects at the end of a top-level task
    }
    \item{recordAccesses:}{logical (default TRUE) if TRUE, record
      counts and times for access ("get") operations on tracked variables}
    \item{maintainSummary:}{logical (default TRUE) if TRUE, record time & number of accesses}
    \item{autoTrackExcludePattern:}{character vector (default \code{c("^\\.track", "^\\.required")}) variables
  whose name matches any of these regular expressions are not auto-tracked}
    \item{autoTrackExcludeClass:}{character vector (default \code{"RODBC"}) variables
  whose classes is in this vector are not auto-tracked}
    \item{alwaysSaveSummary:}{logical (default TRUE) if TRUE, always save
    the summary on any change}
    \item{RDataSuffix:}{character (default \code{"rda"}) suffix to use for files
      containing saved R objects}
    \item{compress:}{character or logical (default \code{TRUE})
  passed to \code{save()}.  Possible values are \code{"none"},
  \code{"gzip"}, \code{"xz"}, \code{"bzip2"}.  \code{save()} currently
  uses gzip by default (i.e., when \code{compress=TRUE}), which
  offers the best tradeoff of filesize and compression and decompression
  times.}
    \item{compression_level:}{numeric (default \code{1})
      passed to \code{save()}}
    \item{cacheKeepFun:}{A function that specifies which objects to keep
  in memory. See "cache plugin functions" below.}
}

  The option settings are saved as a list in an object called
  \code{.trackingOptions} in the tracking environment (with a copy
  mirrored to a file in the tracking dir if \code{save=TRUE}.)

  The options can be used to tune performance to resource availability (time &
  memory) and robustness in the face of machine or user error.  Some
  possible settings are:

  \describe{
    \item{maximize robustness and speed:}{ \code{cache=TRUE} and
      \code{writeToDisk=TRUE} (the default): always write an object to disk when it is
      changed, and keep a copy in memory, so that an object only needs
      to be read once}
    \item{minimize memory usage and maximize robustness:}{
      \code{writeToDisk=TRUE}, \code{cache=FALSE}: always write an
      object to disk when it is changed, and don't keep a copy in memory
      -- need to read from disk whenever the object is referred to}
    \item{maximize speed:}{ \code{writeToDisk=FALSE},
      \code{cache=TRUE}: don't write the object to disk - just keep
      a copy in memory after it is first accessed and only write it when
      \code{track.stop()} or
      one of \code{track.save()} or its friends is called.  This
      combination less robust because changed variables can be lost if R crashes,
      or the user quits R without remembering to call
      \code{track.stop()}.  This mode of operation is like the
      \code{g.data} package, but with automatically keeping track of
      which variables have been changed and need to be written to disk
      (and the writing of changed variables with one call to
      \code{track.save()} or \code{track.stop()}).}
  }
  The combination \code{writeToDisk=FALSE} and
  \code{cache=FALSE} is possible, but is unlikely to be desirable
  -- this will keep changed objects in memory, but will not keep
  merely fetched objects in memory.

  The options \code{maintainSummary}, \code{recordAccesses}, and
  \code{alwaysSaveSummary} control when the object summary is updated
  and when it is saved to disk (the default is for it to be updated and
  saved to disk for every read and write access to an object, whether or
  not the object is cached in memory).

  Global default values can be set in
  \code{options("global.track.options")} as a list like
  \code{options(global.track.options=list(cache=TRUE, cachePolicy='tltPurge'))}.
}
\value{
  The value returned is a list of option values.  If options were
  specified as arguments, the old values of those options are returned
  (unless \code{only.preprocess=TRUE} was supplied).  If no options were
  specified as arguments, the full list of current option values is returned.
}
\section{Cache plugin functions}{

\code{track} contains an experimental feature that allows
users to supply their own plugin functions that specify
cache rules.  Currently, the plugin function can specify
whether or not an object will be flushed from memory at the
end of a top-level command.

Here's an example of a cache plugin function that keeps in
memory variables whose names begin with the letter 'x'.

\preformatted{
plugin <- function(objs, inmem, envname) {
    keep <- regexpr("^x", rownames(objs))>0
    # browser() # uncomment for debugging & development
    return(keep)
}
}

To use this function, supply it to \code{track.options()}:
\preformatted{
  track.options(cacheKeepFun=plugin, save=TRUE)
  }

The plugin function takes three arguments:
\itemize{

  \item{objs}: the object summary dataframe - same as returned by
  \code{track.summary()}.  The names of the objects are in the rownames
  of the dataframe.

  \item{inmem}: a logical vector with length equal to the number of rows
  in \code{objs}.  It will have value \code{TRUE} where the
  corresponding object is in memory, and \code{FALSE} otherwise.

  \item{envname}: a single string containing the name of the tracking
  environment, in a form like \code{<env R_GlobalEnv>}.

}

The plugin function should return a logical vector the same length as
\code{inmem}, with \code{TRUE} values where the corresponding objects
should be kept in memory.

}

\author{Tony Plate <tplate@acm.org>}
\seealso{ \link[=track-package]{Overview} and \link[=track.design]{design} of the \code{track} package. }
\examples{
library(track)
unlink("tmp1", recursive=TRUE)
track.start("tmp1")
x <- 33
X <- array(1:24, dim=2:4)
track.status()
track.options(cache=TRUE, writeToDisk=FALSE) # change for just this session
# different ways of retrieving option values
track.options(c("cache", "writeToDisk"))
track.options("cache", "writeToDisk")
track.options("cache")
track.options()
# see the effect of the changed options on the status of X (X is not saved to disk)
track.status()
X[1,1,1] <- 0
track.status()
track.flush()
track.status()
track.stop(pos=1)
track.start("tmp1")
# note that options previously changed are back at defaults (because default
# to track.options() is save=FALSE
track.options(c("cache", "writeToDisk"))
track.options(cache=TRUE, writeToDisk=FALSE, save=TRUE) # change the options on disk
track.options(c("cache", "writeToDisk"))
track.stop(pos=1)
track.start("tmp1")
# now options previously changed are remembered (because track.options(..., save=TRUE) was used)
track.options(c("cache", "writeToDisk"))
track.stop(pos=1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
