\name{track.summary}
\alias{track.summary}
\title{ Return a summary of the basic properties of tracked objects }
\description{
  Return a summary of the basic properties of tracked objects: name,
  class, size, dimensions (if any), and creation, modification and
  access times.
}
\usage{
track.summary(expr, pos = 1, envir = as.environment(pos), list = NULL, pattern = NULL,
        glob = NULL, all.names = FALSE,
        times = track.options("summaryTimes", envir=envir)[[1]],
        access = track.options("summaryAccess", envir=envir)[[1]],
        size=TRUE, cache=FALSE, full=FALSE)
}

\arguments{
  \item{expr}{: An unquoted variable name }
  \item{pos}{: The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{: The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
  of specifying the environment being tracked, but should be rarely needed.}
  \item{list}{: A character vector of variable names to summarize }
  \item{pattern}{: A regular expression specifying variable names to summarize }
  \item{glob}{: A regular expression specifying variable names to summarize }
  \item{all.names}{: should names beginning with a period be included
  (like \code{all.names} in \code{ls})}
  \item{times}{: An integer 0..3 specifying how many time columns to
    return (in order of: modify, create, access) }
  \item{access}{: An integer 0..3 specifying how many access-count
    columns to return for each writes and accesses (0=none, 1=total,
    2=prior and current session, 3=prior, current session and total)}
  \item{size}{: (logical) include the \code{size} column?  (The values in
    this column are system dependent, so make it easy to exclude so that
    test output is constant across systems.)}
  \item{cache}{: (logical) include the \code{cache} column?}
  \item{full}{: (logical) include all the optional columns?}
}
\details{
  Returns part or all of the cached summary data.  There is one row per
  object.  Only tracked objects appear in the summary.
}
\value{
  The value returned is a dataframe that summarizes the specified
  objects.  This function does not create any output itself -- the
  auto-printing of the returned value is the visible output.
  The data frame has one row for each
  object (rownames are the object names) and some of the following columns:

    \item{class:}{(character) from \code{class()} (if \code{class(obj)}
      has more than one component, all components are appended separated
      by commas, if \code{class(obj)} returns a zero-length result, the
    value is \code{"?"})}
    \item{mode:}{(character) from \code{mode()}}
    \item{extent:}{(character) from \code{dim()} or \code{length()},
    with double brackets if the object is a list (will contain
    \code{"(error)"} if \code{dim(obj)} causes an error)}
    \item{length:}{(integer) from \code{length()} (will be
    \code{NA} if \code{length(obj)} causes an error)}
    \item{size:}{(integer) from \code{object.size()}}
    \item{cache:}{(character): Indicates whether the objected should
      always be kept cached.  Possible values are
      \describe{
	\item{yes, no:}{Automatically determined from
      \code{track.options("alwaysCache")} (object names)
      or \code{track.options("alwaysCacheClass")} (object classes); re-evaluated when
      object changes}
	\item{fixedyes, fixedno:}{Always do this; don't re-evaluate when
      object changes.  This is intended to allow manually setting this
      for each object if desired, but as of version 1.0.9 no way for the
      user to do this safely has been provided.}
      }}
    \item{modified:}{most recent modification time}
    \item{created:}{time object created}
    \item{read:}{most recent modification time}
    \item{A:}{(logical) Accuracy of times: \code{TRUE} if object summary
    has been maintained since object was first tracked; \code{FALSE} if
    the object summary was lost at some point and then recreated}
    \item{ES:}{(integer) sessions alive}
    \item{SR:}{(integer) num reads this session}
    \item{SW:}{(integer) num writes this session}
    \item{PR:}{(integer) total reads prior to this session}
    \item{PW:}{(integer) total writes prior to this session}
    \item{TR:}{(integer) total reads}
    \item{TW:}{(integer) total writes}

  Which columns are present depends on the arguments \code{times},
  \code{access}, and \code{size}.

  The reason for the \code{class} column containing all classes of the
  object separated by commas is that extracting the most informative
  class label is not simple, for example, the class of an object
  returned by \code{glm()} is \code{c("glm", "lm")} (most informative
  first), while the class of an object returned by \code{Sys.time()} is
  \code{c("POSIXt", "POSIXct")} (most informative last).
}
\note{
  The object summary data is maintained in an object called
  \code{.trackingSummary} kept in the tracking environment.  It is not
  visible on the search path.
}
\author{Tony Plate <tplate@acm.org>}
\seealso{ \link[=track-package]{Overview} and \link[=track.design]{design} of the \code{track} package. }
\examples{
##############################################################
# Warning: running this example will cause variables currently
# in the R global environment to be written to .RData files
# in a tracking database on the filesystem under R's temporary
# directory, and will cause the variables to be removed temporarily
# from the R global environment.
# It is recommended to run this example with a fresh R session
# with no important variables in the global environment.
##############################################################

library(track)
track.start(dir=file.path(tempdir(), 'rdatadir11'))
x <- 33
X <- array(1:24, dim=2:4)
Y <- list(a=1:3,b=2)
X[2] <- -1
y1 <- 2
y2 <- 3
track.summary()
track.summary(time=0, access=1, size=FALSE)
track.summary(X)
track.summary(list=c("x", "X"))
track.summary(pattern="[xX]")
# Would normally not call track.stop(), but do so here to clean up after
# running this example.
track.stop(pos=1, keepVars=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
