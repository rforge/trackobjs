\name{track.summary}
\alias{track.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Return a summary of the basic properties of tracked objects }
\description{
  Return a summary of the basic properties of tracked objects: name,
  class, size, dimensions (if any), and creation, modification and
  access times.
}
\usage{
track.summary(expr, pos = 1, envir = as.environment(pos), list = NULL, pattern = NULL,
        glob = NULL, times = track.options("summaryTimes", envir=envir)[[1]],
        access = track.options("summaryAccess", envir=envir)[[1]], size=TRUE)
}

\arguments{
  \item{expr}{ An unquoted variable name }
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
  of specifying the environment being tracked, but should be rarely needed.}
  \item{list}{ A character vector of variable names to summarize }
  \item{pattern}{ A regular expression specifying variable names to summarize }
  \item{glob}{ A regular expression specifying variable names to summarize }
  \item{times}{ An integer 0..3 specifying how many time columns to
    return (in order of: modify, create, access) }
  \item{access}{  An integer 0..3 specifying how many access-count
    columns to return for each writes and accesses (0=none, 1=total,
    2=prior and current session, 3=prior, current session and total)}
  \item{size}{A logical: include the \code{size} column?  (This is
    system dependent, so make it easy to exclude)}
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
\seealso{ \link[=trackObjs-package]{Overview} and \link[=track.design]{design} of the \code{trackObjs} package. }
\examples{
library(trackObjs)
unlink("tmp1", recursive=TRUE)
track.start("tmp1")
track(x <- 33)
X <- array(1:24, dim=2:4)
track(X)
track(Y <- list(a=1:3,b=2))
X[2] <- -1
y1 <- 2
y2 <- 3
track.summary()
track.summary(time=0, access=1, size=FALSE)
track.summary(X)
track.summary(list=c("x", "X"))
track.summary(pattern="[xX]")
track.stop()
unlink("tmp1", recursive=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
