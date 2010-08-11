\name{track.status}
\alias{track.status}
\alias{track.dir}
\alias{tracked}
\alias{untracked}
\alias{untrackable}
\alias{track.unsaved}
\alias{track.orphaned}
\alias{track.masked}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Return information about the status of tracking }
\description{
  Return information about the status of tracking in a particular
  environment.  Functions tell which variables are and which are not
  tracked, and whether objects exist in memory or in files.
}
\usage{
track.status(pos = 1, envir = as.environment(pos), expr,
        qexpr = NULL, list = NULL, pattern = NULL, glob = NULL,
        file.status = TRUE, tracked = NA, reserved=FALSE,
        what = c("all", "tracked", "trackable", "untracked",
                 "orphaned", "masked", "unsaved", "untrackable"))
tracked(        pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL)
untracked(      pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL)
track.orphaned( pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL)
track.masked(   pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL)
untrackable(    pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL)
track.unsaved(  pos=1, envir=as.environment(pos), list=NULL, pattern=NULL, glob=NULL)
}

\arguments{
  \item{pos}{ The search path position of the environment being tracked
    (default is 1 for the global environment)}
  \item{envir}{ The environment being tracked.  This is an alternate way
    (to the use of \code{pos=})
  of specifying the environment being tracked, but should be rarely needed.}
  \item{expr}{ An unquoted variable name }
  \item{qexpr}{ A variable name as an expression -- not intended for use
    by end-users }
  \item{list}{ A character vector of variable names }
  \item{pattern}{ A regular expression specifying variable names to operate upon }
  \item{glob}{ A glob pattern specifying variable names to operate upon }
  \item{file.status}{ Check whether or not the file associated with a
    tracked object exists }
  \item{tracked}{ If \code{TRUE}, return information only on tracked
    objects, if \code{FALSE}, return information only on objects that
    are not tracked, and if \code{NA} return information on all
    variables (subject to other filtering).}
  \item{what}{ controls the information returned: \code{"all"} means
    return a data frame of status, other values means return a list of
    names of objects having that status }
  \item{reserved}{ If \code{TRUE}, include info about non-tracked variables with
    reserved names.  The default is to always omit these variables from
    the status summary.}
}
\details{
  These functions return information about the status of tracking on
  some or all variables in \code{envir} and the tracking environment.
  Tracking status depends on the relationship among four entities used
  for a tracked object:
  \itemize{
  \item the name of the object
  \item the binding in \code{envir} which should
    be an active binding that refers to the tracking environment
  \item the cached object in the tracking environment (i.e., stored in
  memory in R)
  \item the corresponding disk file in the tracking directory
  }
  Statuses are defined as follows:
  \tabular{rllll}{
    Status       \tab object name \tab variable \tab cached object \tab file \cr
    tracked      \tab ordinary \tab active binding \tab maybe \tab yes, maybe up-to-date \cr
    untrackable  \tab reserved name \tab ordinary \tab no \tab no \cr
    untracked    \tab ordinary \tab ordinary \tab no \tab no \cr
    masked       \tab ordinary \tab ordinary \tab yes \tab maybe \cr
    orphaned     \tab ordinary \tab none \tab yes \tab maybe \cr
    unsaved      \tab ordinary \tab active binding \tab yes \tab not up-to-date \cr
  }
  The arguments \code{expr}, \code{list}, \code{pattern}, and
  \code{glob} all serve to restrict the set of variables considered.
}
\value{
  \item{track.status:}{returns a \code{data.frame} if
    \code{what=="all"}, or a character vector otherwise.}
  \item{track.dir:}{returns a single character string that is the
    full path to the directory where copies of objects are stored (the
    "tracking directory").}
  \item{tracked, untracked, track.orphaned,
    track.masked, untrackable, track.unsaved:}{all
    return a character vector naming the variables with a particular
    status.}
}
\author{ Tony Plate <tplate@acm.org> }
\note{These functions check whether the binding in \code{envir} is an
  active binding, but they cannot check whether the active binding has
  the correct function associated with it because R provides no
  mechanism for R-level access to the function associated with active
  bindings.
}
\seealso{ \link[=trackObjs-package]{Overview} and \link[=track.design]{design} of the \code{trackObjs} package. }
\examples{
library(trackObjs)
unlink("tmp1", recursive=TRUE)
remove(list=ls(all=TRUE))
track.start("tmp1", cache=TRUE)
# with the standard options, create a variable with each status:
# "tracked", "trackable", "untracked", "orphaned", "masked", "unsaved"
# create some objects that are untrackable for various reasons
makeActiveBinding("uncoop", function(v) {cat("I'm a binding that prints this message when accessed!\n"); "used to be a an active binding"}, globalenv())
.trackingFileMap <- "aren't I a mischevious little object :-)"
# .trackingEnv <- "Assigning me like this will break tracking!"
uncoop
.trackingFileMap
x1 <- 123
x2 <- 456
x3 <- 789
x4 <- -321
x5 <- -654
x6 <- -987
track.status()
remove(list=c("x4", "x5"))
x5 <- "not tracked"
track.flush(x2)
track.status(reserved=TRUE)
track.options(writeToDisk=FALSE)
x3 <- 0
track.status()
track.save()
track.status()
x3 <- 1
track.status()
track.unsaved()
tracked()
untracked()
untrackable()
track.masked()
track.orphaned()
track.save(x3)
track.status()
# Test forgetting an unsaved variable
x3
x3 <- 2
track.unsaved()
x3
track.forget(x3)
x3
# Test untracking a variable
track.options(cache=FALSE)
x2
track.status()
bindingIsActive("x2", globalenv())
untrack(x2)
track.status()
bindingIsActive("x2", globalenv())
bindingIsActive("x3", globalenv())
untrack(x3, keep.in.db=TRUE)
track.status()
bindingIsActive("x3", globalenv())
# Try saving some objects to a file then loading them into the tracked environment
y1 <- 1
y2 <- "abc"
y3 <- list(2, "def")
save(list=c("y1", "y2", "y3"), file="tmpy.rda")
remove(list=c("y1", "y2", "y3"))
track.options(list(writeToDisk=TRUE, cache=TRUE))
track.status()
track.load("tmpy.rda")
unlink("tmpy.rda")
track.status()
y1
y2
y3
# See if the protection mechanisms against overwriting vars in
# track.load work correctly.
y4 <- 4
y5 <- 5
save(list=c("y3", "y4", "y5"), file="tmpy.rda")
remove(list=c("y4"))
y3 <- "new value"
y3
track.load("tmpy.rda")
y3
track.status()
track.load("tmpy.rda", clobber=TRUE)
y3
y4
y5
track.stop(pos=1)
unlink("tmp1", recursive=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
