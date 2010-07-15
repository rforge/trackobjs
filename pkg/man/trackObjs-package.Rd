\name{trackObjs-package}
\alias{trackObjs-package}
\alias{trackObjs}
\docType{package}
\title{
Overview of trackObjs package
}
\description{

The trackObjs package sets up a link between R objects in memory and
files on disk so that objects are automatically resaved to files when
they are changed.  R objects in files are read in on demand and do not
consume memory prior to being referenced.  The trackObjs package also
tracks times when objects are created and modified, and caches some
basic characteristics of objects to allow for fast summaries of objects.

Each object is stored in a separate RData file using the standard
format as used by \code{save()}, so that objects can be manually
picked out of or added to the trackObjs database if needed.

Tracking works by replacing a tracked variable by an
\code{activeBinding}, which when accessed looks up information in an
associated 'tracking environment' and reads or writes the corresponding
RData file and/or gets or assigns the variable in the tracking
environment.  A callback for the end-of-top-level-task handles newly
created or deleted variables.

}
\details{
There are three main reasons to use the \code{trackObjs} package:
\itemize{
  \item conveniently handle many moderately-large objects that would
  collectively exhaust memory or be inconvenient to manage in
  files by manually using \code{save()}, \code{load()}, and/or \code{save.image()}.
  \item keep track of creation and modification times on objects
  \item get fast summaries of basic characteristics of objects - class,
  size, dimension, etc.
}

There is an option to control whether tracked objects are cached in
memory as well as being stored on disk.  By default, objects are
not cached.  To save time when working with collections of objects that
will all fit in memory, turn on
caching with
\code{\link{track.options}(cache=TRUE)}, or start
tracking with \code{\link{track.start}(..., cache=TRUE)}.

Here is a brief example of tracking some variables in the global environment:

\preformatted{
> library(trackObjs)
> track.start("tmp1")
> x <- 123                  # Variable 'x' is now tracked
> y <- matrix(1:6, ncol=2)  # 'y' is assigned & tracked
> z1 <- list("a", "b", "c")
> z2 <- Sys.time()
> track.summary(size=F)     # See a summary of tracked vars
            class    mode extent length            modified TA TW
x         numeric numeric    [1]      1 2007-09-07 08:50:58  0  1
y          matrix numeric  [3x2]      6 2007-09-07 08:50:58  0  1
z1           list    list  [[3]]      3 2007-09-07 08:50:58  0  1
z2 POSIXt,POSIXct numeric    [1]      1 2007-09-07 08:50:58  0  1
> # (TA="total accesses", TW="total writes")
> ls(all=TRUE)
[1] "x"  "y"  "z1" "z2"
> track.stop()              # Stop tracking
> ls(all=TRUE)
character(0)
>
> # Restart using the tracking dir -- the variables reappear
> track.start("tmp1") # Start using the tracking dir again
> ls(all=TRUE)
[1] "x"  "y"  "z1" "z2"
> track.summary(size=F)
            class    mode extent length            modified TA TW
x         numeric numeric    [1]      1 2007-09-07 08:50:58  0  1
y          matrix numeric  [3x2]      6 2007-09-07 08:50:58  0  1
z1           list    list  [[3]]      3 2007-09-07 08:50:58  0  1
z2 POSIXt,POSIXct numeric    [1]      1 2007-09-07 08:50:58  0  1
> track.stop()
>
> # the files in the tracking directory:
> list.files("tmp1", all=TRUE)
[1] "."                    ".."
[3] "filemap.txt"          ".trackingSummary.rda"
[5] "x.rda"                "y.rda"
[7] "z1.rda"               "z2.rda"
>
}

There are several points to note:
\itemize{
  \item The global environment is the default environment for tracking
  -- it is possible to track variables in other environments, but that
  environment must be supplied as an argument to the track functions.
  \item By default, newly created or deleted variables are automatically
  added to or removed from the tracking database.  This feature can be
  disabled by supplying \code{auto=FALSE} to \code{track.start()}, or by
  calling \code{track.auto(FALSE)}.
  \item When tracking is stopped, all
  tracked variables are saved on disk and will be no longer accessible
  until tracking is started again.
  \item The objects are stored each in their own file in the
  tracking dir, in the
  format used by \code{save()}/\code{load()} (RData files).
}
}

\section{List of basic functions and common calling patterns}{

For straightforward use of the trackObjs package, only a single
call to \code{track.start()} need be made to start automatically tracking the global
environment.  If it is desired to save untrackable variables at the end
of the session, \code{track.stop()} should be called before calling
\code{save.image()} or \code{q('yes')}.  The basic functions used in
automatic tracking are as follows:
\itemize{
  \item \code{\link{track.start}(dir=...)}: start tracking
  the global environment, with files saved in \code{dir} (the default is \code{rdatadir}).
  \item \code{\link{track.summary}()}: print a summary of
  the basic characteristics of tracked variables: name, class, extent,
  and creation, modification and access times.
  \item \code{\link{track.stop}()}: stop tracking
  (any unsaved tracked variables are saved to disk and all tracked variables
  become unavailable until tracking starts again)
  \item \code{\link{track.attach}(dir=..., pos=)}: attach an existing
  tracking database to the search list at the specified position.
  Changes to variables in the attached environment will be automatically
  saved to the database.
}

For the non-automatic mode, four other functions cover the majority of common usage:

\itemize{
  \item \code{\link{track.start}(dir=..., auto=TRUE/FALSE)}: start tracking
  the global environment, with files saved in \code{dir}
  \item \code{\link{track}(x)}: start tracking \code{x} -
  \code{x} in the global environment is replaced by an active binding
  and \code{x} is saved in its corresponding file in the tracking
  directory and, if caching is on, in the tracking environment
  \item \code{\link{track}(x <- value)}: start tracking \code{x}
  \item \code{\link{track}(list=c('x', 'y'))}: start tracking
  specified variables
  \item \code{\link{track}(all=TRUE)}: start tracking all
  untracked variables in the global environment
  \item \code{\link{untrack}(x)}: stop tracking variable \code{x} -
  the R object \code{x} is put back as an ordinary object in the global environment
  \item \code{\link{untrack}(all=TRUE)}: stop tracking all
  variables in the global environment (but tracking is still set up)
  \item \code{\link{untrack}(list=...)}: stop tracking specified variables
  \item \code{\link{track.remove}(x)}: completely remove all
  traces of \code{x} from the global environment, tracking environment
  and tracking directory.   Note that if variable \code{x} in the global
  environment is tracked,
  \code{remove(x)} will make \code{x} an "orphaned" variable:
  \code{remove(x)} will just remove the active binding from the global
  environment, and leave \code{x} in the tracked environment and on
  file, and \code{x} will reappear after restarting tracking.
}
}

\section{Complete list of functions and common calling patterns}{

The \code{trackObjs} package provides many additional functions for
controlling how tracking is performed (e.g., whether or not tracked variables
are cached in memory), examining the state of tracking (show which
variables are tracked, untracked, orphaned, masked, etc.) and repairing
tracking environments and databases that have become inconsistent or incomplete
(this may result from resource limitiations, e.g., being unable to
write a save file due to lack of disk space, or from manual tinkering,
e.g., dropping a new save file into a tracking directory.)

The functions that can be used to set up and take down tracking are:
\itemize{
  \item \code{\link{track.start}(dir=...)}: start tracking,
  using the supplied directory
  \item \code{\link{track.stop}()}: stop tracking
  (any unsaved tracked variables are saved to disk and all tracked variables
  become unavailable until tracking starts again)
  \item \code{\link{track.dir}()}: return the path of the
  tracking directory
}

Functions for tracking and stopping tracking variables:
\itemize{
  \item \code{\link{track}(x)}
  \code{\link{track}(var <- value)}
  \code{\link{track}(list=...)}
  \code{\link{track}(all=TRUE)}: start tracking variable(s)
  \item \code{\link{track.load}(file=...): load some objects from
    a RData file into the tracked environment}
  \item \code{\link{untrack}(x, keep.in.db=FALSE)}
  \code{\link{untrack}(list=...)}
  \code{\link{untrack}(all=TRUE)}: stop tracking variable(s) -
  value is left in place, and optionally, it is also left in the the database
}

Functions for getting status of tracking and summaries of variables:
\itemize{  \item \code{\link{track.summary}()}: return a data
  frame containing a summary of the basic characteristics of tracked
  variables: name, class, extent, and creation, modification and access times.
  \item \code{\link{track.status}()}: return a data frame
  containing information about the tracking status of variables: whether
  they are saved to disk or not, etc.
  \item \code{\link{env.is.tracked}()}: tell whether an
  environment is currently tracked
}

The remaining functions allow the user to more closely manage variable
tracking, but are less likely to be of use to new users.

Functions for getting status of tracking and summaries of variables:
\itemize{
  \item \code{\link{tracked}()}: return the names of tracked variables
  \item \code{\link{untracked}()}: return the names of
  untracked variables
  \item \code{\link{untrackable}()}:  return the names of
  variables that cannot be tracked
  \item \code{\link{track.unsaved}()}: return the names of
  variables whose copy on file is out-of-date
  \item \code{\link{track.orphaned}()}: return the
  names of once-tracked variables that have lost their active binding
  (should not happen)
  \item \code{\link{track.masked}()}: return the names of
  once-tracked variables whose active binding has been overwritten by an
  ordinary variable (should not happen)
}

Functions for managing tracking and tracked variables:
\itemize{
  \item \code{\link{track.options}()}: examine and set
  options to control tracking
  \item \code{\link{track.remove}()}: completely remove all
  traces of a tracked variable
  \item \code{\link{track.save}()}: write unsaved variables to disk
  \item \code{\link{track.flush}()}: write unsaved variables to disk, and remove from memory
  \item \code{\link{track.forget}()}: delete cached
  versions without saving to file (file version will be retrieved next
  time the variable is accessed)
  \item \code{\link{track.rescan}()}: reload variable
  values from disk (can forget all cached vars, remove no-longer existing tracked vars)
  \item \code{\link{track.load}()}: load variables from a
  saved RData file into the tracking session
}

Functions for recovering from errors:
\itemize{
  \item \code{\link{track.rebuild}()}: rebuild tracking
  information from objects in memory or on disk
  \item \code{\link{track.flush}}: write unsaved variables to disk, and remove from memory
}

Design and internals of tracking:
\itemize{
  \item \code{\link{track.design}}
}
}

\author{Tony Plate <tplate@acm.org>}
\references{
Roger D. Peng. Interacting with data using the filehash package. R
News, 6(4):19-24, October
2006. \url{http://cran.r-project.org/doc/Rnews} and
\url{http://sandybox.typepad.com/software}

David E. Brahm. Delayed data packages. R News, 2(3):11-12, December
2002.  \url{http://cran.r-project.org/doc/Rnews}
}

\seealso{
\link[=track.design]{Design} of the \code{trackObjs} package.

Potential \link[=track.future]{future features} of the \code{trackObjs} package.

Documentation for \code{\link{save}} and \code{\link{load}} (in 'base' package).

Documentation for \code{\link{makeActiveBinding}} and related
functions (in 'base' package).

Inspriation from the packages \code{\link[g.data:g.data.save]{g.data}} and
\code{\link[filehash:dbLoad]{filehash}}.

Description of the facility
(\code{\link[base:addTaskCallback]{addTaskCallback}}) for adding a
callback function that is called at the end of each top-level task (each
time R returns to the prompt after completing a command):
\url{http://developer.r-project.org/TaskHandlers.pdf}.

}
\keyword{ package }
\keyword{ data }
\keyword{ database }
\keyword{ utilities }
