\name{trackObjs-package}
\alias{trackObjs-package}
\alias{trackObjs}
\docType{package}
\title{
Overview of trackObjs package
}
\description{

The trackObjs package sets up a link between R objects in memory and
files on disk so that objects are automatically saved to files when
they are changed.  R objects in files are read in on demand and do not
consume memory prior to being referenced.  The trackObjs package also
tracks times when objects are created and modified, and caches some
basic characteristics of objects to allow for fast summaries of objects.

Each object is stored in a separate RData file using the standard
format as used by \code{save()}, so that objects can be manually
picked out of or added to the trackObjs database if needed.  The
trackObjs database is a directory usually named \code{rdatadir} that
contains a RData file for each object and several housekeeping files
that are either plain text or RData files.

Tracking works by replacing a tracked variable by an
\code{activeBinding}, which when accessed looks up information in an
associated 'tracking environment' and reads or writes the corresponding
RData file and/or gets or assigns the variable in the tracking
environment.  In the default mode of operation, R variables that are
accessed are stored in memory for the duration of the top level task
(i.e., in one expression evaluated from the prompt.) A callback that is
called each time a top-level-task completes does three major things:
\itemize{
  \item detects newly created or deleted variables, and adds or removes
  from the tracking database as appropriate, and
  \item writes changed variables to the database, and
  \item deletes cached objects from memory.
  }

Tracking is not particularly suitable for storing objects that contain
environments, because those environments and their contents will be
fully written out in the saved file (in a live R session, environments
are references, and there can be multiple references to one
environment.)  Functions are one of the most common objects that contain
environments, which can contain data objects local to the function
(e.g., see the examples in the R FAQ in the section "Lexical scoping"
under "What are the differences between R and S?"
\url{http://cran.r-project.org/doc/FAQ/R-FAQ.html#Lexical-scoping}).
Additionally, the results of some modeling functions contain
environments, e.g., \code{lm} holds several references to the
environment that contains the data.  When an \code{lm} object is
\code{save}'ed, the environment containing the data, and all the other
objects in that environment, can be saved in the same file.  To work
with large data objects and modeling functions, consider first creating
a tracking database that contains the data objects.  Then, in a
different R session (which can be running at the same time), use
\code{track.attach} to attach the db of data objects at \code{pos=2} on
the search list.  When working in this way, the data objects will only
be kept in memory when being used, and modeling functions that record
environments in their results can be successful used (though beware of
modeling functions that store large amounts of data in their results.)
Alternatively, use modeling functions that do not store references to
environments.  The utility function \code{\link{show.envs}} from the
\code{trackObjs} package will show what environments are referenced
within an object (though it is not guaranteed to find them all.)

The trackObjs package also provides a self-contained incremental history
saving function that writes the most recent command to the file
\code{.Rincr_history} at the end of each top-level task, along with a
time stamp that does not appear in the interactive history. The standard
history functionality (savehistory/loadhistory) in R writes the history
only at the end of the session.  Thus, if the R session terminates
abnormally, history is lost.

}
\details{
There are four main reasons to use the \code{trackObjs} package:
\itemize{
  \item conveniently handle many moderately-large objects that would
  collectively exhaust memory or be inconvenient to manage in
  files by manually using \code{save()}, \code{load()}, and/or
  \code{save.image()}.
  \item have changed or newly created objects saved automatically at the
  end of each top-level command, which ensures objects are preserved
  in the event of accidental or abnormal termination of the R session,
  and which also makes startup and saving much faster when many large
  objects in the global environment must be loaded or saved.
  \item keep track of creation and modification times on objects
  \item get fast summaries of basic characteristics of objects - class,
  size, dimension, etc.
}

There is an option to control whether tracked objects are cached in
memory as well as being stored on disk.  By default, objects are cached
in memory for the duration of a top-level task.  To save time when
working with collections of objects that will all fit in memory, turn on
caching with and turn off cache-flushing
\code{\link{track.options}(cache=TRUE, cachePolicy="none")}, or start tracking with
\code{\link{track.start}(..., cache=TRUE, cachePolicy="none")}.  A possible future
improvement is to allow conditional and/or more intelligent caching of
objects.  Some data that would be needed for this is already collected
in access counts and times that are recorded in the tracking summary.

Here is a brief example of tracking some variables in the global environment:

\preformatted{
> library(trackObjs)
> track.start()
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
> track.stop(pos=1)              # Stop tracking
> ls(all=TRUE)
character(0)
>
> # Restart using the tracking dir -- the variables reappear
> track.start() # Start using the same tracking dir again ("rdatadir")
> ls(all=TRUE)
[1] "x"  "y"  "z1" "z2"
> track.summary(size=F)
            class    mode extent length            modified TA TW
x         numeric numeric    [1]      1 2007-09-07 08:50:58  0  1
y          matrix numeric  [3x2]      6 2007-09-07 08:50:58  0  1
z1           list    list  [[3]]      3 2007-09-07 08:50:58  0  1
z2 POSIXt,POSIXct numeric    [1]      1 2007-09-07 08:50:58  0  1
> track.stop(pos=1)
>
> # the files in the tracking directory:
> list.files("rdatadir", all=TRUE)
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
\code{save.image()} or \code{q('yes')}, because \code{track.stop()} will
ensure that tracked variables are saved to disk and then remove them
from the global environment, leaving \code{save.image()} to save only
the untracked or untrackable variables.  The basic functions used in
automatic tracking are as follows:
\itemize{
  \item \code{\link{track.start}(dir=...)}: start tracking
  the global environment, with files saved in \code{dir} (the default is \code{rdatadir}).
  \item \code{\link{track.summary}()}: print a summary of
  the basic characteristics of tracked variables: name, class, extent,
  and creation, modification and access times.
  \item \code{\link{track.info}()}: print a summary of
  which tracking databases are currently active.
  \item \code{\link{track.stop}(pos=, all=)}: stop tracking.
  Any unsaved tracked variables are saved to disk.  Unless
  \code{keepVars=TRUE} is supplied, all tracked variables
  become unavailable until tracking starts again.
  \item \code{\link{track.attach}(dir=..., pos=)}: attach an existing
  tracking database to the search list at the specified position.  The
  default when attaching at positions other than 1 is to use readonly
  mode, but in non-readonly mode, changes to variables in the attached
  environment will be automatically saved to the database.
  \item \code{\link{track.rescan}(pos=)}: rescan a tracking directory
  that was attached by \code{track.attach()} at a position other than 1,
  and that is preferably readonly.
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
  \item \code{\link{track.info}()}: return a data frame
  containing information about which tracking dbs are currently active.
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
\note{
  Some special kinds of objects don't work properly if referenced as
  active bindings and/or stored in a save file.  One example is RODBC
  connections.  To make it easy to work with such objects, two ways of
  excluding variables from automatic tracking are provided: the
  \code{autoTrackExcludePattern} option (a vector regular expressions:
  variables whose name match one of these will not be tracked); and the
  \code{autoTrackExcludeClass} option (a vector of class names:
  variables whose class matches one of these will not be tracked).  New
  values can be added to these options as follows:
  \preformatted{
track.options(autoTrackExcludePattern="regexp")
track.options(autoTrackExcludeClass="classname")
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
\examples{
\dontrun{
library(trackObjs)
# start tracking the global environment using directory 'rdatadir'
# inside dontrun to avoid creating/removing rdatadir
track.start()
a <- 1
b <- 2
ls()
track.status()
track.summary()
track.info()
track.stop()
# variables are now gone
ls()
# bring them back
track.start()
ls()
track.stop()
}
}
\keyword{ package }
\keyword{ data }
\keyword{ database }
\keyword{ utilities }
