\name{track.history}
\alias{track.history}
\alias{track.history.start}
\alias{track.history.stop}
\alias{track.history.status}
\alias{track.history.load}
\alias{track.history.writer}

\title{
  Functions for incrementally writing command history to a file.
}
\description{
  These functions provide the ability to append recent commands to a
  history file after (almost) every top level command.  This makes it
  unnecessary to use \code{savehistory()} and solves the problem of
  command history not being saved on accidental or abnormal termination
  of an R session.
}
\usage{
track.history.start(file = NULL, width = NULL, style = NULL, times =
  NULL, load = TRUE, verbose = FALSE, message="Session start")
track.history.stop()
track.history.status()
track.history.load(times = FALSE)
track.history.writer(expr, value, ok, visible)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
    File to store the incremental history in.  Defaults to \code{.Rincr_history}.
}
\item{width}{
  Width to use deparsing for fast-style history saving.  Defaults to \code{120}.
}
\item{style}{
  Two styles are possible: \itemize{
  \item \code{full}: (default) Use the internal R history mechanism.  This is slower, but it records everything typed
  at the command prompt, in the original formatting.
  \item \code{fast}: Use deparsed version of the
  most recently executed command.  This is fast, but it doesn't record
  comments and some commands with errors, and it changes formatting.
  }
}
\item{times}{
  Should time stamps be written to or read from the file?  The default
  behavior for \code{track.history.start()} is \code{TRUE}.  The default
  when loading history is \code{FALSE}, so that time stamps do not
  appear in the interactive history.
}
\item{load}{
  Should existing history be loaded when starting incremental history?
}
\item{verbose}{
  Should comments be printed?
}
\item{expr}{
  Provided by the task callback
}
\item{value}{
  Provided by the task callback
}
\item{ok}{
  Provided by the task callback
}
\item{visible}{
  Provided by the task callback
}
\item{message}{
    A string to be written (once) to the incremental history file along with
  the date and time when incremental history tracking is started.}
}
\details{
  Default values are taken first from options \code{incr.hist.style},
  \code{incr.hist.width},  \code{incr.hist.file} and
  \code{incr.hist.times}.
  If those option values don't exist, values are taken from environment
  variables \code{R_INCR_HIST_STYLE},  \code{R_INCR_HIST_WIDTH},
  \code{R_INCR_HIST_FILE},  \code{R_INCR_HIST_TIMES}.
  \itemize{
    \item \code{track.history.start()} installs \code{track.history.writer()}
  as a task callback handler.
    \item \code{track.history.load()} loads history from the file that
  incremental history is being written to.
    \item \code{track.history.stop()} removes the task callback handler.
    \item \code{track.history.writer()} is the task callback handler -- it is
    not intended to be called by the user.
  }
  If arguments are supplied to \code{track.history.start()}, their
  values are remembered in \code{options()} and used for the remainder
  of the session or until changed.


The history stored using \code{style="full"} is more complete and
accurate, in that it includes comments, unparseable commands, and
original formatting.  It is somewhat slower because it is based
on the internal history mechanism, which doesn't provide an easy way of
identifying which are the new commands.  Consequently, when using
\code{style="full"} \code{track.history.writer()} must inspect the
entire internal history at the end of each command to work out which
lines in it have been added since the last time history was written.
However, the time difference seems negligible for interactive use on
ordinary workstations circa 2010.

  To set up incremental history tracking automatically, put the
  following in your \code{.Rprofile}:
  \preformatted{
if (interactive()) {
    track.history.load()
    track.history.start()
}
    }

}
\value{
  \code{track.history.status()} returns a character string: \code{"on"}
  or \code{"off"}.
  The other functions currently provide no useful return values.
}
\author{
  Tony Plate \email{tplate@acm.org}
}
\seealso{
\code{\link{addTaskCallback}}
To read in command history that is stored in a particular file, use
\code{\link{loadhistory}(file)}.
\code{\link{savehistory}}
}
\examples{
\dontrun{
## Can't use history except in Rgui and Rterm
track.history.start()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
