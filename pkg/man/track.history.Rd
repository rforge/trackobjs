\name{track.history}
\alias{track.history}
\alias{track.history.start}
\alias{track.history.stop}
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
track.history.start(file = NULL, width = NULL, style = NULL, times = NULL)
track.history.stop()
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
    \item \code{fast}: use deparsed version of
  most recently executed command.  This is fast, but it doesn't record
  comments and some commands with errors, and it changes formatting.
  \item \code{full}: write history to a file and read back commands to
  find the recent ones.  This is slower, but it records everything typed
  at the command prompt, in the original formatting.}
}
\item{times}{
  Should time stamps be written to or read from the file?  The default
  behavior for \code{track.history.start()} is \code{TRUE}.
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

  To read in history for a different file, use \code{loadhistory(file)}.

  The operation of history mechanism with \code{style="full"}
  unavoidably results in timestamps being inserted into the interactive
  history.  These are needed to identify how much of the history has
  been written to the incremental history file so far.  There may be
  ways to avoid their use - patches are welcome.

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
  These functions currently provide no useful return values.
}
\author{
  Tony Plate \email{tplate@acm.org}
}
\seealso{
\code{\link{addTaskCallback}}
\code{\link{loadhistory}}
\code{\link{savehistory}}
}
\examples{
\dontrun{
## Can't use history except in Rgui and Rterm
track.history.start(style="full")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
