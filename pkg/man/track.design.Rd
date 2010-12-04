\name{track.design}
\alias{track.design}
%- Also NEED an '\alias' for EACH other topic documented here.
\docType{package}
\title{ Design of a tracking environment}
\description{
This document describes the layout of a tracking environment.  Object
tracking works by replacing a variable with an active binding, and
keeping the actual value of the variable on disk and/or in another
environment.  Tracked objects are automatically resaved to disk when
they are changed.  Basic characteristics, such as class, size, extent,
and creation and modification times are recorded in a summary of all
tracked objects.
}
\details{
Object tracking works by replacing a variable with an active binding,
and keeping the actual value of the variable on disk and/or in another
environment.  Whenever the variable is fetched or assigned, the active
binding is called, and it writes the object to disk if necessary, and
records basic characteristics of the objects in a summary of all
objects, including creation, modification and access times.

A tracking environment can be linked to one environment on the search
path, but the tracking environment is not on the search path itself.  An
environment on the search path can only have one tracking environment
linked to it.  In standard use, variables are tracked automatically by a
task callback function.  Alternatively, variables to track can be
registered with the tracking environment using the function
\code{track()}.

Any user-created environment on the search path, or the global
environment, can be tracked.

The format used to store R objects in files is the one used by
\code{save()}/\code{load()} -- the objects in those files can be read
using \code{load()} if desired.

The various variables and files involved in tracking are as follows
(assuming the RData suffix being used is "rda").
Note that the default tracked visible environment is the global environment.

\preformatted{

Tracked Visible Environment
(on search list)          Tracking Environment
                          (not on search list)
+-----------------+   +-------------------------+
|     .trackingEnv|-->|            .trackingDir |---> Tracking Directory (files)
|                 |   |                         |         |
|         x (*)   |   |                   x (@) |         +- x.rda
|       abc (*)   |   |                 abc (@) |         +- abc.rda
|         Y (*)   |   |                   Y (@) |         +- _1.rda
|        x1       |   |                         |         |
|        x2       |   |                         |         |
|                 |   |        .trackingFileMap |         +- filemap.txt
|                 |   |        .trackingSummary |         +- .trackingSummary.rda
|                 |   |        .trackingUnsaved |
|                 |   | .trackingSummaryChanged |
|                 |   |        .trackingOptions |
+-----------------+   +-------------------------+

}
\itemize{
\item variables marked (*) are tracked and are actually an active
binding that refers to the corresponding variable in the tracking
environment.  There can also be untracked variables in the visible
tracked environment, but in the standard mode of operation these are
detected by the end-of-task callback function and are immediately
converted to tracked variables (except for variables with reserved names
like .trackingSummary, and variables matching exclude patterns, see
options \code{autoTrackExcludePattern} and \code{autoTrackExcludeClass}
in \link{track.options}.
\item variables marked (@) may or may not exist -- if they do not exist in
  the tracking environment, they will be automatically read from file
  when the corresponding tracked object is accessed.
\item The "trackingEnv" attribute on the tracked environment is the
tracking environment.  This is implemented as an attribute on the
tracked environment rather than as a variable in the tracked environment
so that \code{save.image()} on the tracked environment will ignore the
tracking environment.  If the tracking environment were stored as a
variable in the tracked environment, \code{save.image()} could end up
storing two copies of every tracked variable: one when it accessed the
active binding (it stores a copy of the object: \code{save()} doesn't
know it's an active binding); and another if the object is cached in the
tracking environment.
\item The "trackingDir" attribute on the tracking environment specifies
the absolute pathname of the directory under which tracked objects are
stored on file.  It uses the absolute pathname because the current
directory of the R session can be changed using \code{setwd()}, which
would result in losing a relative pathname.
\item \code{.trackingFileMap} stores the base part of the file name corresponding to
each tracked object as a named character vector (the names on the
vector are the object names).  Objects that do not have simple names
have an associated file name like "\_NNN" where
"NNN" is a number.  For example, the \code{.trackingFileMap} for the
above configuration could be \code{c(abc="abc", x="x", Y="_1")}.
Simple object names are those conforming to the following rules:
\itemize{
  \item less than 55 characters
  \item are comprised of only lower-case letters, digits 0 through 9,
  "." and "\_"
  \item begin with a lower-case letter
  \item are not one of the following: \code{con}, \code{prn},
  \code{aux}, \code{nul}, \code{com1} through \code{com9}, \code{lpt1}
  through \code{lpt9}, and do not begin with one of these names followed
  by a period (i.e., \code{prn.foo} and \code{prn.foo.bar} are both not
  simple names) (these are special file names under Microsoft Windows - see
  \url{http://en.wikipedia.org/wiki/Filename} and \url{http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/naming_a_file.asp})
}
The object \code{.trackingFileMap} is always kept in memory and is
always saved to disk (as text in the file \code{filemap.txt}) whenever it is
changed.
\item \code{.trackingSummary} is a data frame recording various basic
  characteristics of the tracked objects, such as class, size and
  extent, and also times of creation, and most recent modification and
  access.  The tracking summary should be accessed using the function \code{track.summary()}.
\item \code{.trackingSummaryChanged} A logical flag indicating whether
  or not the tracking summary copy on disk is in sync with version in
  memory.  To reduce overhead on accessing objects, there is an option
  to not resave the tracking summary when it is changed on accessing an
  object -- this variable indicates if it has been changed.
\item \code{.trackingUnsaved}: If the tracking options are set up so
  that objects are not automatically written to files on assignment,
  this variable contains a vector of names of all objects that have not
  been saved.
\item \code{.trackingOptions}: are accessed and changed by the
\code{track.options()} function.  They are kept in memory, and also
written to disk whenever they are changed.
The tracking directory is organized as an R package.  It's layout is as follows (saying, for example, that
\code{attr(trackingEnv, "trackingDir")} is \code{/tmp/trackdir1}, and \code{.trackingFileMap} is \code{c(abc="abc", x="x", Y="_1")}):
}

\preformatted{
/tmp/trackdir1
      |
      +- filemap.txt
      +- .trackingSummary.rda
      +- x.rda
      +- abc.rda
      +- _1.rda
}

}

\section{Terminology}{

One could describe a tracking environment as "attached" to the tracked
environment, but that using that term would risk confusion with the role
of the \code{attach()} function and search path in R.  So, instead the
\code{track} package says that a tracking environment is "linked" to
the tracked environment.

  \describe{
    \item{track:}{The \code{track} \emph{tracks} variables, by
      setting up a one-to-one relationship between R objects and files
      on disks so that when an object in R is modified, the file on disk
      is automatically updated.}
    \item{tracked environment:}{A \emph{tracked} environment contains
      user variables and is usually on the search path.}
    \item{tracked object:}{A \emph{tracked} object (in a tracked
      environment) that has an active binding
      so that when it is modified, the corresponding file on disk is
      also modified.}
    \item{untracked object:}{An \emph{untracked} object in a tracked
      environment is an ordinary object that is not tracked and has no
      corresponding file.}
    \item{tracking environment:}{A \emph{tracking} environment is a
      special environment used by the \code{track} package to track
      objects in the \emph{tracked} environment}
    \item{linked:}{A tracking environment is \emph{linked} to a tracked
      environment (by the \code{trackingEnv} attribute on the tracked
      environment, which \emph{points} to the tracking environment.)}
    \item{start tracking, stop tracking:}{Tracking is \emph{started} by
      creating a tracking environment, linking it to the tracked
      environment, and setting up bindings for tracked objects.}
    \item{tracking database:}{A \emph{tracking} database is the
      collection of files and directories that stores the tracking information.}
    \item{active tracking database:}{A \emph{tracking} database that is
      currently linked to an environment in a running R session.}
    }
}

\section{Untrackable variables -- reserved names}{
  Only ordinary variables can be tracked -- variables that are active
  bindings cannot be tracked.

  Several variable names are reserved and cannot be tracked:
  \code{.trackingEnv}, \code{.trackingFileMap}, \code{.trackingUnsaved},
  \code{.trackingSummary}, \code{.trackingSummaryChanged},
  \code{.trackingOptions}.  Additionally, any variable with a newline
  character ("\\n") as part of its name cannot be tracked (the main
  reason for this is that the mapping from object names to file names is
  stored in a text file, and newline character delimits the name).
}

\section{The file map}{
  The mapping from object names to file names is stored in the file
  \code{fileMap.txt}.  This data is stored as ordinary text file to make
  it easy for users to see the object-file mappings outside of R.
}

\section{Implementation considerations}{

The reason that objects must be explicitly registered for tracking is
that there is currently no way of setting up a function to be called
when a new object is created, so new objects are always created as
ordinary R objects.  Similarly, the R \code{remove()} functions does not
have any hooks, so if \code{remove()} is called on a tracked variable,
it will just remove the active binding in the visible environment, but
will not disturb the underlying tracking environment.  The
\code{track.remove()} function will completely remove a tracked variable
from the visible environment and the underlying tracking environment
(including deleting an associated disk file.)

Object tracking was intended to be used in situations where large
numbers of large objects must be manipulated.  Consequently, there is a
good chance of exhausting resources while using the \code{track}
package.  The \code{track} code tries to check return codes when
creating objects or writing files, and in cases where it is unable to
complete an operation it tries leave the tracking environment in a state
from which objects can be salvaged.  The functions
\code{track.rebuild()} and \code{track.flush()} are provided to
help recover from situations where resource limitations prevented
successful operation.  Note that files are generally written in a
"unsafe" manner (i.e., existing files can be overwritten with partial
new files), but in these cases data is retained in the memory and can be
rewritten after resolving file system problems.

The R functions \code{exists()} should be used with care on tracked
objects, because it will actually fetch the object, possibly needing to
read it from disk.  In the \code{track} code, the \code{exists("x")}
function is not used to check existence of a possibly tracked object
\code{x}, instead an idiom like \code{is.element("x", objects(all=TRUE))}
is used.

These statements about the available facilities in R were true as of
R-2.4.1 (released Dec 2006).

The rules for how variable names are mapped to file names are based on
trying to use filenames that will work properly on all three operating
systems R works on (Linux, Windows, and Mac OS X).  A somewhat obscure
point that must be taken into account is the case-insensitivity of Mac
OS X and Windows.  Even though modern versions of the OS's seem to use
case in their file names, this is because they are case preserving, but
they are in fact still case insensitive.  This means that a file created
with the name "X.rda" is the same file as the "x.rda".
Here is a short shell transcript showing this behavior in
a bash shell running under
Windows and Mac OS X (it's the same in both).
\preformatted{
    $ echo 123 > X
    $ cat x
    123
    $ echo 456 > x
    $ cat x
    456
    $ cat X
    456
  }
Thus, in order to work on OS's, file mapping must be used to create
different filenames for the R objects "x" and "X" (which are in fact
different in R.)

}
\section{Portability}{
  Tracking directories are intended to be operating-system independent
  and completely portable across different operating systems.
}

\section{Compression}{

  Saved R objects are compressed by default in R and by the \code{track}
  package.  Decompression speed is
  very important for interactive response when using track, because each
  time an object is accessed, it is read from its file (unless the
  object is cached).  Of the compression algorithms available as of
  R-2.12.0, which are gzip, bzip2, and xz, gzip is the winner in terms
  of speed.  The default compression level in R for gzip is 6, but level
  1 gives faster compression with slightly larger files (though
  decompression is not faster).  The lzop compression algorithm
  \url{http://www.lzop.org} is still faster but it is not yet available in R.

  Here are some comparisons and benchmarks of various compression
  programs:
  \itemize{
      \item \url{http://www.linuxjournal.com/node/8051/print}
      \item \url{http://tukaani.org/lzma/benchmarks.html}
      \item \url{http://stephane.lesimple.fr/wiki/blog/lzop_vs_compress_vs_gzip_vs_bzip2_vs_lzma_vs_lzma2-xz_benchmark_reloaded}
      \item \url{http://aliver.wordpress.com/2010/06/22/huge-unix-file-compresser-shootout-with-tons-of-datagraphs}
      \item \url{http://www.maximumcompression.com/index.html}
      \item \url{http://mattmahoney.net/dc/text.html}
    }

    Compression/decompression is nicely handled in R: only the call to
    \code{save()} has arguments for compression.  Decompression in
    \code{load()} is handled automatically using a standard code (magic)
    at the start of the saved file.  Saved files can also be compressed
    or decompressed outside of R, and \code{load()} will still handle
    them correctly, provided the compression used is one of the types
    that R knows about.

  }

\references{
Roger D. Peng. Interacting with data using the filehash package. R
News, 6(4):19-24, October
2006. \url{http://cran.r-project.org/doc/Rnews} and
\url{http://sandybox.typepad.com/software}

David E. Brahm. Delayed data packages. R News, 2(3):11-12, December
2002.  \url{http://cran.r-project.org/doc/Rnews}
}

\author{Tony Plate <tplate@acm.org>}

\seealso{
\link[=track-package]{Overview} of the \code{track} package.

Documentation for \code{\link[base]{makeActiveBinding}} and related
functions (in 'base' package).

Inspriation from the packages \code{\link[g.data:g.data.save]{g.data}} and
\code{\link[filehash:dbLoad]{filehash}}.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
