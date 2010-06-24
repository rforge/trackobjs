\name{track.future}
\alias{track.future}
\docType{package}
\title{
Potential future features of the trackObjs package
}
\description{

Potential future features of the trackObjs package, in some vague order
of feasibility and priority ('easy', 'medium' and 'hard' are an estimate
of design and coding difficulty):

\describe{
  \item{untracked variables in the summary:}{(easy) would this be useful?
    wouldn't need to cache these, mark with an asterisk in a special
    column?}
  \item{other default tracked environment:}{(easy) would it be useful to allow
    an environment other than the global environment to be the
    default tracking environment?  This could be implemented by using
    \code{options("tracked.environment")} as the default environment for all the
    tracking functions (rather than the currently hardcoded pos=1)}
  \item{better cleanup:}{(easy) provide an integrated quiting function that
    saves all tracked vars and history before quitting (and maybe also
    saves untracked vars in an RData file)}
  \item{caching rules:}{(hard) allow rule-based decisions for caching, e.g.,
    only cache objects under a certain size, or only cache objects of
    certain classes, or enforce a limit on memory for caching tracked
    variables, and flush out least-recently used variables}
  \item{auto-trust in rebuild:}{(easy) when rebuilding an active tracking
    environment, base decision whether to use summary row from file or
    environment on which has more recent dates in it. (whole dataframe,
    or row by row?)}
  \item{smarter reading of filemap.txt:}{(medium) check the mod time on
    filemap.txt when getting the filemap obj, and if the file on disk
    appears to have changed, reread it instead of just getting it from
    memory.  This would allow working together better with other
    sessions that are simultaneously using this tracking dir.  Don't know how much
    it would slow things down -- do some timings.  Note that to make
    this work in a fool-proof manner would require locks.}
  \item{investigate double-get:}{ doing subset-replacement (e.g.,
    \code{X[2] <- ...}) retrieves \code{X} twice
    (see example below)}
  \item{readonly mode:}{(hard) to allow linking tracking dirs that might be
    in use by other R processes -- would require not recording
    gets -- this would require adding a new env on the
    search path and tracking it}
  \item{autoflush:}{(hard) automatic flushing of variables that haven't been used
    frequently (triggered automaticall when memory runs low?) -- this
    is why the summary records fetches as well as writes}
  \item{safer restart:}{(medium) check that we will be able to restart before
    doing the stop (check for masked variables or other potential clobber problems)}
  \item{safe saves:}{(hard) write files in a safe way so that the original
    file is not removed until the new file is written -- not sure if
    this is necessary, because objects are in memory, and can be
    rewritten if there is a failure}
  \item{autotrack:}{(hard) automatically track new variables? (would require hooks in
    base-R that get called when a new var is created)}
}

Example of the "double-get" when assigning a subset (using the example
from the help page for \code{makeActiveBinding}).  Note that it works
correctly, but retrieving the object twice seems unneccessary and could
be slow with very large objects.

\preformatted{
> f <- local( {
+     x <- 1
+     function(v) {
+        if (missing(v))
+            cat("get\n")
+        else {
+            cat("set\n")
+            x <<- v
+        }
+        x
+     }
+ })
> makeActiveBinding("X", f, .GlobalEnv)
NULL
> bindingIsActive("X", .GlobalEnv)
[1] TRUE
> X
get
[1] 1
> X <- 2
set
> X
get
[1] 2
> 
> X[1]
get
[1] 2
> X[2] <- 1 # 'X' is fetched twice
get
get
set
> X
get
[1] 2 1
>
}
}
\examples{
# Example (transcript shown above) of how subset-assignment
# results in two retrievals when the object is an active binding.
f <- local( {
    x <- 1
    function(v) {
       if (missing(v)) {
           cat("get\n")
       } else {
           cat("set\n")
           x <<- v
       }
       x
    }
})
makeActiveBinding("X", f, .GlobalEnv)
bindingIsActive("X", .GlobalEnv)
X
X <- 2
X
X[1]
X[2] <- 1 # 'X' is fetched twice
X
}
\seealso{ \link[=trackObjs-package]{Overview} and \link[=track.design]{design} of the \code{trackObjs} package. }
\keyword{ data }
