\name{show.envs}
\alias{show.envs}
\title{
  Show the environments referenced within an object.
}
\description{
  \code{show.envs} recursively examines \code{x} and the objects within
it, printing the names of any environments encountered.  It does NOT
recursively enter environments -- once it finds an environment it just
prints the name of that environment and doesn't look further inside the
environment.
}
\usage{
show.envs(x, obj = substitute(x))
}
\arguments{
  \item{x}{The object to examine.
}
  \item{obj}{An expression describing the object.  Not intended to be
    supplied by the user.
}
}
\details{
  \code{show.envs} attemps to show the environments referenced within an
  object, but it may miss some.  If you encounter any such cases, please
  email them to \email{tplate@acm.org}.
}
\value{
  The number of environments encountered.
}
\author{Tony Plate \email{tplate@acm.org}.}

\examples{
x <- data.frame(a=1:10, b=10:1)
m <- lm(a ~ b, data=x)
show.envs(m)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
