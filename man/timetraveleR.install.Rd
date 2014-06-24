\name{Install}
\alias{Install}
\encoding{UTF-8}
\title{Install}
\description{
  Install a CRAN state's packages
}

\usage{
  Install(state, pkg.dir, lib.dir, log.dir=NULL, try.failed=FALSE)
}
\arguments{
  \item{state}{The CRAN state to install like the one returned by Init.}
  \item{pkg.dir}{Directory where packages are stored.}
  \item{lib.dir}{Directory where to install packages.}
  \item{log.dir}{Directory where to write log of failed installation.}
  \item{try.failed}{If FALSE do not try to install packages that failed
    to install previously (i.e. have a file in log.dir)}
}
\details{
  Install packages of a given state and log failures if \code{log.dir}
  is not NULL. Returns the state with column installed updated.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{virtualmachine}
\keyword{virtualbox}
\keyword{vagrant}
\keyword{cran}
\keyword{install}
\keyword{state}