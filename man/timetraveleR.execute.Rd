\name{Execute}
\alias{Execute}
\encoding{UTF-8}
\title{Execute}
\description{
  Execute a script on all installed packages.
}

\usage{
  Execute(state, lib.dir, script, dest, ..., log.dir=NULL)
}
\arguments{
  \item{state}{The CRAN state to install like the one returned by Init.}
  \item{lib.dir}{Directory where to install packages.}
  \item{script}{The path of the script to execute}
  \item{dest}{Directory where to write result output.}
  \item{...}{Additional arguments to pass to the script.}
  \item{log.dir}{Directory where to write log of failed installation.}
}
\details{
  Run a (R) script on each package installed and log failures if
  \code{log.dir} is not NULL. The script must take the following
  parameters: the name of the package, the name of the package, the
  version of the package, the directory where packages are installed,
  where to store the result and additional arguments.

  If the script is meant to be run only one for each successfully
  installed package (i.e. should not be run a second time on the same
  package version with a different CRAN state), it should create a file
  in \code{dest} named package_version.rds.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{virtualmachine}
\keyword{virtualbox}
\keyword{vagrant}
\keyword{cran}
\keyword{execute}
\keyword{state}