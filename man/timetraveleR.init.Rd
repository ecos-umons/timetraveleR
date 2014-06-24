\name{Init}
\alias{Init}
\encoding{UTF-8}
\title{Init}
\description{
  Initialize the state of CRAN for a given date.
}

\usage{
  Init(packages, deps, date, deps.types=c("depends", "imports", "linkingto"))
}
\arguments{
  \item{packages}{Dataframe containing all CRAN packages with three
    columns: package, version and mtime (containing release time of the
    package).}
  \item{deps}{Datafrme containing all CRAN dependencies with columns
    package, version, type and dependency.}
  \item{date}{Date of the state to create}
  \item{deps.types}{Dependency types to use.}
}
\details{
  Returns a dataframe representing the state with columns package,
  version, mtime, missing.deps, installed and loaded. The two last
  columns are booleans telling if the package has missing dependencies,
  is installed on the VM and can be loaded. Column install is set to
  FALSE. Rows are sorted by (dependency) topological order so they can
  be installed in the order they appear in the dataframe.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{virtualmachine}
\keyword{virtualbox}
\keyword{vagrant}
\keyword{cran}
\keyword{init}
\keyword{state}