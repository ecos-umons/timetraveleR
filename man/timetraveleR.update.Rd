\name{Update}
\alias{Update}
\encoding{UTF-8}
\title{Update}
\description{
  Initialize a previous CRAN state to a newer one.
}

\usage{
  Update(state, old.state)
}
\arguments{
  \item{state}{New state returned by Init}
  \item{old.state}{Older previous state}
}
\details{
  Update \code{state$installed} such that the value is TRUE if the one
  in old.state is TRUE.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{virtualmachine}
\keyword{virtualbox}
\keyword{vagrant}
\keyword{cran}
\keyword{update}
\keyword{state}