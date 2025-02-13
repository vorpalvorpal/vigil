% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kill.R
\name{kill_processes}
\alias{kill_processes}
\title{Kill processes with escalating force}
\usage{
kill_processes(pids, timeout = 5)
}
\arguments{
\item{pids}{Vector of process IDs to kill}

\item{timeout}{Timeout in seconds}
}
\value{
Logical indicating if all processes were killed
}
\description{
Kill processes with escalating force
}
\keyword{internal}
