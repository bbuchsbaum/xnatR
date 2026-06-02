\name{xnatR_cli}
\alias{xnatR_cli}
\title{Command-line interface for xnatR}
\usage{
xnatR_cli(args = commandArgs(trailingOnly = TRUE))
}
\arguments{
\item{args}{Character vector of command-line arguments.}
}
\value{
Integer status code: \code{0} for success and \code{2} for usage/runtime errors.
}
\description{
Parses xnatR command-line arguments, dispatches to package functions, writes
results to stdout, and returns a process-style integer status code.
}
