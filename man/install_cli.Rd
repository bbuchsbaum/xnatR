\name{install_cli}
\alias{install_cli}
\title{Install the xnatR command-line wrapper}
\usage{
install_cli(dest_dir = "~/.local/bin", overwrite = FALSE, commands = NULL)
}
\arguments{
\item{dest_dir}{Destination directory for command wrappers.}

\item{overwrite}{If \code{TRUE}, replace an existing command wrapper.}

\item{commands}{Optional command names to install. Currently only \code{"xnatR"}
is supported.}
}
\value{
Invisibly returns the installed wrapper path.
}
\description{
Copies the packaged \code{xnatR} command wrapper to a directory on your shell
\code{PATH}, or to another destination directory you choose.
}
