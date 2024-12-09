% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{download_xnat_file}
\alias{download_xnat_file}
\title{Download File from XNAT}
\usage{
download_xnat_file(url, dest_file)
}
\arguments{
\item{url}{The full URL to the file resource you want to download.}

\item{dest_file}{The destination path where the downloaded file will be saved.}
}
\value{
Invisibly returns the response object if successful. Stops execution if the download fails.
}
\description{
Downloads a file from the XNAT server using the authenticated session.
}
\examples{
\dontrun{
download_xnat_file(
  url = "https://your-xnat-server/data/projects/PROJECT_ID/subjects/SUBJECT_ID/experiments/EXPERIMENT_ID/scans/SCAN_ID/resources/RESOURCE_ID/files/FILE_NAME",
  dest_file = "downloaded_file.zip"
)
}
}
