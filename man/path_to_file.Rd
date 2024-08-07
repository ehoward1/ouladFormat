% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/path_to_file.R
\name{path_to_file}
\alias{path_to_file}
\title{Get file path to the sample_studentVLE.csv file}
\source{
This function is adapted from `readxl::readxl_example()`.
}
\usage{
path_to_file(path = NULL)
}
\arguments{
\item{path}{Name of file in quotes with extension. If `NULL`, the example file will be listed.}
}
\description{
ouladFormat comes bundled with the sample studentVLE data in its `inst/extdata`
directory (`sample_studentVle.csv`). This .csv file is a subset (rows 10,000-14,999) of
the original OULAD studentVle.csv. This function makes it easy to access.
}
\examples{
path_to_file()
read.csv(path_to_file("sample_studentVLE.csv"))
}
