% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_VLE_activity.R
\name{dataset_VLE_activity}
\alias{dataset_VLE_activity}
\title{Returns the formatted VLE data set based on activity types}
\usage{
dataset_VLE_activity(
  module = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
  presentation = c("2013B", "2014B", "2013J", "2014J", "All"),
  repeat_students = c("remove", "keep"),
  week_begin = -4,
  week_end = 39,
  example_data = FALSE
)
}
\arguments{
\item{module}{name of the module to be included, either \code{"AAA"}, \code{"BBB"}, \code{"CCC"}, \code{"DDD"}, \code{"EEE"}, \code{"FFF"} or \code{"GGG"}.}

\item{presentation}{name of the semester of the module to be included, either \code{"2013B"},
\code{"2014B"}, \code{"2013J"}, \code{"2014J"}, or \code{"All"}.
\code{"B"} indicates a February start time whereas \code{"J"} indicates an October start time. \code{"All"} indicates that all presentations of the module will be included in the returned data.}

\item{repeat_students}{indicator of whether students who had previous attempts at the module should be removed, either \code{"remove"} or \code{"keep"}.
If presentation is set to \code{"All"}, this is automatically set to \code{"remove"}.}

\item{week_begin}{the first semester week of VLE data to be included in the formatted data. Depending on the module presentation, students
started to view activities up to four weeks prior to the initial module start date. Weeks prior to the initial module start
are indicated by a negative integer.}

\item{week_end}{the last semester week of VLE data to be included in the outputted data.
Week 39 is the last week material was viewed (and earlier in some module presentations).}

\item{example_data}{logical. Indicates whether to run a subset of the data as an example.}
}
\value{
Returns two \code{tibbles} (objects of class \code{tbl_df}) based on the OULAD studentVle.csv and vle.csv files,
the specified inputs (module, presentation, and repeat_students), and
the range of the weeks included in the \code{tibbles}. \code{week_begin} and \code{week_end}
indicates the first and last semester week respectively that is included in
the output \code{tibbles}. These may be different to the corresponding input parameters.
Weeks prior to the initial module start day are indicated by a negative integer.
The two \code{tibbles} returned are: 1) filtered_data, and 2) resource_data.
}
\description{
Load and formats the student Virtual Learning Environment (VLE) data set from the OULAD for data analysis.
}
\section{filtered_data tibble}{

A \code{tibble} based on the combined oulad files of studentVLE.csv and vle.csv,
and the inputs. The \code{tibble} consists of (Kuzilek et al., 2017):

\itemize{
\item{code_module - the module identification code.}
\item{code_presentation - the presentation identification code.}
\item{id_site - the VLE material identification number.}
\item{activity_type - the role associated with the module material.}
\item{week_from - the week from which the material is planned to be used.}
\item{week_to - the week until which the material is planned to be used.}
\item{id_student - the unique student identification number.}
\item{date - the day of student’s interaction with the material (module starts on day 0).}
\item{sum_click - the number of times the student interacted with the material for the date.}
}
}

\section{resource_data tibble}{

A \code{tibble} where each row represents a unique student and each column their number of interactions with
different VLE activity types for the weeks and module presentation inputted.
}

\examples{
# Uses subset of the VLE data set for example
dataset_VLE_activity(example_data = TRUE)
\donttest{
# Slow to run as it loads the full VLE data set
dataset_VLE_activity(module = "BBB", presentation = "2013J",
repeat_students = "remove", week_begin = 1, week_end = 13,
example_data=FALSE)}
}
\references{
Kuzilek, J., Hlosta, M., & Zdrahal, Z. (2017). Open university learning analytics dataset. Scientific Data
volume 4 , (pp. 1–8). https://doi.org/10.1038/sdata.2017.171.
}
\seealso{
\code{\link[=convert_VLE]{convert_VLE()}}, \code{\link[=dataset_VLE_time]{dataset_VLE_time()}}, \code{\link[=VLE_learning_classification]{VLE_learning_classification()}} and \code{\link[=combined_dataset]{combined_dataset()}}
}
