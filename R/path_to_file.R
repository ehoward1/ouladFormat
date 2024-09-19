#' Get file path to the sample_studentVLE.csv file
#'
#' ouladFormat comes bundled with the sample studentVLE data in its `inst/extdata`
#' directory (`sample_studentVLE.csv`). This .csv file is a subset (rows 10,000-14,999) of
#' the original OULAD studentVLE.csv. This function makes it easy to access.
#'
#' @param path Name of file in quotes with extension. If `NULL`, the example file will be listed.
#'
#' @returns No return value, called for side effects.
#' @export
#' @examples
#' path_to_file()
#' read.csv(path_to_file("sample_studentVLE.csv"))
#' @source This function is adapted from `readxl::readxl_example()`.
path_to_file <- function(path = NULL){
  if(is.null(path)){
    dir(system.file("extdata", package = "ouladFormat"))
  }else{
    system.file("extdata", path, package = "ouladFormat", mustWork = TRUE)
  }
}
