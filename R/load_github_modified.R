#' A modified version of the Rfssa load_github_data() function
#'
#' Loads .RData files directly from a GitHub repository.
#' The original Rfssa function (Haghbin et al., 2022) is
#' based on the Stack Overflow post (Hartman, 2016):
#' https://stackoverflow.com/questions/24846120/importing-data-into-r-rdata-from-github,
#' and loads the .RData to the global environment. This function
#' allows the user to choose the environment which the
#' .RData files are loaded to.
#'
#' @param github_data_url The GitHub url of the .RData data set to be loaded.
#' @param env Environment where the data is to be loaded into.
#'
#' @return
#' Loads the .RData into the local environment.
#'
#' @export
#' @importFrom httr "GET" "content" "stop_for_status" "timeout"
#'
#' @references Haghbin, H., Trinka, J., Najibi, S., M., & Maadooliat, M. (2022).
#' Package 'Rfssa'. https://cran.r-project.org/web/packages/Rfssa/Rfssa.pdf.
#'
#' Hartman, R. (2016). Importing data into R (.rData) from Github.
#' https://stackoverflow.com/questions/24846120/importing-data-into-r-rdata-from-github.
#'
#' @examples
#' \donttest{
#' env = environment()
#' load_github_modified("https://github.com/ehoward1/oulad_data/blob/main/vle.RData", env)}
load_github_modified = function(github_data_url, env)
{
  url_len <- nchar(github_data_url)
  if (substr(github_data_url, start = (url_len - 8), stop = url_len) != "?raw=true")
    github_data_url <- paste0(github_data_url, "?raw=true")
    temp_file <- tempfile()
    on.exit(unlink(temp_file))
    request <- httr::GET(github_data_url, httr::timeout(30))
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)
    load(temp_file, envir = env)
}
